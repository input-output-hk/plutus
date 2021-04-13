{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# OPTIONS_GHC -fno-warn-orphans               #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module PlutusCore.Examples.Builtins where

import           PlutusCore
import           PlutusCore.Constant
import           PlutusCore.Evaluation.Machine.ExBudget
import           PlutusCore.Evaluation.Machine.ExBudgetingDefaults
import           PlutusCore.Evaluation.Machine.ExMemory
import           PlutusCore.Evaluation.Machine.Exception
import           PlutusCore.Evaluation.Result
import           PlutusCore.MkPlc                                  (mkTyBuiltin)
import           PlutusCore.Pretty
import           PlutusCore.Universe

import qualified PlutusCore.StdLib.Data.List                       as Plc

import           Control.Monad.Error
import           Data.Char
import           Data.Either
import           Data.Hashable                                     (Hashable)
import qualified Data.Kind                                         as GHC (Type)
import           Data.Proxy
import           Data.String
import           Data.Text.Prettyprint.Doc
import           Data.Void
import           GHC.Generics
import           GHC.Ix

instance (Bounded a, Bounded b) => Bounded (Either a b) where
    minBound = Left  minBound
    maxBound = Right maxBound

size :: forall a. (Bounded a, Enum a) => Int
size = fromEnum (maxBound :: a) - fromEnum (minBound :: a) + 1

-- >>> map fromEnum [Left False .. Right GT]
-- [0,1,2,3,4]
-- >>> map toEnum [0 .. 4] :: [Either Bool Ordering]
-- [Left False,Left True,Right LT,Right EQ,Right GT]
instance (Eq a, Eq b, Bounded a, Bounded b, Enum a, Enum b) => Enum (Either a b) where
    succ (Left x)
        | x == maxBound = Right minBound
        | otherwise     = Left $ succ x
    succ (Right y)
        | y == maxBound = error "Out of bounds"
        | otherwise     = Right $ succ y

    pred (Left x)
        | x == minBound = error "Out of bounds"
        | otherwise     = Left $ pred x
    pred (Right y)
        | y == minBound = Left maxBound
        | otherwise     = Right $ pred y

    toEnum i
        | i < s     = Left  $ toEnum i
        | otherwise = Right $ toEnum (i - s)
        where s = size @a

    fromEnum (Left  x) = fromEnum x
    fromEnum (Right y) = size @a + fromEnum y

-- >>> import GHC.Ix
-- >>> map (unsafeIndex (Left False, Right GT)) [Left False .. Right GT]
-- [0,1,2,3,4]
-- >>> let bounds = (Left (False, EQ), Right (True, GT)) in map (unsafeIndex bounds) $ range bounds
-- [0,1,2,3,4,5,6,7,8,9]
instance (Bounded a, Bounded b, Ix a, Ix b) => Ix (Either a b) where
    range (Right _, Left  _) = []
    range (Right x, Right y) = map Right (range (x, y))
    range (Left  x, Right y) = map Left (range (x, maxBound)) ++ map Right (range (minBound, y))
    range (Left  x, Left  y) = map Left (range (x, y))

    unsafeIndex (Right _, _) (Left  _) = error "Out of bounds"
    unsafeIndex (Left  x, n) (Left  i) = unsafeIndex (x, fromLeft maxBound n) i
    unsafeIndex (Right x, n) (Right i) = unsafeIndex (x, fromRight (error "Out of bounds") n) i
    unsafeIndex (Left  x, n) (Right i) =
        unsafeIndex (x, maxBound) maxBound + 1 +
            unsafeIndex (minBound, fromRight (error "Out of bounds") n) i

    inRange (m, n) i = m <= i && i <= n

data ExtensionFun
    = Factorial
    | Const
    | Id
    | IdFInteger
    | IdList
    | IdRank2
    | Absurd
    | CharToInteger
    | ReplicateAtChar
--     | Null
--     | Head
--     | Tail
--     | Swap
    deriving (Show, Eq, Ord, Enum, Bounded, Ix, Generic, Hashable)
    deriving (ExMemoryUsage) via (GenericExMemoryUsage ExtensionFun)

instance Pretty ExtensionFun where pretty = viaShow

instance (ToBuiltinMeaning uni fun1, ToBuiltinMeaning uni fun2) =>
            ToBuiltinMeaning uni (Either fun1 fun2) where
    type CostingPart uni (Either fun1 fun2) = (CostingPart uni fun1, CostingPart uni fun2)

    toBuiltinMeaning (Left  fun) = case toBuiltinMeaning fun of
        BuiltinMeaning sch toF toExF -> BuiltinMeaning sch toF (toExF . fst)
    toBuiltinMeaning (Right fun) = case toBuiltinMeaning fun of
        BuiltinMeaning sch toF toExF -> BuiltinMeaning sch toF (toExF . snd)

defBuiltinsRuntimeExt
    :: HasConstantIn DefaultUni term
    => BuiltinsRuntime (Either DefaultFun ExtensionFun) term
defBuiltinsRuntimeExt = toBuiltinsRuntime (defaultCostModel, ())

data PlcListRep (a :: GHC.Type)
instance KnownTypeAst uni a => KnownTypeAst uni (PlcListRep a) where
    toTypeAst _ = TyApp () Plc.listTy . toTypeAst $ Proxy @a
type instance ToBinds (PlcListRep a) = ToBinds a

instance KnownTypeAst uni Void where
    toTypeAst _ = runQuote $ do
        a <- freshTyName "a"
        pure $ TyForall () a (Type ()) $ TyVar () a
instance KnownType term Void where
    makeKnown = absurd
    readKnown = throwingWithCause _UnliftingError "Can't unlift a 'Void'" . Just
type instance ToBinds Void = '[]

-- Every polymorphic function ignores the memory annotation of its argument. This is due to the fact
-- that no function duplicates the AST and so threading an argument through a function or dropping
-- it completely does not increase memory consumption. It does not seem to be possible to define
-- a function that duplicates the AST of its argument with the current set of built-in types,
-- but once we have lists in the universe, that should be easy to do. Once there's a duplicating
-- function, should we multiple memory consumption by the number of duplicates it returns minus one?
-- For example, should @\x -> [x, x]@ increase memory consumption by the memory annotation of @x@
-- just once and not twice, since one of the @x@s is going to be essentially reused and the other
-- one will be a true duplicate? Or should memory consumption stay the same regardless of the
-- number of duplicates (modulo whatever the spine of the freshly list takes up)? Since
--
-- 1. later we can call @head@ over the resulting list thus not duplicating anything in the end
-- 2. any monomorphic builtin forcing a 'Constant' node of the duplicated AST will automatically
--    add it to the current budget. And if we never touch the duplicate again and just keep it
--    around, then it won't ever increase memory consumption. And any other node will be taken into
--    account automatically as well: just think that having @\x -> f x x@ as a PLC term is supposed
--    to be handled correctly by design
instance uni ~ DefaultUni => ToBuiltinMeaning uni ExtensionFun where
    type CostingPart uni ExtensionFun = ()
    toBuiltinMeaning :: forall term. HasConstantIn uni term => ExtensionFun -> BuiltinMeaning term ()
    toBuiltinMeaning Factorial =
        makeBuiltinMeaning
            (\(n :: Integer) -> product [1..n])
            mempty  -- Whatever.
    toBuiltinMeaning Const =
        makeBuiltinMeaning
            const
            (\_ _ _ -> ExBudget 1 0)
    toBuiltinMeaning Id =
        makeBuiltinMeaning
            Prelude.id
            (\_ _ -> ExBudget 1 0)
    toBuiltinMeaning IdFInteger =
        makeBuiltinMeaning
            (Prelude.id
                :: a ~ Opaque term (TyAppRep (TyVarRep ('TyNameRep "f" 0)) Integer)
                => a -> a)
            (\_ _ -> ExBudget 1 0)
    toBuiltinMeaning IdList =
        makeBuiltinMeaning
            (Prelude.id
                :: a ~ Opaque term (PlcListRep (TyVarRep ('TyNameRep "a" 0)))
                => a -> a)
            (\_ _ -> ExBudget 1 0)
    toBuiltinMeaning IdRank2 =
        makeBuiltinMeaning
            (Prelude.id
                :: ( f ~ 'TyNameRep "f" 0
                   , a ~ 'TyNameRep @GHC.Type "a" 1
                   , afa ~ Opaque term (TyForallRep a (TyAppRep (TyVarRep f) (TyVarRep a)))
                   )
                => afa -> afa)
            (\_ _ -> ExBudget 1 0)
    toBuiltinMeaning Absurd =
        makeBuiltinMeaning
            (absurd
                :: a ~ Opaque term (TyVarRep ('TyNameRep "a" 0))
                => Void -> a)
            (\_ _ -> ExBudget 1 0)
    toBuiltinMeaning CharToInteger =
        makeBuiltinMeaning
            (toInteger . ord)
            mempty  -- Whatever.
    toBuiltinMeaning ReplicateAtChar =
        makeBuiltinMeaning
            (replicate :: Int -> Char -> [Char])
            mempty  -- Whatever.
    -- toBuiltinMeaning Null = makeBuiltinMeaning nullPlc mempty where
    --     nullPlc
    --         :: a ~ TyVarRep ('TyNameRep "a" 0)
    --         => SomeValueN DefaultUni [] '[a] -> Bool
    --     nullPlc (SomeValueArg uniA (SomeValueRes _ xs)) = null xs

--         makeBuiltinMeaning
--             _
--             mempty where
        -- nullPlc :: Opaque term (TyAppRep [] (TyVarRep ('TyNameRep "a" 0))) -> KnownTypeMonad term Bool
        -- nullPlc (Opaque term) = case asConstant term of
        --     Just (Some (ValueOf uni xs)) -> case uni of
        --         DefaultUniList _ -> pure $ null xs
        --         _ -> do
        --             let err = fromString $ "Expected a list, but got " ++ gshow uni
        --             KnownTypeMonad $ throwingWithCause _UnliftingError err $ Just term
        --     Nothing -> KnownTypeMonad $ throwingWithCause _UnliftingError "Not a constant" $ Just term
    -- toBuiltinMeaning Head = makeBuiltinMeaning headPlc mempty where
    --     headPlc
    --         :: a ~ TyVarRep ('TyNameRep "a" 0)
    --         => SomeValueN DefaultUni [] '[a] -> EvaluationResult (Opaque term a)
    --     headPlc (SomeValueArg uniA (SomeValueRes _ xs)) = case xs of
    --         x : _ -> pure . Opaque . fromConstant . Some $ ValueOf uniA x
    --         _     -> throwError evaluationFailure

        -- (Opaque term) = case asConstant term of
        --     Just (Some (ValueOf uni xs)) -> case uni of
        --         DefaultUniList a -> case xs of
        --             x:_ -> pure . Opaque . fromConstant . Some $ ValueOf a x
        --             []  -> KnownTypeMonad $ throwError evaluationFailure
        --         _ -> do
        --             let err = fromString $ "Expected a list, but got " ++ gshow uni
        --             KnownTypeMonad $ throwingWithCause _UnliftingError err $ Just term
        --     Nothing -> KnownTypeMonad $ throwingWithCause _UnliftingError "Not a constant" $ Just term
    -- toBuiltinMeaning Tail = makeBuiltinMeaning tailPlc mempty where
    --     tailPlc
    --         :: a ~ TyVarRep ('TyNameRep "a" 0)
    --         => SomeValueN DefaultUni [] '[a] -> EvaluationResult (SomeValueN DefaultUni [] '[a])
    --     tailPlc (SomeValueArg uniA (SomeValueRes uniListA xs)) = case xs of
    --         _ : xs' -> pure . SomeValueArg uniA $ SomeValueRes uniListA xs'

            -- xs -- pure . Opaque . fromConstant . Some $ ValueOf uniA x
            -- _       -> throwError evaluationFailure


        -- tailPlc
        --     :: a ~ TyVarRep ('TyNameRep "a" 0)
        --     => SomeValueN DefaultUni [] '[a] -> EvaluationResult (Opaque term [a])
        -- tailPlc (SomeValueArg uniA (SomeValueRes _ xs)) = case xs of
        --     _ : xs -> _ xs -- pure . Opaque . fromConstant . Some $ ValueOf uniA x
        --     _      -> throwError evaluationFailure

--         makeBuiltinMeaning
--             (\case [] -> EvaluationFailure; _:xs -> pure (xs :: [Integer]))
--             mempty  -- Whatever.
    -- toBuiltinMeaning Swap =
    --     BuiltinMeaning _ _ _ where
    --         -- sch =
    --         --     TypeSchemeAll (Proxy @'("a", 0, *)) $ \a ->
    --         --     TypeSchemeAll (Proxy @'("b", 0, *)) $ \b ->


data SomeValueOf1 uni f = forall a. SomeValueOf1 (uni a) (f a)
data SomeValueOf2 uni f = forall a b. SomeValueOf2 (uni a) (uni b) (f a b)

data ValueOf1 uni f a = ValueOf1 (uni a) (f a)
-- -- data ValueOf2 uni f a b = ValueOf2 (uni a) (uni b) (f a b)
-- type ValueOf2 uni f a b = ValueOf2 (uni a) (ValueOf1 uni (f a) b)


-- instance HasConstantIn DefaultUni => KnownType term

-- data Foo (uni :: forall k. k -> *) = Foo

-- uni :: forall k. k -> *
-- uni :: newtyp

-- unliftConstantList
--     :: forall a m term err.
--        (MonadError (ErrorWithCause err term) m, AsUnliftingError err, HasConstantIn DefaultUni term)
--     => term -> m (Some (ValueOf1 DefaultUni []))
-- unliftConstantList term = case asConstant term of
--     Just (Some (ValueOf uni xs)) -> case uni of
--         DefaultUniRunTypeApp (DefaultUniList `DefaultUniApply` el) -> pure . Some $ ValueOf1 el xs
--         _                 -> do
--             let err = fromString $ "Expected a list, but got " ++ gshow uni
--             throwingWithCause _UnliftingError err $ Just term
--     Nothing -> throwingWithCause _UnliftingError "Not a constant" $ Just term

-- f : all a. [_] a -> bool

-- instance (uni `Includes` [], uni `Includes` Hole) => KnownTypeAst uni [] where
--     toTypeAst _ = mkTyBuiltin @[Hole] ()

-- null :: Opaque term [Meta] -> Bool
-- null : [meta] -> bool

-- null :: Opaque term [TyVarRep ('TyNameRep "a" 0)] -> Bool

-- instance uni `Includes` [a]           => KnownTypeAst uni (Mono [a]) where
--     toTypeAst = toBuiltinTypeAst

-- instance KnownTypeAst           => KnownTypeAst uni (Poly [a]) where
--     toTypeAst = toBuiltinTypeAst

-- instance KnownBuiltinType term [a]           => KnownType term (Mono [a])



-- data TypeScheme term (args :: [GHC.Type]) res where
--     TypeSchemeResult
--         :: KnownType term res
--         => Proxy res -> TypeScheme term '[] res
--     TypeSchemeArrow
--         :: KnownType term arg
--         => Proxy arg -> TypeScheme term args res -> TypeScheme term (arg ': args) res
--     TypeSchemeAll


-- swap : all a b. (a, b) -> (b, a)

-- instance DefaultUni `Includes` Opaque term' rep

-- Some (Compose (ValueOf uni) [])


-- null :: Some (ValueOf1 uni []) -> Bool

-- null : all a. [a] -> bool

-- instance Convert [Integer] [Whatever]

-- makeKnown :: a -> m term
-- readKnown :: term -> m a


-- null :: uni `Includes` a => [a] -> Bool
-- null :: [Any] -> Bool

-- replicate : integer -> meta -> [meta]
-- replicate : metaAll a. integer -> a -> [a]
-- replicate : all a. integer -> a -> [a]
-- null : all a. [a] -> bool

-- null {integer} [1,2,3]

-- tuples with general terms in them are not representable, hence @swap@ over a tuple with general
-- terms in it is essentially @absurd@
