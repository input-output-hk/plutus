{-| Description: PLC Syntax, typechecker,semantics property based testing.

This file contains
1. A duplicate of the Plutus Core Abstract Syntax (types and terms)
2. A kind checker and a type checker
3. Reduction semantics for types
-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

module PlutusCore.Generators.NEAT.Term
  ( TypeBuiltinG (..)
  , TypeG (..)
  , ClosedTypeG
  , convertClosedType
  , TermG (..)
  , ClosedTermG
  , convertClosedTerm
  , Check (..)
  , stepTypeG
  , normalizeTypeG
  , GenError (..)
  , Neutral (..)
  ) where

import           Control.Enumerable
import           Control.Monad.Except
import           Data.Bifunctor.TH
import           Data.ByteString                   (ByteString, pack)
import           Data.Coolean                      (Cool, false, toCool, true, (&&&))
import qualified Data.Map                          as Map
import qualified Data.Stream                       as Stream
import qualified Data.Text                         as Text
import           PlutusCore
import           PlutusCore.Generators.NEAT.Common
import           Text.Printf

import           PlutusCore.Generators.NEAT.Type

{-

NOTE: We don't just want to enumerate arbitrary types but also normal
      types that cannot reduce any further. Neutral types are a subset
      of normal forms that consist of either a variable or a stuck
      application.

      Two approaches spring to mind:

      1. We could define a seperate AST for normal types and possibly
      also a seperate AST for terms with normalized types. This would
      be the safest option as it would then be impossible to generate
      a non-normalized type that claimed to be normalized. This is how
      it's done in the metatheory package. The downside is that there
      there is some duplication of code and/or it's a bit cumbersome and
      slow to convert from a normalized type back to an ordinary one.

     2. We could use a simple newtype wrapper to mark a type as
     normalized/neutral. This is how it's done in the plutus-core
     package. It's a helpful cue but there's nothing stopping us
     marking any type as normalised. This saves some extra work as we
     can conveniently unwrap a normal form and reuse code such as
     substitution.

     Here we go with option 2. The enumerable instances below explain
     how to construct a normalized or neutral type using the machinery of
     Control.Enumerable from the sized-based package.
-}

instance Enumerable tyname => Enumerable (Normalized (TypeG tyname)) where
  enumerate = share $ aconcat
    [       c1 $ \ty -> Normalized (unNeutral ty)
    , pay . c1 $ \ty -> Normalized (TyLamG (unNormalized ty))
    , pay . c3 $ \ty1 k ty2 -> Normalized (TyIFixG (unNormalized ty1) k (unNormalized ty2))
    , pay . c2 $ \k ty      -> Normalized (TyForallG k (unNormalized ty))
    , pay . c1 $ \tyBuiltin -> Normalized (TyBuiltinG tyBuiltin)
    , pay . c2 $ \ty1 ty2   -> Normalized (TyFunG (unNormalized ty1) (unNormalized ty2))
    ]

instance Enumerable tyname => Enumerable (Neutral (TypeG tyname)) where
  enumerate = share $ aconcat
    [ pay . c1 $ \i         -> Neutral (TyVarG i)
    , pay . c3 $ \ty1 ty2 k -> Neutral (TyAppG (unNeutral ty1) (unNormalized ty2) k)
    ]

-- ** Enumerating terms

-- Word8 is enumerable so we get an enumerable instance via pack
instance Enumerable ByteString where
  enumerate = share $ fmap pack access

data TermConstantG = TmIntegerG Integer
                   | TmByteStringG ByteString
                   | TmStringG String
                   | TmBoolG Bool
                   | TmUnitG ()
                   | TmCharG Char
                   deriving (Show, Eq)

deriveEnumerable ''TermConstantG

deriveEnumerable ''DefaultFun

data TermG tyname name
    = VarG
      name
    | LamAbsG
      (TermG tyname (S name))
    | ApplyG
      (TermG tyname name)
      (TermG tyname name)
      (TypeG tyname)
    | TyAbsG
      (TermG (S tyname) name)
    | TyInstG
      (TermG tyname name)
      (TypeG (S tyname))
      (TypeG tyname)
      (Kind ())
    | ConstantG TermConstantG
    | BuiltinG DefaultFun
    | WrapG (TermG tyname name)
    | UnWrapG (TypeG tyname) (Kind ()) (TypeG tyname) (TermG tyname name)
    | ErrorG (TypeG tyname)
    deriving (Eq, Show)

deriveBifunctor ''TermG
deriveEnumerable ''TermG

type ClosedTermG = TermG Z Z

-- * Converting types

-- |Convert generated builtin types to Plutus builtin types.
convertTypeBuiltin :: TypeBuiltinG -> SomeTypeIn DefaultUni
convertTypeBuiltin TyByteStringG = SomeTypeIn DefaultUniByteString
convertTypeBuiltin TyIntegerG    = SomeTypeIn DefaultUniInteger
convertTypeBuiltin TyBoolG       = SomeTypeIn DefaultUniBool
convertTypeBuiltin TyUnitG       = SomeTypeIn DefaultUniUnit
convertTypeBuiltin TyCharG       = SomeTypeIn DefaultUniChar
convertTypeBuiltin TyListG       = SomeTypeIn DefaultUniProtoList

-- Calling it 'TypeStringG' rather than @TyStringG@ to emphasize that it creates a 'TypeG' rather
-- than a 'TyBuiltinG'.
pattern TypeStringG :: TypeG n
pattern TypeStringG = TyAppG (TyBuiltinG TyListG) (TyBuiltinG TyCharG) (Type ())

-- |Convert well-kinded generated types to Plutus types.
--
-- NOTE: Passes an explicit `TyNameState`, instead of using a State
--       monad, as the type of the `TyNameState` changes throughout
--       the computation.  Alternatively, this could be written using
--       an indexed State monad.
--
-- NOTE: Roman points out that this is more like reader than state,
--       however it doesn't fit easily into this pattern as the
--       function `extTyNameState` is monadic (`MonadQuote`).
convertType
  :: (Show tyname, MonadQuote m, MonadError GenError m)
  => TyNameState tyname -- ^ Type name environment with fresh name stream
  -> Kind ()            -- ^ Kind of type below
  -> TypeG tyname       -- ^ Type to convert
  -> m (Type TyName DefaultUni ())
convertType tns _ (TyVarG i) =
  return (TyVar () (tynameOf tns i))
convertType tns (Type _) (TyFunG ty1 ty2) =
  TyFun () <$> convertType tns (Type ()) ty1 <*> convertType tns (Type ()) ty2
convertType tns (Type _) (TyIFixG ty1 k ty2) =
  TyIFix () <$> convertType tns k' ty1 <*> convertType tns k ty2
  where
    k' = KindArrow () (KindArrow () k (Type ())) (KindArrow () k (Type ()))
convertType tns (Type _) (TyForallG k ty) = do
  tns' <- extTyNameState tns
  TyForall () (tynameOf tns' FZ) k <$> convertType tns' (Type ()) ty
convertType _ _ (TyBuiltinG tyBuiltin) =
  pure $ TyBuiltin () (convertTypeBuiltin tyBuiltin)
convertType tns (KindArrow _ k1 k2) (TyLamG ty) = do
  tns' <- extTyNameState tns
  TyLam () (tynameOf tns' FZ) k1 <$> convertType tns' k2 ty
convertType tns k2 (TyAppG ty1 ty2 k1) =
  TyApp () <$> convertType tns (KindArrow () k1 k2) ty1 <*> convertType tns k1 ty2
convertType _ k ty = throwError $ BadTypeG k ty

-- |Convert generated closed types to Plutus types.
convertClosedType
  :: (MonadQuote m, MonadError GenError m)
  => Stream.Stream Text.Text
  -> Kind ()
  -> ClosedTypeG
  -> m (Type TyName DefaultUni ())
convertClosedType tynames = convertType (emptyTyNameState tynames)

-- ** Converting terms

-- |Convert (well-typed) generated terms to Plutus terms.
--
-- NOTE: Passes an explicit `TyNameState` and `NameState`, instead of using a
--       State monad, as the type of the `TyNameState` changes throughout the
--       computation. This could be written using an indexed State monad.
--
--       No checking is performed during conversion. The type is given
--       as it contains information needed to fully annotate a `Term`.
--       `Term`, unlike `TermG`, contains all necessary type
--       information to infer the type of the term. It is expected
--       that this function is only called on a well-typed
--       term. Violating this would point to an error in the
--       generator/checker.
convertTermConstant :: TermConstantG -> Some (ValueOf DefaultUni)
convertTermConstant (TmByteStringG b) = Some $ ValueOf DefaultUniByteString b
convertTermConstant (TmIntegerG i)    = Some $ ValueOf DefaultUniInteger i
convertTermConstant (TmStringG s)     = Some $ ValueOf DefaultUniString s
convertTermConstant (TmBoolG b)       = Some $ ValueOf DefaultUniBool b
convertTermConstant (TmUnitG u)       = Some $ ValueOf DefaultUniUnit u
convertTermConstant (TmCharG c)       = Some $ ValueOf DefaultUniChar c

convertTerm
  :: (Show tyname, Show name, MonadQuote m, MonadError GenError m)
  => TyNameState tyname -- ^ Type name environment with fresh name stream
  -> NameState name     -- ^ Name environment with fresh name stream
  -> TypeG tyname       -- ^ Type of term below
  -> TermG tyname name  -- ^ Term to convert
  -> m (Term TyName Name DefaultUni DefaultFun ())
convertTerm _tns ns _ty (VarG i) =
  return (Var () (nameOf ns i))
convertTerm tns ns (TyFunG ty1 ty2) (LamAbsG tm) = do
  ns' <- extNameState ns
  ty1' <- convertType tns (Type ()) ty1
  LamAbs () (nameOf ns' FZ) ty1' <$> convertTerm tns ns' ty2 tm
convertTerm tns ns ty2 (ApplyG tm1 tm2 ty1) =
  Apply () <$> convertTerm tns ns (TyFunG ty1 ty2) tm1 <*> convertTerm tns ns ty1 tm2
convertTerm tns ns (TyForallG k ty) (TyAbsG tm) = do
  tns' <- extTyNameState tns
  TyAbs () (tynameOf tns' FZ) k <$> convertTerm tns' ns ty tm
convertTerm tns ns _ (TyInstG tm cod ty k) =
  TyInst () <$> convertTerm tns ns (TyForallG k cod) tm <*> convertType tns k ty
convertTerm _tns _ns _ (ConstantG c) =
  return $ Constant () (convertTermConstant c)
convertTerm _tns _ns _ (BuiltinG b) = undefined -- return $ Builtin () b
convertTerm tns ns (TyIFixG ty1 k ty2) (WrapG tm) = IWrap () <$> convertType tns k' ty1 <*> convertType tns k ty2 <*> convertTerm tns ns (normalizeTypeG ty') tm
  where
  k'  = KindArrow () (KindArrow () k (Type ())) (KindArrow () k (Type ()))
  -- Γ ⊢ A · ƛ (μ (weaken A) (` Z)) · B
  ty' = TyAppG (TyAppG ty1 (TyLamG (TyIFixG (weakenTy ty1) k (TyVarG FZ))) (KindArrow () k (Type ()))) ty2 k
convertTerm tns ns _ (UnWrapG ty1 k ty2 tm) = Unwrap () <$> convertTerm tns ns (TyIFixG ty1 k ty2) tm
convertTerm tns _ns _ (ErrorG ty) = Error () <$> convertType tns (Type ()) ty
convertTerm _ _ ty tm = throwError $ BadTermG ty tm

-- |Convert generated closed terms to Plutus terms.
convertClosedTerm
  :: (MonadQuote m, MonadError GenError m)
  => Stream.Stream Text.Text
  -> Stream.Stream Text.Text
  -> ClosedTypeG
  -> ClosedTermG
  -> m (Term TyName Name DefaultUni DefaultFun ())
convertClosedTerm tynames names = convertTerm (emptyTyNameState tynames) (emptyNameState names)


-- * Checking

class Check t a where
  check :: t -> a -> Cool


-- ** Kind checking

-- |Kind check builtin types.
--
-- NOTE: If we make |checkTypeBuiltinG| non-strict in its second argument,
--       lazy-search will only ever return one of the various builtin types.
--       Perhaps this is preferable?
--
instance Check (Kind ()) TypeBuiltinG where
  check (Type _)                        TyByteStringG = true
  check (Type _)                        TyIntegerG    = true
  check (Type _)                        TyBoolG       = true
  check (Type _)                        TyCharG       = true
  check (Type _)                        TyUnitG       = true
  check (KindArrow _ (Type _) (Type _)) TyListG       = true
  check _                               _             = false

-- |Kind check types.
checkKindG :: KCS n -> Kind () -> TypeG n -> Cool
checkKindG kcs k (TyVarG i)
  = varKindOk
  where
    varKindOk = toCool $ k == kindOf kcs i

checkKindG kcs (Type _) (TyFunG ty1 ty2)
  = ty1KindOk &&& ty2KindOk
  where
    ty1KindOk = checkKindG kcs (Type ()) ty1
    ty2KindOk = checkKindG kcs (Type ()) ty2

checkKindG kcs (Type _) (TyIFixG ty1 k ty2)
  = ty1KindOk &&& ty2KindOk
  where
    ty1Kind   =
      KindArrow () (KindArrow () k (Type ())) (KindArrow () k (Type ()))
    ty1KindOk = checkKindG kcs ty1Kind ty1
    ty2KindOk = checkKindG kcs k ty2

checkKindG kcs (Type _) (TyForallG k body)
  = tyKindOk
  where
    tyKindOk = checkKindG (extKCS k kcs) (Type ()) body

checkKindG _ k (TyBuiltinG tyBuiltin)
  = tyBuiltinKindOk
  where
    tyBuiltinKindOk = check k tyBuiltin

checkKindG kcs (KindArrow () k1 k2) (TyLamG body)
  = bodyKindOk
  where
    bodyKindOk = checkKindG (extKCS k1 kcs) k2 body

checkKindG kcs k' (TyAppG ty1 ty2 k)
  = ty1KindOk &&& ty2KindOk
  where
    ty1Kind   = KindArrow () k k'
    ty1KindOk = checkKindG kcs ty1Kind ty1
    ty2KindOk = checkKindG kcs k ty2

checkKindG _ _ _ = false


instance Check (Kind ()) ClosedTypeG where
  check = checkKindG emptyKCS


instance Check (Kind ()) (Normalized ClosedTypeG) where
  check k ty = check k (unNormalized ty)


-- ** Kind checking state

newtype KCS tyname = KCS{ kindOf :: tyname -> Kind () }

emptyKCS :: KCS Z
emptyKCS = KCS{ kindOf = fromZ }

extKCS :: forall tyname. Kind () -> KCS tyname -> KCS (S tyname)
extKCS k KCS{..} = KCS{ kindOf = kindOf' }
  where
    kindOf' :: S tyname -> Kind ()
    kindOf' FZ     = k
    kindOf' (FS i) = kindOf i


-- ** Type checking

instance Check (TypeG n) TermConstantG where
  check (TyBuiltinG TyByteStringG) (TmByteStringG _) = true
  check (TyBuiltinG TyIntegerG   ) (TmIntegerG    _) = true
  check (TyBuiltinG TyBoolG      ) (TmBoolG       _) = true
  check (TyBuiltinG TyCharG      ) (TmCharG       _) = true
  check (TyBuiltinG TyUnitG      ) (TmUnitG       _) = true
  check TypeStringG                (TmStringG     _) = true
  check _                          _                 = false


defaultFunTypes :: Ord tyname => Map.Map (TypeG tyname) [DefaultFun]
defaultFunTypes = Map.fromList [(TyFunG (TyBuiltinG TyIntegerG) (TyFunG (TyBuiltinG TyIntegerG) (TyBuiltinG TyIntegerG))
                   ,[AddInteger,SubtractInteger,MultiplyInteger,DivideInteger,QuotientInteger,RemainderInteger,ModInteger])
                  ,(TyFunG (TyBuiltinG TyIntegerG) (TyFunG (TyBuiltinG TyIntegerG) (TyBuiltinG TyBoolG))
                   ,[LessThanInteger,LessThanEqualsInteger,GreaterThanInteger,GreaterThanEqualsInteger,EqualsInteger])
                  ,(TyFunG (TyBuiltinG TyByteStringG) (TyFunG (TyBuiltinG TyByteStringG) (TyBuiltinG TyByteStringG))
                   ,[Concatenate])
                  ,(TyFunG (TyBuiltinG TyIntegerG) (TyFunG (TyBuiltinG TyByteStringG) (TyBuiltinG TyByteStringG))
                   ,[TakeByteString,DropByteString])
                  ,(TyFunG (TyBuiltinG TyByteStringG) (TyBuiltinG TyByteStringG)
                   ,[Sha2_256,Sha3_256])
                  ,(TyFunG (TyBuiltinG TyByteStringG) (TyFunG (TyBuiltinG TyByteStringG) (TyFunG (TyBuiltinG TyByteStringG) (TyBuiltinG TyBoolG)))
                   ,[VerifySignature])
                  ,(TyFunG (TyBuiltinG TyByteStringG) (TyFunG (TyBuiltinG TyByteStringG) (TyBuiltinG TyBoolG))
                   ,[EqualsByteString,LessThanByteString,GreaterThanByteString])
                  ,(TyForallG (Type ()) (TyFunG (TyBuiltinG TyBoolG) (TyFunG (TyVarG FZ) (TyFunG (TyVarG FZ) (TyVarG FZ))))
                   ,[IfThenElse])
                  ,(TyFunG (TyBuiltinG TyCharG) TypeStringG
                   ,[CharToString])
                  ,(TyFunG TypeStringG (TyFunG TypeStringG TypeStringG)
                   ,[Append])
                  ,(TyFunG TypeStringG (TyBuiltinG TyUnitG)
                   ,[Trace])
                  ]

instance Ord tyname => Check (TypeG tyname) DefaultFun where
  check ty b = case Map.lookup ty defaultFunTypes of
    Just bs -> toCool $ elem b bs
    Nothing -> false

-- it's not clear to me whether this function should insist that some
-- types are in normal form...
checkTypeG
  :: Ord tyname
  => KCS tyname
  -> TCS tyname name
  -> TypeG tyname
  -> TermG tyname name
  -> Cool
checkTypeG _ tcs ty (VarG i)
  = varTypeOk
  where
    varTypeOk = toCool $ ty == typeOf tcs i

checkTypeG kcs tcs (TyForallG k ty) (TyAbsG tm)
  = tmTypeOk
  where
    tmTypeOk = checkTypeG (extKCS k kcs) (firstTCS FS tcs) ty tm

checkTypeG kcs tcs (TyFunG ty1 ty2) (LamAbsG tm)
  = tyKindOk &&& tmTypeOk
  where
    tyKindOk = checkKindG kcs (Type ()) ty1
    tmTypeOk = checkTypeG kcs (extTCS ty1 tcs) ty2 tm

checkTypeG kcs tcs ty2 (ApplyG tm1 tm2 ty1)
  = tm1TypeOk &&& tm2TypeOk
  where
    tm1TypeOk = checkTypeG kcs tcs (TyFunG ty1 ty2) tm1
    tm2TypeOk = checkTypeG kcs tcs ty1 tm2

checkTypeG kcs tcs vTy (TyInstG tm vCod ty k)
  = tmTypeOk &&& tyKindOk &&& tyOk
  where
    tmTypeOk = checkTypeG kcs tcs (TyForallG k vCod) tm
    tyKindOk = checkKindG kcs k ty
    tyOk = vTy == normalizeTypeG (TyAppG (TyLamG vCod) ty k)
checkTypeG _kcs _tcs tc (ConstantG c) = check tc c
checkTypeG kcs tcs (TyIFixG ty1 k ty2) (WrapG tm) = ty1Ok &&& ty2Ok &&& tmOk
  where
    ty1Ok = checkKindG kcs (KindArrow () (KindArrow () k (Type ())) (KindArrow () k (Type ()))) ty1
    ty2Ok = checkKindG kcs k ty2
    tmTy  = TyAppG (TyAppG ty1 (TyLamG (TyIFixG (weakenTy ty1) k (TyVarG FZ))) (KindArrow () k (Type ()))) ty2 k
    tmOk  = checkTypeG kcs tcs (normalizeTypeG tmTy) tm
checkTypeG kcs tcs vTy (UnWrapG ty1 k ty2 tm) = ty1Ok &&& ty2Ok &&& tmOk &&& vTyOk
  where
    ty1Ok = checkKindG kcs (KindArrow () (KindArrow () k (Type ())) (KindArrow () k (Type ()))) ty1
    ty2Ok = checkKindG kcs k ty2
    tmOk  = checkTypeG kcs tcs (TyIFixG ty1 k ty2) tm
    vTyOk = vTy == normalizeTypeG (TyAppG (TyAppG ty1 (TyLamG (TyIFixG (weakenTy ty1) k (TyVarG FZ))) (KindArrow () k (Type ()))) ty2 k)
checkTypeG kcs _tcs vTy (ErrorG ty) = tyKindOk &&& tyOk
  where
    tyKindOk = checkKindG kcs (Type ()) ty
    tyOk = vTy == normalizeTypeG ty
checkTypeG _kcs _tcs vTy (BuiltinG b) = check vTy b
checkTypeG _ _ _ _ = false

instance Check ClosedTypeG ClosedTermG where
  check = checkTypeG emptyKCS emptyTCS


-- ** Type checking state

newtype TCS tyname name = TCS{ typeOf :: name -> TypeG tyname }

emptyTCS :: TCS tyname Z
emptyTCS = TCS{ typeOf = fromZ }

extTCS :: forall tyname name. TypeG tyname -> TCS tyname name -> TCS tyname (S name)
extTCS ty TCS{..} = TCS{ typeOf = typeOf' }
  where
    typeOf' :: S name -> TypeG tyname
    typeOf' FZ     = ty
    typeOf' (FS i) = typeOf i

firstTCS :: (tyname -> tyname') -> TCS tyname name -> TCS tyname' name
firstTCS f tcs = TCS{ typeOf = fmap f . typeOf tcs }


-- * Normalisation

weakenTy :: TypeG m -> TypeG (S m)
weakenTy ty = sub (TyVarG . FS) ty

-- |Reduce a generated type by a single step, or fail.
stepTypeG :: TypeG n -> Maybe (TypeG n)
stepTypeG (TyVarG _)                  = empty
stepTypeG (TyFunG ty1 ty2)            = (TyFunG <$> stepTypeG ty1 <*> pure ty2)
                                    <|> (TyFunG <$> pure ty1 <*> stepTypeG ty2)
stepTypeG (TyIFixG ty1 k ty2)         = (TyIFixG <$> stepTypeG ty1 <*> pure k <*> pure ty2)
                                    <|> (TyIFixG <$> pure ty1 <*> pure k <*> stepTypeG ty2)
stepTypeG (TyForallG k ty)            = TyForallG <$> pure k <*> stepTypeG ty
stepTypeG (TyBuiltinG _)              = empty
stepTypeG (TyLamG ty)                 = TyLamG <$> stepTypeG ty
stepTypeG (TyAppG (TyLamG ty1) ty2 _) = pure (sub (\case FZ -> ty2; FS i -> TyVarG i) ty1)
stepTypeG (TyAppG ty1 ty2 k)          = (TyAppG <$> stepTypeG ty1 <*> pure ty2 <*> pure k)
                                    <|> (TyAppG <$> pure ty1 <*> stepTypeG ty2 <*> pure k)

-- |Normalise a generated type.
normalizeTypeG :: TypeG n -> TypeG n
normalizeTypeG ty = maybe ty normalizeTypeG (stepTypeG ty)

-- * Errors

-- NOTE: The errors we need to handle in property-based testing are
--       when the generator generates garbage (which shouldn't happen).

data GenError
  = forall tyname. Show tyname => BadTypeG (Kind ()) (TypeG tyname)
  | forall tyname name. (Show tyname, Show name) => BadTermG (TypeG tyname) (TermG tyname name)

instance Show GenError where
  show (BadTypeG k ty) =
    printf "Test generation error: convert type %s at kind %s" (show ty) (show k)
  show (BadTermG ty tm) =
    printf "Test generation error: convert term %s at type %s" (show tm) (show ty)
