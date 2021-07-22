{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

-- | In this module we define Church-encoded type-level natural numbers,
-- Church-encoded vectors and Scott-encoded vectors.
--
-- See @/docs/fomega/gadts/ScottVec.agda@ for how Scott-encoded vectors work.

module PlutusCore.Examples.Data.Vec where

import           PlutusCore.Core
import           PlutusCore.Default.Builtins
import           PlutusCore.MkPlc
import           PlutusCore.Name
import           PlutusCore.Quote

import           PlutusCore.StdLib.Data.Integer
import           PlutusCore.StdLib.Data.Unit

import           Universe

-- |
--
-- > natK = (* -> *) -> * -> *
natK :: Kind ()
natK = KindArrow () (KindArrow () (Type ()) $ Type ()) . KindArrow () (Type ()) $ Type ()

-- We don't have eta-equality for types, so we need to eta-expand some things manually.
-- Should we have eta-equality?
-- |
--
-- > getEta n = \(f :: * -> *) (z :: *) -> n f z
getEta :: Type TyName uni () -> Quote (Type TyName uni ())
getEta n = do
    f <- freshTyName "f"
    z <- freshTyName "z"
    return
        . TyLam () f (KindArrow () (Type ()) $ Type ())
        . TyLam () z (Type ())
        $ mkIterTyApp () n
            [ TyVar () f
            , TyVar () z
            ]

-- |
--
-- > zeroT = \(f :: * -> *) (z :: *) -> z
zeroT :: Type TyName uni ()
zeroT = runQuote $ do
    f <- freshTyName "f"
    z <- freshTyName "z"
    return
        . TyLam () f (KindArrow () (Type ()) $ Type ())
        . TyLam () z (Type ())
        $ TyVar () z

-- |
--
-- > succT = \(n : natK) (f :: * -> *) (z :: *) -> f (n f z)
succT :: Type TyName uni ()
succT = runQuote $ do
    n <- freshTyName "n"
    f <- freshTyName "f"
    z <- freshTyName "z"
    return
        . TyLam () n natK
        . TyLam () f (KindArrow () (Type ()) $ Type ())
        . TyLam () z (Type ())
        . TyApp () (TyVar () f)
        $ mkIterTyApp () (TyVar () n)
            [ TyVar () f
            , TyVar () z
            ]

-- |
--
-- > plusT = \(n : natK) (m : natK) (f :: * -> *) (z :: *) -> n f (m f z)
plusT :: Type TyName uni ()
plusT = runQuote $ do
    n <- freshTyName "n"
    m <- freshTyName "m"
    f <- freshTyName "f"
    z <- freshTyName "z"
    return
        . TyLam () n natK
        . TyLam () m natK
        . TyLam () f (KindArrow () (Type ()) $ Type ())
        . TyLam () z (Type ())
        $ mkIterTyApp () (TyVar () n)
            [ TyVar () f
            , mkIterTyApp () (TyVar () m)
                [ TyVar () f
                , TyVar () z
                ]
            ]

-- |
--
-- > stepFun a r1 r2 = all (p :: natK). a -> r1 p -> r2 (succT p)
getStepFun :: TyName -> Type TyName uni () -> TyName -> Quote (Type TyName uni ())
getStepFun a r1 r2 = do
    p <- freshTyName "p"
    return
        . TyForall () p natK
        . TyFun () (TyVar () a)
        . TyFun () (TyApp () r1 $ TyVar () p)
        . TyApp () (TyVar () r2)
        . TyApp () succT
        $ TyVar () p

-- |
--
-- > churchVec =
-- >     \(a :: *) (n :: natK) ->
-- >         all (r :: natK -> *). (all (p :: natK). a -> r p -> r (succT p)) -> r zeroT -> r n
churchVec :: Type TyName uni ()
churchVec = runQuote $ do
    a <- freshTyName "a"
    n <- freshTyName "n"
    r <- freshTyName "r"
    stepFun <- getStepFun a (TyVar () r) r
    return
        . TyLam () a (Type ())
        . TyLam () n natK
        . TyForall () r (KindArrow () natK $ Type ())
        . TyFun () stepFun
        . TyFun () (TyApp () (TyVar () r) zeroT)
        . TyApp () (TyVar () r)
        $ TyVar () n

-- |
--
-- > churchNil =
-- >     /\(a :: *) ->
-- >         /\(r :: natK -> *) -> \(f : all (p :: natK). a -> r p -> r (succT p)) (z : r zeroT) ->
-- >             z
churchNil :: Term TyName Name uni fun ()
churchNil = runQuote $ do
    a <- freshTyName "a"
    r <- freshTyName "r"
    f <- freshName "f"
    z <- freshName "z"
    stepFun <- getStepFun a (TyVar () r) r
    return
        . TyAbs () a (Type ())
        . TyAbs () r (KindArrow () natK $ Type ())
        . LamAbs () f stepFun
        . LamAbs () z (TyApp () (TyVar () r) zeroT)
        $ Var () z

-- |
--
-- > churchCons =
-- >     /\(a :: *) (n :: natK) -> \(x : a) (xs : churchVec a n) ->
-- >         /\(r :: natK -> *) -> \(f : all (p :: natK). a -> r p -> r (succT p)) (z : r zeroT) ->
-- >             f {n} x (xs {r} f z)
churchCons :: Term TyName Name uni fun ()
churchCons = runQuote $ do
    a <- freshTyName "a"
    n <- freshTyName "n"
    x <- freshName "x"
    xs <- freshName "xs"
    r <- freshTyName "r"
    f <- freshName "f"
    z <- freshName "z"
    stepFun <- getStepFun a (TyVar () r) r
    return
        . TyAbs () a (Type ())
        . TyAbs () n natK
        . LamAbs () x (TyVar () a)
        . LamAbs () xs (mkIterTyApp () churchVec [TyVar () a, TyVar () n])
        . TyAbs () r (KindArrow () natK $ Type ())
        . LamAbs () f stepFun
        . LamAbs () z (TyApp () (TyVar () r) zeroT)
        $ mkIterApp () (TyInst () (Var () f) $ TyVar () n)
            [ Var () x
            , mkIterApp () (TyInst () (Var () xs) $ TyVar () r)
                [ Var () f
                , Var () z
                ]
            ]

-- |
--
-- > churchConcat =
-- >     /\(a :: *) (n :: natK) (m :: natK) -> \(xs : churchVec a n) (ys : churchVec a m) ->
-- >         /\(r :: natK -> *) -> \(f : all (p :: natK). a -> r p -> r (succT p)) (z : r zeroT) ->
-- >             xs
-- >                 {\(p :: natK) -> r (plusT p m)}
-- >                 (/\(p :: natK) -> f {plusT p m})
-- >                 (ys {r} f z)
churchConcat :: Term TyName Name uni fun ()
churchConcat = runQuote $ do
    a <- freshTyName "a"
    n <- freshTyName "n"
    m <- freshTyName "m"
    xs <- freshName "xs"
    ys <- freshName "ys"
    r <- freshTyName "r"
    f <- freshName "f"
    z <- freshName "z"
    p <- freshTyName "p"
    stepFun <- getStepFun a (TyVar () r) r
    mEta <- getEta $ TyVar () m
    let plusTPM = mkIterTyApp () plusT [TyVar () p, TyVar () m]
    return
        . TyAbs () a (Type ())
        . TyAbs () n natK
        . TyAbs () m natK
        . LamAbs () xs (mkIterTyApp () churchVec [TyVar () a, TyVar () n])
        . LamAbs () ys (mkIterTyApp () churchVec [TyVar () a, mEta])
        . TyAbs () r (KindArrow () natK $ Type ())
        . LamAbs () f stepFun
        . LamAbs () z (TyApp () (TyVar () r) zeroT)
        $ mkIterApp () (TyInst () (Var () xs) . TyLam () p natK $ TyApp () (TyVar () r) plusTPM)
            [ TyAbs () p natK $ TyInst () (Var () f) plusTPM
            , mkIterApp () (TyInst () (Var () ys) $ TyVar () r)
                [ Var () f
                , Var () z
                ]
            ]

-- |
--
-- > scottVecF =
-- >     \(a :: *) (rec :: natK -> *) (n :: natK) ->
-- >         all (r :: natK -> *). (all (p :: natK). a -> rec p -> r (succT p)) -> r zeroT -> r n
scottVecF :: Type TyName uni ()
scottVecF = runQuote $ do
    a <- freshTyName "a"
    rec <- freshTyName "rec"
    n <- freshTyName "n"
    r <- freshTyName "r"
    stepFun <- getStepFun a (TyVar () rec) r
    return
        . TyLam () a (Type ())
        . TyLam () rec (KindArrow () natK $ Type ())
        . TyLam () n natK
        . TyForall () r (KindArrow () natK $ Type ())
        . TyFun () stepFun
        . TyFun () (TyApp () (TyVar () r) zeroT)
        . TyApp () (TyVar () r)
        $ TyVar () n

-- |
--
-- > scottVec = \(a :: *) (n :: natK) -> ifix (scottVecF a) n
scottVec :: Type TyName uni ()
scottVec = runQuote $ do
    a <- freshTyName "a"
    n <- freshTyName "n"
    return
        . TyLam () a (Type ())
        . TyLam () n natK
        . TyIFix () (TyApp () scottVecF $ TyVar () a)
        $ TyVar () n

-- |
--
-- > scottNil =
-- >     /\(a :: *) ->
-- >         iwrap (scottVecF a) zeroT
-- >             (/\(r :: natK -> *) ->
-- >                 \(f : all (p :: natK). a -> scottVec a p -> r (succT p)) (z : r zeroT) ->
-- >                     z)
scottNil :: Term TyName Name uni fun ()
scottNil = runQuote $ do
    a <- freshTyName "a"
    r <- freshTyName "r"
    f <- freshName "f"
    z <- freshName "z"
    stepFun <- getStepFun a (TyApp () scottVec $ TyVar () a) r
    return
        . TyAbs () a (Type ())
        . IWrap () (TyApp () scottVecF $ TyVar () a) zeroT
        . TyAbs () r (KindArrow () natK $ Type ())
        . LamAbs () f stepFun
        . LamAbs () z (TyApp () (TyVar () r) zeroT)
        $ Var () z

-- |
--
-- > scottCons =
-- >     /\(a :: *) (n :: natK) -> \(x : a) (xs : scottVec a n) ->
-- >         iwrap (scottVecF a) (succT n)
-- >             (/\(r :: natK -> *) ->
-- >                 \(f : all (p :: natK). a -> scottVec a p -> r (succT p)) (z : r zeroT) ->
-- >                     f {n} x xs)
scottCons :: Term TyName Name uni fun ()
scottCons = runQuote $ do
    a <- freshTyName "a"
    n <- freshTyName "n"
    x <- freshName "x"
    xs <- freshName "xs"
    r <- freshTyName "r"
    f <- freshName "f"
    z <- freshName "z"
    stepFun <- getStepFun a (TyApp () scottVec $ TyVar () a) r
    return
        . TyAbs () a (Type ())
        . TyAbs () n natK
        . LamAbs () x (TyVar () a)
        . LamAbs () xs (mkIterTyApp () scottVec [TyVar () a, TyVar () n])
        . IWrap () (TyApp () scottVecF $ TyVar () a) (TyApp () succT $ TyVar () n)
        . TyAbs () r (KindArrow () natK $ Type ())
        . LamAbs () f stepFun
        . LamAbs () z (TyApp () (TyVar () r) zeroT)
        $ mkIterApp () (TyInst () (Var () f) $ TyVar () n)
            [ Var () x
            , Var () xs
            ]

-- |
--
-- > scottHead =
-- >     /\(a :: *) (n :: natK) -> (xs : scottVec a (suc n)) ->
-- >         unwrap
-- >             xs
-- >             {\(p :: natK) -> p (\(z :: *) -> a) unit}
-- >             (/\(p :: natK) (x : a) (xs' : scottVec a p) -> x)
-- >             unitval
scottHead :: uni `Includes` () => Term TyName Name uni fun ()
scottHead = runQuote $ do
    a <- freshTyName "a"
    n <- freshTyName "n"
    p <- freshTyName "p"
    z <- freshTyName "z"
    x <- freshName "x"
    xs <- freshName "xs"
    xs' <- freshName "xs'"
    return
        . TyAbs () a (Type ())
        . TyAbs () n natK
        . LamAbs () xs (mkIterTyApp () scottVec [TyVar () a, TyApp () succT $ TyVar () n])
        $ mkIterApp ()
            (   TyInst () (Unwrap () $ Var () xs)
              . TyLam () p natK
                $ mkIterTyApp () (TyVar () p)
                    [ TyLam () z (Type ()) $ TyVar () a
                    , unit
                    ])
            [   TyAbs () p natK
              . LamAbs () x (TyVar () a)
              . LamAbs () xs' (mkIterTyApp () scottVec [TyVar () a, TyVar () p])
              $ Var () x
            , unitval
            ]

-- |
--
-- > scottSumHeadsOr0 =
-- >     /\(n :: natK) -> (xs ys : scottVec integer n) ->
-- >         unwrap
-- >             xs
-- >             {\(p :: natK) -> (scottVec Integer n -> scottVec integer p) -> integer}
-- >             (/\(p :: natK) (x : integer) (xs' : scottVec integer p)
-- >                 \(coe : scottVec Integer n -> scottVec integer (suc p)) ->
-- >                     x + scottHead {integer} {p} (coe ys))
-- >             (\(coe : scottVec Integer n -> scottVec integer zero) -> 0)
-- >             (/\(xs' :: scottVec Integer n) -> xs')
scottSumHeadsOr0
    :: (uni `Includes` Integer, uni `Includes` ())
    => Term TyName Name uni DefaultFun ()
scottSumHeadsOr0 = runQuote $ do
    n <- freshTyName "n"
    p <- freshTyName "p"
    x <- freshName "x"
    xs <- freshName "xs"
    ys <- freshName "ys"
    xs' <- freshName "xs'"
    coe <- freshName "coe"
    let vecInteger l = mkIterTyApp () scottVec [integer, l]
    return
        . TyAbs () n natK
        . LamAbs () xs (vecInteger $ TyVar () n)
        . LamAbs () ys (vecInteger $ TyVar () n)
        $ mkIterApp ()
            (   TyInst () (Unwrap () $ Var () xs)
              . TyLam () p natK
              . TyFun () (TyFun () (vecInteger $ TyVar () n) $ vecInteger (TyVar () p))
              $ integer
            )
            [   TyAbs () p natK
              . LamAbs () x integer
              . LamAbs () xs' (vecInteger $ TyVar () p)
              . LamAbs () coe
                  (TyFun () (vecInteger $ TyVar () n) $ vecInteger (TyApp () succT $ TyVar () p))
              $ mkIterApp () (mkBuiltin () AddInteger)
                  [ Var () x
                  ,   Apply () (mkIterInst () scottHead [integer, TyVar () p])
                    . Apply () (Var () coe)
                    $ Var () ys
                  ]
            ,   LamAbs () coe (TyFun () (vecInteger $ TyVar () n) $ vecInteger zeroT)
              $ mkConstant @Integer () 0
            , LamAbs () xs' (vecInteger $ TyVar () n) $ Var () xs'
            ]
