{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module PlutusCore.Examples.Data.List
    ( omapList
    ) where

import           PlutusCore.Core
import           PlutusCore.Default
import           PlutusCore.MkPlc
import           PlutusCore.Name
import           PlutusCore.Quote

import           PlutusCore.StdLib.Data.Function
import           PlutusCore.StdLib.Data.List

import           PlutusCore.Examples.Builtins

-- | Monomorphic @map@ over built-in lists.
--
-- /\(a :: *) -> \(f : a -> a) ->
--     fix {list a} {list a} \(rec : list a -> list a) (xs : list a) ->
--         caseList {a} xs {list a} xs \(x : a) (xs' : list a) -> cons {a} (f x) (rec xs')
omapList :: Term TyName Name DefaultUni (Either DefaultFun ExtensionFun) ()
omapList = runQuote $ do
    a   <- freshTyName "a"
    f   <- freshName "f"
    rec <- freshName "rec"
    xs  <- freshName "xs"
    x   <- freshName "x"
    xs' <- freshName "xs'"
    let listA = TyApp () list $ TyVar () a
        unwrap' ann = apply ann . tyInst () (mapFun Left caseList) $ TyVar () a
    return
        . tyAbs () a (Type ())
        . lamAbs () f (TyFun () (TyVar () a) $ TyVar () a)
        . apply () (mkIterInst () fix [listA, listA])
        . lamAbs () rec (TyFun () listA listA)
        . lamAbs () xs listA
        . apply () (apply () (tyInst () (unwrap' () (var () xs)) listA) $ var () xs)
        . lamAbs () x (TyVar () a)
        . lamAbs () xs' listA
        $ mkIterApp () (tyInst () (mkBuiltin () $ Right Cons) $ TyVar () a)
            [ apply () (var () f) $ var () x
            , apply () (var () rec) $ var () xs'
            ]
