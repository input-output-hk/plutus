{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module PlutusCore.Examples.Data.Pair
    ( obothPair
    ) where

import           PlutusCore.Core
import           PlutusCore.Default
import           PlutusCore.MkPlc
import           PlutusCore.Name
import           PlutusCore.Quote

import           PlutusCore.StdLib.Data.Pair

import           PlutusCore.Examples.Builtins

-- | Apply a monomorphic function to both components of a pair.
--
-- > /\(a :: *) -> \(f : a -> a) (p : pair a a) ->
-- >     comma {a} {a} (f (fst {a} {a} p)) (f (snd {a} {a} p))
obothPair :: TermLike term TyName Name DefaultUni (Either DefaultFun ExtensionFun) => term ()
obothPair = runQuote $ do
    a <- freshTyName "a"
    f <- freshName "f"
    p <- freshName "p"
    let atAA fun = mkIterInst () (mkBuiltin () fun) [TyVar () a, TyVar () a]
    return
        . tyAbs () a (Type ())
        . lamAbs () f (TyFun () (TyVar () a) $ TyVar () a)
        . lamAbs () p (mkIterTyApp () pair [TyVar () a, TyVar () a])
        $ mkIterApp () (atAA $ Right Comma)
            [ apply () (var () f) . apply () (atAA $ Left FstPair) $ var () p
            , apply () (var () f) . apply () (atAA $ Left SndPair) $ var () p
            ]
