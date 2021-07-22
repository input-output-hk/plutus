-- | @nat@ and related functions.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module PlutusCore.StdLib.Data.Nat
    ( natData
    , natTy
    , zero
    , succ
    , foldrNat
    , foldNat
    , natToInteger
    ) where

import           Prelude                         hiding (succ)

import           PlutusCore.Core
import           PlutusCore.Default.Builtins
import           PlutusCore.MkPlc
import           PlutusCore.Name
import           PlutusCore.Quote

import           PlutusCore.StdLib.Data.Function
import           PlutusCore.StdLib.Type

import           Universe

-- | @Nat@ as a PLC type.
--
-- > fix \(nat :: *) -> all r. r -> (nat -> r) -> r
natData :: RecursiveType uni fun ()
natData = runQuote $ do
    nat <- freshTyName "nat"
    r   <- freshTyName "r"
    makeRecursiveType () nat []
        . TyForall () r (Type ())
        . TyFun () (TyVar () r)
        . TyFun () (TyFun () (TyVar () nat) $ TyVar () r)
        $ TyVar () r

natTy :: Type TyName uni ()
natTy = _recursiveType natData

-- |  '0' as a PLC term.
--
-- > wrapNat [] /\(r :: *) -> \(z : r) (f : nat -> r) -> z
zero :: TermLike term TyName Name uni fun => term ()
zero = runQuote $ do
    let RecursiveType nat wrapNat = natData
    r <- freshTyName "r"
    z <- freshName "z"
    f <- freshName "f"
    return
        . wrapNat []
        . tyAbs () r (Type ())
        . lamAbs () z (TyVar () r)
        . lamAbs () f (TyFun () nat $ TyVar () r)
        $ var () z

-- |  'succ' as a PLC term.
--
-- > \(n : nat) -> wrapNat [] /\(r :: *) -> \(z : r) (f : nat -> r) -> f n
succ :: TermLike term TyName Name uni fun => term ()
succ = runQuote $ do
    let RecursiveType nat wrapNat = natData
    n <- freshName "n"
    r <- freshTyName "r"
    z <- freshName "z"
    f <- freshName "f"
    return
        . lamAbs () n nat
        . wrapNat []
        . tyAbs () r (Type ())
        . lamAbs () z (TyVar () r)
        . lamAbs () f (TyFun () nat $ TyVar () r)
        . apply () (var () f)
        $ var () n

-- |  @foldrNat@ as a PLC term.
--
-- > /\(r :: *) -> \(f : r -> r) (z : r) ->
-- >     fix {nat} {r} \(rec : nat -> r) (n : nat) ->
-- >         unwrap n {r} z \(n' : nat) -> f (rec n')
foldrNat :: TermLike term TyName Name uni fun => term ()
foldrNat = runQuote $ do
    let nat = _recursiveType natData
    r   <- freshTyName "r"
    f   <- freshName "f"
    z   <- freshName "z"
    rec <- freshName "rec"
    n   <- freshName "n"
    n'  <- freshName "n'"
    return
        . tyAbs () r (Type ())
        . lamAbs () f (TyFun () (TyVar () r) (TyVar () r))
        . lamAbs () z (TyVar () r)
        . apply () (mkIterInst () fix [nat, TyVar () r])
        . lamAbs () rec (TyFun () nat $ TyVar () r)
        . lamAbs () n nat
        . apply () (apply () (tyInst () (unwrap () (var () n)) $ TyVar () r) $ var () z)
        . lamAbs () n' nat
        . apply () (var () f)
        . apply () (var () rec)
        $ var () n'

-- |  @foldNat@ as a PLC term.
--
-- > /\(r :: *) -> \(f : r -> r) ->
-- >     fix {r} {nat -> r} \(rec : r -> nat -> r) (z : r) (n : nat) ->
-- >         unwrap n {r} z (\(n' : nat) -> rec (f z) n')
foldNat :: TermLike term TyName Name uni fun => term ()
foldNat = runQuote $ do
    let nat = _recursiveType natData
    r   <- freshTyName "r"
    f   <- freshName "f"
    rec <- freshName "rec"
    z   <- freshName "z"
    n   <- freshName "n"
    n'  <- freshName "n'"
    return
        . tyAbs () r (Type ())
        . lamAbs () f (TyFun () (TyVar () r) (TyVar () r))
        . apply () (mkIterInst () fix [TyVar () r, TyFun () nat $ TyVar () r])
        . lamAbs () rec (TyFun () (TyVar () r) . TyFun () nat $ TyVar () r)
        . lamAbs () z (TyVar () r)
        . lamAbs () n nat
        . apply () (apply () (tyInst () (unwrap () (var () n)) $ TyVar () r) $ var () z)
        . lamAbs () n' nat
        . mkIterApp () (var () rec)
        $ [ apply () (var () f) $ var () z
          , var () n'
          ]

-- | Convert a @nat@ to an @integer@.
--
-- > foldNat {integer} (addInteger 1) 1
natToInteger :: (TermLike term TyName Name uni DefaultFun, uni `Includes` Integer) => term ()
natToInteger = runQuote $ do
    return $
        mkIterApp () (tyInst () foldNat $ mkTyBuiltin @_ @Integer ())
          [ apply () (mkBuiltin () AddInteger) (mkConstant @Integer () 1)
          , mkConstant @Integer () 0
          ]
