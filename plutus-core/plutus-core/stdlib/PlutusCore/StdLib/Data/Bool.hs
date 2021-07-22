-- | @boolean@ and related functions.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module PlutusCore.StdLib.Data.Bool
    ( bool
    , true
    , false
    , ifThenElse
    ) where

import           PlutusCore.Core
import           PlutusCore.Default.Builtins
import           PlutusCore.MkPlc
import           PlutusCore.Name
import           PlutusCore.Quote

import           PlutusCore.StdLib.Data.Unit

import           Universe

-- | 'Bool' as a PLC type.
bool :: uni `Includes` Bool => Type TyName uni ()
bool = mkTyBuiltin @_ @Bool ()

-- | 'True' as a PLC term.
true :: (TermLike term TyName Name uni fun, uni `Includes` Bool) => term ()
true = mkConstant () True

-- | 'False' as a PLC term.
false :: (TermLike term TyName Name uni fun, uni `Includes` Bool) => term ()
false = mkConstant () False

-- | @if_then_else_@ as a PLC term.
--
-- > /\(A :: *) -> \(b : Bool) (x y : () -> A) -> IfThenElse {() -> A} b x y ()
ifThenElse
    :: ( TermLike term TyName Name uni DefaultFun
       , uni `Includes` Bool, uni `Includes` ()
       )
    => term ()
ifThenElse = runQuote $ do
    a <- freshTyName "a"
    b <- freshName "b"
    x <- freshName "x"
    y <- freshName "y"
    let unitFunA = TyFun () unit (TyVar () a)
    return
       . tyAbs () a (Type ())
      $ mkIterLamAbs [
          VarDecl () b bool,
          VarDecl () x unitFunA,
          VarDecl () y unitFunA
          ]
      $ mkIterApp ()
          (tyInst () (mkBuiltin () IfThenElse) unitFunA)
          [var () b, var () x, var () y, unitval]
