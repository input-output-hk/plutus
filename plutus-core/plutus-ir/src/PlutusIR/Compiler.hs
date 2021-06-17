{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PlutusIR.Compiler (
    compileTerm,
    compileToReadable,
    compileReadableToPlc,
    Compiling,
    Error (..),
    AsError (..),
    AsTypeError (..),
    AsTypeErrorExt (..),
    Provenance (..),
    noProvenance,
    CompilationOpts,
    coOptimize,
    coMaxSimplifierIterations,
    coSimplifierUnwrapCancel,
    coSimplifierBeta,
    coSimplifierFloatTerm,
    coSimplifierInline,
    coSimplifierRemoveDeadBindings,
    defaultCompilationOpts,
    CompilationCtx,
    ccOpts,
    ccEnclosing,
    ccTypeCheckConfig,
    PirTCConfig(..),
    AllowEscape(..),
    toDefaultCompilationCtx) where

import           PlutusIR

import qualified PlutusIR.Compiler.Let              as Let
import           PlutusIR.Compiler.Lower
import           PlutusIR.Compiler.Provenance
import           PlutusIR.Compiler.Types
import           PlutusIR.Error
import qualified PlutusIR.Transform.Beta            as Beta
import qualified PlutusIR.Transform.DeadCode        as DeadCode
import qualified PlutusIR.Transform.Inline          as Inline
import qualified PlutusIR.Transform.LetFloat        as LetFloat
import qualified PlutusIR.Transform.NonStrict       as NonStrict
import           PlutusIR.Transform.Rename          ()
import qualified PlutusIR.Transform.ThunkRecursions as ThunkRec
import qualified PlutusIR.Transform.Unwrap          as Unwrap
import           PlutusIR.TypeCheck.Internal

import qualified PlutusCore                         as PLC

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           PlutusPrelude

-- | Actual simplifier
simplify
    :: forall m e uni fun a b. (Compiling m e uni fun a, Semigroup b)
    => Term TyName Name uni fun b -> m (Term TyName Name uni fun b)
simplify term =
  do
    selectedPasses <- map snd <$> filterM (shouldRunPass . fst) availablePasses
    let pass = foldl' (>=>) pure selectedPasses
    pass term
  where
    availablePasses
      :: [(Getting Bool CompilationOpts Bool,
           Term TyName Name uni fun b -> m (Term TyName Name uni fun b))
         ]
    availablePasses =
      [ (coSimplifierUnwrapCancel       , pure . Unwrap.unwrapCancel)
      , (coSimplifierBeta               , pure . Beta.beta)
      , (coSimplifierFloatTerm          , pure . LetFloat.floatTerm)
      , (coSimplifierInline             , Inline.inline)
      , (coSimplifierRemoveDeadBindings , DeadCode.removeDeadBindings)
      ]

    shouldRunPass :: Getting Bool CompilationOpts Bool -> m Bool
    shouldRunPass coOpt = view (ccOpts . coOpt)

-- | Perform some simplification of a 'Term'.
simplifyTerm :: forall m e uni fun a b. (Compiling m e uni fun a, Semigroup b) => Term TyName Name uni fun b -> m (Term TyName Name uni fun b)
simplifyTerm = runIfOpts $ PLC.rename >=> DeadCode.removeDeadBindings >=> simplify'
    -- NOTE: we need at least one pass of dead code elimination
    where
        simplify' :: Term TyName Name uni fun b -> m (Term TyName Name uni fun b)
        simplify' t = do
            maxIterations <- asks $ view (ccOpts . coMaxSimplifierIterations)
            simplifyNTimes maxIterations t
        -- Run the simplifier @n@ times
        simplifyNTimes :: Int -> Term TyName Name uni fun b -> m (Term TyName Name uni fun b)
        simplifyNTimes n = foldl' (>=>) pure (replicate n simplify)
-- Note: There was a bug in renamer handling non-rec terms, so we need to rename
-- again.
-- https://jira.iohk.io/browse/SCP-2156

-- | Perform floating/merging of lets in a 'Term' to their nearest lambda/Lambda/letStrictNonValue.
-- Note: It assumes globally unique names
floatTerm :: (Compiling m e uni fun a, Semigroup b) => Term TyName Name uni fun b -> m (Term TyName Name uni fun b)
floatTerm = runIfOpts $ pure . LetFloat.floatTerm

-- | Typecheck a PIR Term iff the context demands it.
-- Note: assumes globally unique names
typeCheckTerm :: Compiling m e uni fun b => Term TyName Name uni fun (Provenance b) -> m ()
typeCheckTerm t = do
    mtcconfig <- asks _ccTypeCheckConfig
    case mtcconfig of
        Just tcconfig -> void . runTypeCheckM tcconfig $ inferTypeM t
        Nothing       -> pure ()

-- | The 1st half of the PIR compiler pipeline up to floating/merging the lets.
-- We stop momentarily here to give a chance to the tx-plugin
-- to dump a "readable" version of pir (i.e. floated).
compileToReadable :: Compiling m e uni fun a
                  => Term TyName Name uni fun a
                  -> m (Term TyName Name uni fun (Provenance a))
compileToReadable =
    (pure . original)
    -- We need globally unique names for typechecking, floating, and compiling non-strict bindings
    >=> PLC.rename
    >=> through typeCheckTerm
    >=> simplifyTerm
    >=> (pure . ThunkRec.thunkRecursions)
    >=> floatTerm

-- | The 2nd half of the PIR compiler pipeline.
-- Compiles a 'Term' into a PLC Term, by removing/translating step-by-step the PIR's language construsts to PLC.
-- Note: the result *does* have globally unique names.
compileReadableToPlc :: Compiling m e uni fun a => Term TyName Name uni fun (Provenance a) -> m (PLCTerm uni fun a)
compileReadableToPlc =
    NonStrict.compileNonStrictBindings
    >=> Let.compileLets Let.DataTypes
    >=> Let.compileLets Let.RecTerms
    -- We introduce some non-recursive let bindings while eliminating recursive let-bindings, so we
    -- can eliminate any of them which are unused here.
    >=> simplifyTerm
    >=> Let.compileLets Let.Types
    >=> Let.compileLets Let.NonRecTerms
    >=> lowerTerm

--- | Compile a 'Term' into a PLC Term. Note: the result *does* have globally unique names.
compileTerm :: Compiling m e uni fun a
            => Term TyName Name uni fun a -> m (PLCTerm uni fun a)
compileTerm = compileToReadable >=> compileReadableToPlc
