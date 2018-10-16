-- | The CEK machine.
-- Rules are the same as for the CK machine from "Language.PlutusCore.Evaluation.CkMachine",
-- except we do not use substitution and use environments instead.
-- The CEK machine relies on variables having non-equal 'Unique's whenever they have non-equal
-- string names. I.e. 'Unique's are used instead of string names, so the renamer pass is required.
-- This is for efficiency reasons.
-- The type checker pass is required as well (and in our case it subsumes the renamer pass).
-- Feeding ill-typed terms to the CEK machine will likely result in a 'MachineException'.
-- The CEK machine generates booleans along the way which might contain globally non-unique 'Unique's.
-- This is not a problem as the CEK machines handles name capture by design.

{-# LANGUAGE ExistentialQuantification #-}

module Language.PlutusCore.Interpreter.CekMachine
    ( CekMachineException
    , EvaluationResult (..)
    , evaluateCek
    , runCek
    ) where

import           Language.PlutusCore
import           Language.PlutusCore.Constant
import           Language.PlutusCore.Evaluation.MachineException (MachineError (..), MachineException (..))
import           Language.PlutusCore.View
import           PlutusPrelude

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.IntMap                                     (IntMap)
import qualified Data.IntMap                                     as IntMap
import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Data.Void

type Plain f = f TyName Name ()

-- | The CEK machine-specific 'MachineException'.
type CekMachineException = MachineException Void

-- | A 'Value' packed together with the environment it's defined in.
data Closure = Closure
    { _closureVarEnv :: VarEnv
    , _closureValue  :: Plain Value
    }

-- | Environments used by the CEK machine.
-- Each row is a mapping from the 'Unique' representing a variable to a 'Closure'.
newtype VarEnv = VarEnv (IntMap Closure)

type DynamicBuiltinNameMeanings = Map DynamicBuiltinName DynamicBuiltinNameMeaning

data CekEnv = CekEnv
    { _cekEnvDbnms  :: DynamicBuiltinNameMeanings
    , _cekEnvVarEnv :: VarEnv
    }

-- | The monad the CEK machine runs in.
type CekM = ReaderT CekEnv (Either CekMachineException)

data Frame
    = FrameApplyFun VarEnv (Plain Value)         -- ^ @[V _]@
    | FrameApplyArg VarEnv (Plain Term)          -- ^ @[_ N]@
    | FrameTyInstArg (Type TyName ())            -- ^ @{_ A}@
    | FrameUnwrap                                -- ^ @(unwrap _)@
    | FrameWrap () (TyName ()) (Type TyName ())  -- ^ @(wrap α A _)@

type Context = [Frame]

getVarEnv :: CekM VarEnv
getVarEnv = asks _cekEnvVarEnv

withVarEnv :: VarEnv -> CekM a -> CekM a
withVarEnv env = local $ \cekEnv -> cekEnv { _cekEnvVarEnv = env }

-- | Extend an environment with a variable name, the value the variable stands for
-- and the environment the value is defined in.
extendVarEnv :: Name () -> Plain Value -> VarEnv -> VarEnv -> VarEnv
extendVarEnv argName arg argVarEnv (VarEnv oldVarEnv) =
    VarEnv $ IntMap.insert (unUnique $ nameUnique argName) (Closure argVarEnv arg) oldVarEnv

-- | Look up a variable name in the environment.
lookupVarName :: Name () -> CekM Closure
lookupVarName varName = do
    VarEnv varEnv <- getVarEnv
    case IntMap.lookup (unUnique $ nameUnique varName) varEnv of
        Nothing   -> throwError $ MachineException OpenTermEvaluatedMachineError (Var () varName)
        Just clos -> pure clos

-- | Look up a 'DynamicBuiltinName' in the environment.
lookupDynamicBuiltinName :: DynamicBuiltinName -> CekM (Maybe DynamicBuiltinNameMeaning)
lookupDynamicBuiltinName dynName = Map.lookup dynName <$> asks _cekEnvDbnms

-- | The computing part of the CEK machine.
-- Either
-- 1. adds a frame to the context and calls 'computeCek' ('TyInst', 'Apply', 'Wrap', 'Unwrap')
-- 2. calls 'returnCek' on values ('TyAbs', 'LamAbs', 'Constant')
-- 3. returns 'EvaluationFailure' ('Error')
-- 4. looks up a variable in the environment and calls 'returnCek' ('Var')
computeCek :: Context -> Plain Term -> CekM EvaluationResult
computeCek con (TyInst _ body ty)     = computeCek (FrameTyInstArg ty : con) body
computeCek con (Apply _ fun arg)      = do
    varEnv <- getVarEnv
    computeCek (FrameApplyArg varEnv arg : con) fun
computeCek con (Wrap ann tyn ty term) = computeCek (FrameWrap ann tyn ty : con) term
computeCek con (Unwrap _ term)        = computeCek (FrameUnwrap : con) term
computeCek con tyAbs@TyAbs{}          = returnCek con tyAbs
computeCek con lamAbs@LamAbs{}        = returnCek con lamAbs
computeCek con constant@Constant{}    = returnCek con constant
computeCek _   Error{}                = pure EvaluationFailure
computeCek con (Var _ varName)        = do
    Closure newVarEnv term <- lookupVarName varName
    withVarEnv newVarEnv $ returnCek con term

-- | The returning part of the CEK machine.
-- Returns 'EvaluationSuccess' in case the context is empty, otherwise pops up one frame
-- from the context and either
-- 1. performs reduction and calls 'computeCek' ('FrameTyInstArg', 'FrameApplyFun', 'FrameUnwrap')
-- 2. performs a constant application and calls 'returnCek' ('FrameTyInstArg', 'FrameApplyFun')
-- 3. puts 'FrameApplyFun' on top of the context and proceeds with the argument from 'FrameApplyArg'
-- 4. grows the resulting term ('FrameWrap')
returnCek :: Context -> Plain Value -> CekM EvaluationResult
returnCek []                                  res = pure $ EvaluationSuccess res
returnCek (FrameTyInstArg ty           : con) fun = instantiateEvaluate con ty fun
returnCek (FrameApplyArg argVarEnv arg : con) fun = do
    funVarEnv <- getVarEnv
    withVarEnv argVarEnv $ computeCek (FrameApplyFun funVarEnv fun : con) arg
returnCek (FrameApplyFun funVarEnv fun : con) arg = do
    argVarEnv <- getVarEnv
    applyEvaluate funVarEnv argVarEnv con fun arg
returnCek (FrameWrap ann tyn ty        : con) val = returnCek con $ Wrap ann tyn ty val
returnCek (FrameUnwrap                 : con) dat = case dat of
    Wrap _ _ _ term -> returnCek con term
    term            -> throwError $ MachineException NonWrapUnwrappedMachineError term

-- | Instantiate a term with a type and proceed.
-- In case of 'TyAbs' just ignore the type. Otherwise check if the term is an
-- iterated application of a 'BuiltinName' to a list of 'Value's and, if succesful,
-- apply the term to the type via 'TyInst'.
instantiateEvaluate :: Context -> Type TyName () -> Plain Term -> CekM EvaluationResult
instantiateEvaluate con _  (TyAbs _ _ _ body) = computeCek con body
instantiateEvaluate con ty fun
    | isJust $ termAsPrimIterApp fun = returnCek con $ TyInst () fun ty
    | otherwise                      =
        throwError $ MachineException NonPrimitiveInstantiationMachineError fun

-- | Apply a function to an argument and proceed.
-- If the function is a 'LamAbs', then extend the current environment with a new variable and proceed.
-- If the function is not a 'LamAbs', then 'Apply' it to the argument and view this
-- as an iterated application of a 'BuiltinName' to a list of 'Value's.
-- If succesful, proceed with either this same term or with the result of the computation
-- depending on whether 'BuiltinName' is saturated or not.
applyEvaluate
    :: VarEnv -> VarEnv -> Context -> Plain Value -> Plain Value -> CekM EvaluationResult
applyEvaluate funVarEnv argVarEnv con (LamAbs _ name _ body) arg =
    withVarEnv (extendVarEnv name arg argVarEnv funVarEnv) $ computeCek con body
applyEvaluate funVarEnv _         con fun                    arg =
    let term = Apply () fun arg in
        case termAsPrimIterApp term of
            Nothing                       ->
                throwError $ MachineException NonPrimitiveApplicationMachineError term
            Just (IterApp headName spine) -> do
                constAppResult <- runQuote <$> applyStagedBuiltinName headName spine
                withVarEnv funVarEnv $ case constAppResult of
                    ConstAppSuccess res -> returnCek con res
                    ConstAppFailure     -> pure EvaluationFailure
                    ConstAppStuck       -> returnCek con term
                    ConstAppError   err ->
                        throwError $ MachineException (ConstAppMachineError err) term

applyStagedBuiltinName :: StagedBuiltinName -> [Plain Value] -> CekM (Quote ConstAppResult)
applyStagedBuiltinName (DynamicStagedBuiltinName name) args = do
    mayMean <- lookupDynamicBuiltinName name
    pure $ case mayMean of
        -- return 'ConstAppFailue' in case a dynamic built-in is out of scope.
        Nothing                                -> pure ConstAppFailure
        Just (DynamicBuiltinNameMeaning sch x) -> applyTypeSchemed sch x args
applyStagedBuiltinName (StaticStagedBuiltinName  name) args = pure $ applyBuiltinName name args

-- | Evaluate a term using the CEK machine.
evaluateCekCatch
    :: DynamicBuiltinNameMeanings -> Plain Term -> Either CekMachineException EvaluationResult
evaluateCekCatch dbnms term =
    runReaderT (computeCek [] term) (CekEnv dbnms $ VarEnv IntMap.empty)

-- | Evaluate a term using the CEK machine. May throw a 'CekMachineException'.
evaluateCek :: DynamicBuiltinNameMeanings -> Term TyName Name () -> EvaluationResult
evaluateCek = either throw id .* evaluateCekCatch

-- | Run a program using the CEK machine. May throw a 'CekMachineException'.
-- Calls 'evaluateCek' under the hood.
runCek :: DynamicBuiltinNameMeanings -> Program TyName Name () -> EvaluationResult
runCek dbnms (Program _ _ term) = evaluateCek dbnms term
