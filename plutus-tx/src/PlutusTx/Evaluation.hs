{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module PlutusTx.Evaluation
    ( evaluateCek
    , unsafeEvaluateCek
    , evaluateCekTrace
    , ErrorWithCause(..)
    , EvaluationError(..)
    , CekExTally
    , CekEvaluationException
    )
where

import qualified PlutusCore                               as PLC
import           PlutusCore.Default
import           PlutusCore.Name

import           UntypedPlutusCore
import           UntypedPlutusCore.Evaluation.Machine.Cek hiding (evaluateCek, unsafeEvaluateCek)
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as Cek

-- | Evaluate a program in the CEK machine with the usual string dynamic builtins.
evaluateCek
    :: (uni ~ DefaultUni, fun ~ DefaultFun)
    => Program Name uni fun () -> Either (CekEvaluationException uni fun) (Term Name uni fun ())
evaluateCek (Program _ _ t) = Cek.evaluateCekNoEmit PLC.defaultCekCostModel t

-- | Evaluate a program in the CEK machine with the usual string dynamic builtins. May throw.
unsafeEvaluateCek
    :: (uni ~ DefaultUni, fun ~ DefaultFun)
    => Program Name uni fun () -> EvaluationResult (Term Name uni fun ())
unsafeEvaluateCek (Program _ _ t) = Cek.unsafeEvaluateCekNoEmit PLC.defaultCekCostModel t

-- | Evaluate a program in the CEK machine with the usual string dynamic builtins and tracing, additionally
-- returning the trace output.
evaluateCekTrace
    :: (uni ~ DefaultUni, fun ~ DefaultFun)
    => Program Name uni fun ()
    -> ([String], CekExTally fun, Either (CekEvaluationException uni fun) (Term Name uni fun ()))
evaluateCekTrace (Program _ _ t) =
    case runCek PLC.defaultCekCostModel Cek.tallying True t of
        (errOrRes, TallyingSt st _, logs) -> (logs, st, errOrRes)
