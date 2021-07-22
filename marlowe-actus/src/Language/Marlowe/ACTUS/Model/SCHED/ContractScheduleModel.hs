{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.Marlowe.ACTUS.Model.SCHED.ContractScheduleModel where

import           Data.Maybe                                             (fromJust, fromMaybe, isJust, isNothing)
import           Language.Marlowe.ACTUS.Definitions.ContractTerms       (Cycle (..), IPCB (IPCB_NTL), PPEF (..),
                                                                         PYTP (..), SCEF (..))
import           Language.Marlowe.ACTUS.Definitions.Schedule            (ShiftedDay (..))
import           Language.Marlowe.ACTUS.Model.Utility.DateShift         (applyBDCWithCfg)
import           Language.Marlowe.ACTUS.Model.Utility.ScheduleGenerator (generateRecurrentScheduleWithCorrections, inf,
                                                                         minusCycle, plusCycle, remove)

-- Principal at Maturity
_S = generateRecurrentScheduleWithCorrections
shift = applyBDCWithCfg

_SCHED_IED_PAM scfg _IED = Just [shift scfg _IED]

_SCHED_MD_PAM scfg tmd = Just [shift scfg tmd]

_SCHED_PP_PAM scfg _PREF _OPCL _IED _OPANX _MD =
    let maybeS  | isNothing _OPANX && isNothing _OPCL = Nothing
                | isNothing _OPANX                   = Just $ _IED `plusCycle` fromJust _OPCL
                | otherwise                          = _OPANX

    in case _PREF of
        PPEF_N -> Nothing
        _      -> (\s -> _S s (fromJust _OPCL) _MD scfg) <$> maybeS

_SCHED_PY_PAM scfg _PYTP _PREF _OPCL _IED _OPANX _MD =
    case _PYTP of
        PYTP_O -> Nothing
        _      -> _SCHED_PP_PAM scfg _PREF _OPCL _IED _OPANX _MD

_SCHED_FP_PAM scfg _FER _FECL _IED _FEANX _MD =
    let maybeS  | isNothing _FEANX && isNothing _FECL = Nothing
                | isNothing _FEANX                   = Just $ _IED `plusCycle` fromJust _FECL
                | otherwise                          = _FEANX

        result  | _FER == 0.0                         = Nothing
                | otherwise                          = (\s -> _S s (fromJust _FECL){ includeEndDay = True } _MD scfg) <$> maybeS
    in result

_SCHED_PRD_PAM scfg _PRD =
  case _PRD of
    Just _PRD' ->
      Just [shift scfg _PRD']
    Nothing ->
      Nothing

_SCHED_TD_PAM scfg _TD =
  case _TD of
    Just _TD' ->
      Just [shift scfg _TD']
    Nothing ->
      Nothing

_SCHED_IP_PAM scfg _IPNR _IED _IPANX _IPCL _IPCED _MD =
    let maybeS  | isNothing _IPANX && isNothing _IPCL  = Nothing
                -- | isJust _IPCED                       = _IPCED -- present in the specification, but not correct, according to pam18 test case
                | isNothing _IPANX                    = Just $ _IED `plusCycle` fromJust _IPCL
                | otherwise                           = _IPANX

        result  | isNothing _IPNR                     = Nothing
                | otherwise                           = (\s -> _S s (fromJust _IPCL){ includeEndDay = True } _MD scfg) <$> maybeS

        result' | isJust result && isJust _IPCED = Just $ filter (\ss -> (calculationDay ss) > fromJust _IPCED) $ fromJust result
                | otherwise     = result
    in result'

_SCHED_IPCI_PAM scfg _IED _IPANX _IPCL _IPCED _MD _IPNR =
    -- calculate IP sched:
    let maybeS  | isNothing _IPANX && isNothing _IPCL  = Nothing
                -- | isJust _IPCED                       = _IPCED -- present in the specification, but not correct, according to pam18 test case
                | isNothing _IPANX                    = Just $ _IED `plusCycle` fromJust _IPCL
                | otherwise                           = _IPANX

        schedIP | isNothing _IPNR                     = Nothing
                | otherwise                           = (\s -> _S s (fromJust _IPCL){ includeEndDay = True } _MD scfg) <$> maybeS

        result  | isJust _IPCL && isJust _IPCED = Just $ (filter (\ss -> (calculationDay ss) < fromJust _IPCED) $ fromJust schedIP) ++ [shift scfg $ fromJust _IPCED]
                | otherwise = Nothing
    in result

_SCHED_RR_PAM scfg _IED _SD _RRANX _RRCL _RRNXT _MD =
    let maybeS  | isNothing _RRANX                 = Just $ _IED `plusCycle` fromJust _RRCL
                | otherwise                           = _RRANX

        tt      = (\s -> _S s (fromJust _RRCL){ includeEndDay = False } _MD scfg) <$> maybeS
        trry    = fromJust $ inf (fromJust tt) _SD

        result  | isNothing _RRANX && isNothing _RRCL = Nothing
                | isJust _RRNXT                       = remove trry <$> tt
                | otherwise                           = tt
    in result

_SCHED_RRF_PAM scfg _IED _RRANX _RRCL _MD =
    let maybeS  | isNothing _RRANX                    = Just $ _IED `plusCycle` fromJust _RRCL
                | otherwise                           = _RRANX

        tt      = (\s -> _S s (fromJust _RRCL) _MD scfg) <$> maybeS

        result  | isNothing _RRANX && isNothing _RRCL  = Nothing
                | otherwise                           = tt
    in result

_SCHED_SC_PAM scfg _IED _SCEF _SCANX _SCCL _MD =
    let maybeS  | isNothing _SCANX && isNothing _SCCL  = Nothing
                | isNothing _SCANX                    = Just $ _IED `plusCycle` fromJust _SCCL
                | otherwise                           = _SCANX

        tt      = (\s -> _S s (fromJust _SCCL){ includeEndDay = False } _MD scfg) <$> maybeS

        result  | _SCEF == SE_000                      = Nothing
                | otherwise                           = tt
    in result

-- - Linear Amortizer - --
_SCHED_IED_LAM = _SCHED_IED_PAM

_SCHED_PR_LAM scfg _PRCL _IED _PRANX _MD =
    let maybeS  | isNothing _PRANX && isNothing _PRCL = Nothing
                | isNothing _PRANX                   = Just $ _IED `plusCycle` fromJust _PRCL
                | otherwise                          = _PRANX
    in (\s -> _S s (fromJust _PRCL) _MD scfg ) <$> maybeS

_SCHED_MD_LAM scfg tmd = Just [shift scfg tmd]

_SCHED_PP_LAM = _SCHED_PP_PAM

_SCHED_PY_LAM = _SCHED_PY_PAM

-- To avoid constraint error, we can't fixpoint this
_SCHED_FP_LAM scfg _FER _FECL _IED _FEANX _MD = _SCHED_FP_PAM scfg _FER _FECL _IED _FEANX _MD

_SCHED_PRD_LAM = _SCHED_PRD_PAM

_SCHED_TD_LAM = _SCHED_TD_PAM

_SCHED_IP_LAM = _SCHED_IP_PAM

_SCHED_IPCI_LAM = _SCHED_IPCI_PAM

_SCHED_IPCB_LAM scfg _IED _IPCB _IPCBCL _IPCBANX _MD =
    let maybeS  | isNothing _IPCBANX && isNothing _IPCBCL = Nothing
                | isNothing _IPCBANX                   = Just $ _IED `plusCycle` fromJust _IPCBCL
                | otherwise                          = _IPCBANX

        result  | (fromJust _IPCB) /= IPCB_NTL                   = Nothing -- This means that IPCB != 'NTL', since there is no cycle
                | otherwise                          = (\s -> _S s (fromJust _IPCBCL) _MD scfg) <$> maybeS
    in result

_SCHED_RR_LAM = _SCHED_RR_PAM

_SCHED_RRF_LAM = _SCHED_RRF_PAM

_SCHED_SC_LAM = _SCHED_SC_PAM

-- Negative Amortizer
_SCHED_IED_NAM scfg _IED = _SCHED_IED_PAM scfg _IED

_SCHED_PR_NAM scfg _PRCL _IED _PRANX _MD = _SCHED_PR_LAM scfg _PRCL _IED _PRANX _MD

_SCHED_MD_NAM scfg tmd = _SCHED_MD_PAM scfg tmd

_SCHED_PP_NAM scfg _PREF _OPCL _IED _OPANX _MD = _SCHED_PP_PAM scfg _PREF _OPCL _IED _OPANX _MD

_SCHED_PY_NAM scfg _PYTP _PREF _OPCL _IED _OPANX _MD = _SCHED_PY_PAM scfg _PYTP _PREF _OPCL _IED _OPANX _MD

_SCHED_FP_NAM scfg _FER _FECL _IED _FEANX _MD = _SCHED_FP_PAM scfg _FER _FECL _IED _FEANX _MD

_SCHED_PRD_NAM scfg _PRD = _SCHED_PRD_PAM scfg _PRD

_SCHED_TD_NAM scfg _TD = _SCHED_TD_PAM scfg _TD

_SCHED_IP_NAM scfg _IED _PRCL _PRANX _IPCED _IPANX _IPCL _MD =
    let maybeS  | isNothing _PRANX = Just $ _IED `plusCycle` fromJust _PRCL
                | otherwise        = _PRANX

        _T      = fromJust maybeS `minusCycle` fromJust _PRCL

        r       | isJust _IPCED = _IPCED
                | isJust _IPANX = _IPANX
                | isJust _IPCL  = Just $ _IED `plusCycle` fromJust _IPCL
                | otherwise     = Nothing

        u       | isNothing _IPANX && isNothing _IPCL    = Nothing
                | isJust _IPCED && fromJust _IPCED >= _T = Nothing
                | otherwise                              = (\s -> _S s (fromJust _IPCL) _MD scfg) <$> r

        v       = (\s -> _S s (fromJust _PRCL) _MD scfg) <$> maybeS

        result  = Just ((fromMaybe [] u) ++ (fromMaybe [] v))
    in
        result


_SCHED_IPCI_NAM = _SCHED_IPCI_PAM

_SCHED_IPCB_NAM scfg _IED _IPCB _IPCBCL _IPCBANX _MD = _SCHED_IPCB_LAM scfg _IED _IPCB _IPCBCL _IPCBANX _MD

_SCHED_RR_NAM scfg _IED _SD _RRANX _RRCL _RRNXT _MD = _SCHED_RR_PAM scfg _IED _SD _RRANX _RRCL _RRNXT _MD

_SCHED_RRF_NAM scfg _IED _RRANX _RRCL _MD = _SCHED_RRF_PAM scfg _IED _RRANX _RRCL _MD

_SCHED_SC_NAM scfg _IED _SCEF _SCANX _SCCL _MD = _SCHED_SC_PAM scfg _IED _SCEF _SCANX _SCCL _MD
