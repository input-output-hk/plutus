{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Marlowe.ACTUS.Model.INIT.StateInitializationModel where

import           Data.Maybe                                             (fromJust, fromMaybe, isJust, isNothing)
import           Language.Marlowe.ACTUS.Definitions.ContractState       (ContractStatePoly (ContractStatePoly, fac, feac, ipac, ipcb, ipnr, isc, nsc, nt, prf, prnxt, sd, tmd))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms       (ContractTerms (..), FEB (FEB_N),
                                                                         IPCB (IPCB_NT),
                                                                         SCEF (SE_0N0, SE_0NM, SE_I00, SE_I0M, SE_IN0, SE_INM),
                                                                         n)
import           Language.Marlowe.ACTUS.Model.Utility.ContractRoleSign  (contractRoleSign)
import           Language.Marlowe.ACTUS.Model.Utility.ScheduleGenerator (plusCycle)
import           Language.Marlowe.ACTUS.Model.Utility.YearFraction      (yearFraction)


r = contractRoleSign
y = yearFraction

scef_xNx SE_0N0 = True
scef_xNx SE_0NM = True
scef_xNx SE_IN0 = True
scef_xNx SE_INM = True
scef_xNx _      = False
scef_Ixx SE_IN0 = True
scef_Ixx SE_INM = True
scef_Ixx SE_I00 = True
scef_Ixx SE_I0M = True
scef_Ixx _      = False



_INIT_PAM t0 tminus tfp_minus tfp_plus
  ContractTerms{
      ct_CNTRL  = _CNTRL
    , ct_IED    = _IED
    , ct_DCC    = _DCC
    , ct_PRF    = _PRF
    , ct_FEAC   = _FEAC
    , ct_FEB    = _FEB
    , ct_FER    = _FER
    , ct_IPAC   = _IPAC
    , ct_IPNR   = _IPNR
    , ct_NT     = _NT
    , ct_MD     = _MD
    , ct_SCEF   = _SCEF
    , ct_SCCDD = _SCCDD
  } =
    let
        _IED'   = fromJust _IED
        _DCC'   = fromJust _DCC
        _PRF'   = fromJust _PRF
        _SCEF'  = fromJust _SCEF
        _SCCDD' = fromJust _SCCDD

        tmd                                     = fromJust _MD
        nt
                | _IED' > t0                    = 0.0
                | otherwise                     = r _CNTRL * fromJust _NT
        ipnr
                | _IED' > t0                    = 0.0
                | otherwise                     = fromMaybe 0.0 _IPNR
        ipac
                | isNothing _IPNR               = 0.0
                | isJust _IPAC                  = fromJust _IPAC
                | otherwise                     = (y _DCC' tminus t0 _MD) * nt * ipnr
        fac
                | isNothing _FER                = 0.0
                | isJust _FEAC                  = fromJust _FEAC
                | fromJust _FEB == FEB_N        = y _DCC' tfp_minus t0 _MD * nt * fromJust _FER
                | otherwise                     = (y _DCC' tfp_minus t0 _MD / y _DCC' tfp_minus tfp_plus _MD) * fromJust _FER
        feac
                | isNothing _FER                = 0.0
                | isJust _FEAC                  = fromJust _FEAC
                | fromJust _FEB == FEB_N        = y _DCC' tfp_minus t0 _MD * nt * fromJust _FER
                | otherwise                     = (y _DCC' tfp_minus t0 _MD / y _DCC' tfp_minus tfp_plus _MD) * fromJust _FER
        nsc
                | scef_xNx _SCEF'               = _SCCDD'
                | otherwise                     = 1.0
        isc
                | scef_Ixx _SCEF'               = _SCCDD'
                | otherwise                     = 1.0
        prf                                     = _PRF'
        sd                                      = t0
    in ContractStatePoly { prnxt = 0.0, ipcb = 0.0, tmd = tmd, nt = nt, ipnr = ipnr, ipac = ipac, fac = fac, feac = feac, nsc = nsc, isc = isc, prf = prf, sd = sd }

_INIT_LAM t0 tminus tpr_minus tfp_minus tfp_plus
  terms@ContractTerms{
      ct_CNTRL = _CNTRL
    , ct_IED   = _IED
    , ct_DCC   = _DCC
    , ct_IPCB  = _IPCB
    , ct_IPCBA = _IPCBA
    , ct_NT    = _NT
    , ct_MD    = _MD
    , ct_PRANX = _PRANX
    , ct_PRCL  = _PRCL
    , ct_PRNXT = _PRNXT
  } =
    let
        _IED' = fromJust _IED
        _DCC' = fromJust _DCC

        -- TMD
        maybeTMinus
                    | isJust _PRANX && ((fromJust _PRANX) >= t0) = _PRANX
                    | (_IED' `plusCycle` fromJust _PRCL) >= t0 = Just $ _IED' `plusCycle` fromJust _PRCL
                    | otherwise                           = Just tpr_minus
        tmd
                | isJust _MD = fromJust _MD
                | otherwise = fromJust maybeTMinus `plusCycle` (fromJust _PRCL) { n = ((ceiling ((fromJust _NT) / (fromJust _PRNXT))) * (n (fromJust _PRCL))) }

        pam_init = _INIT_PAM t0 tminus tfp_minus tfp_plus terms

        -- PRNXT
        s
                | isJust _PRANX && ((fromJust _PRANX) > t0) = fromJust _PRANX
                | isNothing _PRANX && ((_IED' `plusCycle` fromJust _PRCL) > t0) = _IED' `plusCycle` fromJust _PRCL
                | otherwise = tpr_minus
        prnxt
                | isJust _PRNXT                 = fromJust _PRNXT
                | otherwise                     = (fromJust _NT) * (1.0 / (fromIntegral $ ((ceiling (y _DCC' s tmd (Just tmd) / y _DCC' s (s `plusCycle` fromJust _PRCL) (Just tmd))) :: Integer)))

        -- IPCB
        ipcb
                | t0 < _IED'                     = 0.0
                | fromJust _IPCB == IPCB_NT     = r _CNTRL * fromJust _NT
                | otherwise                     = r _CNTRL * fromJust _IPCBA
    -- All is same as PAM except PRNXT, IPCB, and TMD
    in pam_init { prnxt = prnxt, ipcb = ipcb, tmd = tmd }

_INIT_NAM t0 tminus tpr_minus tfp_minus tfp_plus
  terms@ContractTerms{
      ct_CNTRL = _CNTRL
    , ct_IED   = _IED
    , ct_DCC   = _DCC
    , ct_IPCB  = _IPCB
    , ct_IPCBA = _IPCBA
    , ct_IPNR  = _IPNR
    , ct_NT    = _NT
    , ct_MD    = _MD
    , ct_PRANX = _PRANX
    , ct_PRCL  = _PRCL
    , ct_PRNXT = _PRNXT
  } =
    let
        _IED'   = fromJust _IED
        _DCC'   = fromJust _DCC
        _PRNXT' = fromJust _PRNXT

        -- TMD
        maybeTMinus
                    | isJust _PRANX && ((fromJust _PRANX) >= t0) = _PRANX
                    | (_IED' `plusCycle` fromJust _PRCL) >= t0 = Just $ _IED' `plusCycle` fromJust _PRCL
                    | otherwise                           = Just tpr_minus
        tmd
                | isJust _MD = fromJust _MD
                | otherwise = fromJust maybeTMinus `plusCycle` (fromJust _PRCL) { n = ceiling((fromJust _NT) / (_PRNXT' - (fromJust _NT)  * (y _DCC' tminus (tminus `plusCycle` fromJust _PRCL) _MD) * fromJust _IPNR))}

        pam_init = _INIT_PAM t0 tminus tfp_minus tfp_plus terms

        -- PRNXT
        prnxt = r _CNTRL * _PRNXT'

        -- IPCB
        ipcb
                | t0 < _IED'                    = 0.0
                | fromJust _IPCB == IPCB_NT     = r _CNTRL * fromJust _NT
                | otherwise                     = r _CNTRL * fromJust _IPCBA
    -- All is same as PAM except PRNXT and TMD, IPCB same as LAM
    in pam_init { prnxt = prnxt, ipcb = ipcb, tmd = tmd }
