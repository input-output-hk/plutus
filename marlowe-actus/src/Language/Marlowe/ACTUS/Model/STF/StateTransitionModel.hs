{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-record-wildcards #-}

module Language.Marlowe.ACTUS.Model.STF.StateTransitionModel where

import           Data.Maybe                                       (fromJust, fromMaybe, isJust, isNothing)
import           Language.Marlowe.ACTUS.Definitions.ContractState (ContractStatePoly (ContractStatePoly, fac, feac, ipac, ipcb, ipnr, isc, nsc, nt, prf, prnxt, sd, tmd))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms (FEB (FEB_N), IPCB (IPCB_NT),
                                                                   SCEF (SE_00M, SE_0N0, SE_0NM, SE_I00))
import           Language.Marlowe.ACTUS.Ops                       (ActusNum (..), ActusOps (_max, _min, _zero),
                                                                   DateOps (_lt), RoleSignOps (_r))
import           Prelude                                          hiding (Fractional, Num, (*), (+), (-), (/))

-- Principal at Maturity
_STF_AD_PAM st@ContractStatePoly{..} t y_sd_t = st {
    ipac = ipac + y_sd_t * ipnr * nt,
    sd = t
}

_STF_IED_PAM st@ContractStatePoly{..} t y_ipanx_t _IPNR _IPANX _CNTRL _IPAC _NT =
    let
        nt'                         = _r _CNTRL * _NT
        ipnr' | isNothing _IPNR     = _zero
              | otherwise           = fromJust _IPNR

        ipac' | isJust _IPAC        = fromJust _IPAC
              | isJust _IPANX       = _lt (fromJust _IPANX) t * y_ipanx_t * nt' * ipnr'
              | otherwise           = _zero
    in st { nt = nt', ipnr = ipnr', ipac = ipac', sd = t }

_STF_MD_PAM st@ContractStatePoly{..} t = st {
    nt = _zero,
    ipac = _zero,
    feac = _zero,
    sd = t
}

_STF_PP_PAM st@ContractStatePoly{..} t pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL =
    let st' = _STF_PY_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL
    in st' {nt = nt - pp_payoff}

_STF_PY_PAM st@ContractStatePoly{..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL =
    let
        ipac' = ipac + y_sd_t * ipnr * nt

        fac' = case _FEB of
            Just FEB_N -> fac + y_sd_t * nt * _FER
            _          -> (y_tfpminus_t / y_tfpminus_tfpplus) * _r _CNTRL * _FER

    in st {ipac = ipac', fac = fac', sd = t}

_STF_FP_PAM st@ContractStatePoly{..} t y_sd_t = st {
    ipac = ipac + y_sd_t * ipnr * nt,
    fac = _zero,
    sd = t
}

_STF_PRD_PAM st@ContractStatePoly{..} = _STF_PY_PAM st

_STF_TD_PAM st@ContractStatePoly{..} t = st {
    nt = _zero,
    ipac = _zero,
    fac = _zero,
    ipnr = _zero,
    sd = t
}

_STF_IP_PAM st@ContractStatePoly{..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL =
    let
        st' = _STF_PY_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL
    in st' {
        ipac = _zero
    }

_STF_IPCI_PAM st@ContractStatePoly{..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL =
    let
        st' = _STF_IP_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL
    in st' {
        nt = nt + ipac + y_sd_t * nt * ipnr
    }


_STF_RR_PAM st@ContractStatePoly{..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _RRLF _RRLC _RRPC _RRPF _RRMLT _RRSP o_rf_RRMO =
    let
        ipac' = ipac + y_sd_t * ipnr * nt
        fac'  =
          case _FEB of
            Just FEB_N -> fac + y_sd_t * nt * _FER
            _          -> (y_tfpminus_t / y_tfpminus_tfpplus) * _r _CNTRL * _FER

        st' = _STF_PRD_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL

        delta_r = (_min (_max (o_rf_RRMO * _RRMLT + _RRSP - ipnr) _RRPF) _RRPC)

        ipnr' = (_min (_max (ipnr + delta_r) _RRLF) _RRLC)
    in st' {ipac = ipac', fac = fac', ipnr = ipnr', sd = t}

_STF_RRF_PAM st@ContractStatePoly{..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _RRNXT =
    let
        st' = _STF_PRD_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL
    in st' {
        ipnr = fromMaybe _zero _RRNXT
    }

_STF_SC_PAM st@ContractStatePoly{..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _SCEF o_rf_SCMO _SCIED =
    let
        st' = _STF_PY_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL

        nsc' = case _SCEF of
            SE_00M -> nsc
            SE_I00 -> nsc
            _      -> (o_rf_SCMO - _SCIED) / _SCIED

        isc' = case _SCEF of
            SE_0N0 -> isc
            SE_00M -> isc
            SE_0NM -> isc
            _      -> (o_rf_SCMO - _SCIED) / _SCIED
    in st' {nsc = nsc', isc = isc'}

_STF_CE_PAM st@ContractStatePoly{..} = _STF_AD_PAM st

-- Linear Amortiser
_STF_AD_LAM st@ContractStatePoly{..} = _STF_AD_PAM st

_STF_IED_LAM st@ContractStatePoly{..} t y_ipanx_t _IPNR _IPANX _CNTRL _IPAC _NT _IPCB _IPCBA =
    let
        nt'                         = _r _CNTRL * _NT
        ipnr'                       = fromJust _IPNR

        ipac' | isJust _IPAC        = fromJust _IPAC
              | isJust _IPANX       = _lt (fromJust _IPANX) t * y_ipanx_t * nt' * ipnr'
              | otherwise           = _zero

        ipcb' | (fromJust _IPCB) == IPCB_NT = _r _CNTRL * _NT
              | otherwise                   = _r _CNTRL * (fromJust _IPCBA)
    in st { nt = nt', ipnr = ipnr', ipac = ipac', ipcb = ipcb', sd = t }

_STF_PR_LAM st@ContractStatePoly{..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _IPCB =
    let
        nt' = nt - _r _CNTRL * prnxt

        fac' = case _FEB of
            Just FEB_N -> fac + y_sd_t * nt * _FER
            _          -> (y_tfpminus_t / y_tfpminus_tfpplus) * _r _CNTRL * _FER

        ipcb' = case (fromJust _IPCB) of
            IPCB_NT -> nt'
            _       -> ipcb

    in st {nt = nt', fac = fac', ipcb = ipcb', sd = t}

_STF_MD_LAM st@ContractStatePoly{..} t = st {
    nt = _zero,
    ipac = _zero,
    feac = _zero,
    ipcb = _zero,
    sd = t
}

_STF_PP_LAM st@ContractStatePoly{..} t pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _IPCB =
    let
        st' = _STF_PY_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL
        nt' = nt - pp_payoff
        ipcb' = case (fromJust _IPCB) of
            IPCB_NT -> nt'
            _       -> ipcb
    in st' {nt = nt', ipcb = ipcb'}

_STF_PY_LAM st@ContractStatePoly{..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL =
    let
        ipac' = ipac + y_sd_t * ipnr * ipcb

        fac' = case _FEB of
            Just FEB_N -> fac + y_sd_t * nt * _FER
            _          -> (y_tfpminus_t / y_tfpminus_tfpplus) * _r _CNTRL * _FER

    in st {ipac = ipac', fac = fac', sd = t}

_STF_FP_LAM st@ContractStatePoly{..} t y_sd_t = st {
    ipac = ipac + y_sd_t * ipnr * ipcb,
    fac = _zero,
    sd = t
}
_STF_PRD_LAM st@ContractStatePoly{..} = _STF_PY_LAM st

_STF_TD_LAM st@ContractStatePoly{..} = _STF_TD_PAM st

_STF_IP_LAM st@ContractStatePoly{..} = _STF_IP_PAM st

_STF_IPCI_LAM st@ContractStatePoly{..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _IPCB =
    let
        st' = _STF_IP_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL
        nt' = nt + ipac + y_sd_t * ipnr * ipcb
        ipcb' = case (fromJust _IPCB) of
            IPCB_NT -> nt'
            _       -> ipcb
    in st' {nt = nt', ipcb = ipcb'}

_STF_IPCB_LAM st@ContractStatePoly{..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL =
    let
        st' = _STF_PRD_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL
    in st' { ipcb = nt }

_STF_RR_LAM st@ContractStatePoly{..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _RRLF _RRLC _RRPC _RRPF _RRMLT _RRSP o_rf_RRMO =
    let
        st' = _STF_PRD_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL

        delta_r = (_min (_max (o_rf_RRMO * _RRMLT + _RRSP - ipnr) _RRPF) _RRPC)

        ipnr' = (_min (_max (ipnr + delta_r) _RRLF) _RRLC)
    in st' {ipnr = ipnr'}

_STF_RRF_LAM st@ContractStatePoly{..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _RRNXT =
    let
        st' = _STF_PRD_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL
    in st' {
        ipnr = fromMaybe _zero _RRNXT
    }

_STF_SC_LAM st@ContractStatePoly{..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _SCEF o_rf_SCMO _SCIED =
    let
        st' = _STF_PY_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL

        nsc' = case _SCEF of
            SE_00M -> nsc
            SE_I00 -> nsc
            _      -> (o_rf_SCMO - _SCIED) / _SCIED

        isc' = case _SCEF of
            SE_0N0 -> isc
            SE_00M -> isc
            SE_0NM -> isc
            _      -> (o_rf_SCMO - _SCIED) / _SCIED
    in st' {nsc = nsc', isc = isc'}

_STF_CE_LAM st@ContractStatePoly{..} = _STF_AD_PAM st

-- Negative Amortizer
_STF_AD_NAM st t y_sd_t = _STF_AD_PAM st t y_sd_t

_STF_IED_NAM st t y_ipanx_t _IPNR _IPANX _CNTRL _IPAC _NT _IPCB _IPCBA =
    _STF_IED_LAM st t y_ipanx_t _IPNR _IPANX _CNTRL _IPAC _NT _IPCB _IPCBA

_STF_PR_NAM st@ContractStatePoly{..} t pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _IPCB =
    let
        st'@ContractStatePoly{ ipac = ipac' } =
          _STF_PP_LAM st t pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _IPCB

        nt' = nt - prnxt - ipac'
    in st' { nt = nt' }

_STF_MD_NAM st t = _STF_MD_LAM st t

_STF_PP_NAM st t pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _IPCB =
    _STF_PP_LAM st t pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _IPCB

_STF_PY_NAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL =
    _STF_PY_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL

_STF_FP_NAM st t y_sd_t = _STF_FP_LAM st t y_sd_t

_STF_PRD_NAM st = _STF_PRD_LAM st

_STF_TD_NAM st t = _STF_TD_PAM st t

_STF_IP_NAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL =
    _STF_IP_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL

_STF_IPCI_NAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _IPCB =
    _STF_IPCI_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _IPCB

_STF_IPCB_NAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL =
  _STF_IPCB_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL

_STF_RR_NAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _RRLF _RRLC _RRPC _RRPF _RRMLT _RRSP o_rf_RRMO =
    _STF_RR_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _RRLF _RRLC _RRPC _RRPF _RRMLT _RRSP o_rf_RRMO

_STF_RRF_NAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _RRNXT =
    _STF_RRF_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _RRNXT

_STF_SC_NAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _SCEF o_rf_SCMO _SCIED =
    _STF_SC_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus _FEB _FER _CNTRL _SCEF o_rf_SCMO _SCIED

_STF_CE_NAM st t y_sd_t = _STF_AD_PAM st t y_sd_t
