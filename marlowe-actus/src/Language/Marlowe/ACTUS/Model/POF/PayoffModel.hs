{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Marlowe.ACTUS.Model.POF.PayoffModel where

import           Language.Marlowe.ACTUS.Definitions.ContractTerms (FEB (FEB_A, FEB_N),
                                                                   PYTP (PYTP_A, PYTP_I, PYTP_N, PYTP_O))
import           Language.Marlowe.ACTUS.Ops                       (ActusNum (..), ActusOps (_max, _zero),
                                                                   RoleSignOps (_r))
import           Prelude                                          hiding (Fractional, Num, (*), (+), (-), (/))


-- Principal at Maturity
_POF_IED_PAM o_rf_CURS _CNTRL _NT _PDIED =
    _zero - o_rf_CURS * _r _CNTRL * (_NT + _PDIED)

_POF_MD_PAM o_rf_CURS nsc nt isct ipac feac =
    o_rf_CURS * (nsc * nt + isct * ipac + feac)

_POF_PP_PAM o_rf_CURS pp_payoff = o_rf_CURS * pp_payoff

_POF_PY_PAM _PYTP o_rf_CURS o_rf_RRMO _PYRT _cPYRT _CNTRL nt ipnr y_sd_t =
    case _PYTP of
        PYTP_A -> o_rf_CURS * _r _CNTRL * _PYRT
        PYTP_N -> _cPYRT
        PYTP_I ->
            let c = o_rf_CURS * _r _CNTRL * y_sd_t * nt
            in  c * _max _zero (ipnr - o_rf_RRMO)
        PYTP_O -> undefined --todo ask Neils

_POF_FP_PAM _FEB _FER o_rf_CURS _CNTRL nt fac y_sd_t =
    let c = o_rf_CURS * _FER
    in  case _FEB of
            FEB_A -> _r _CNTRL * c
            FEB_N -> c * y_sd_t * nt * fac

_POF_PRD_PAM o_rf_CURS _CNTRL _PPRD ipac ipnr nt y_sd_t =
    _zero - o_rf_CURS * _r _CNTRL * (_PPRD + ipac + y_sd_t * ipnr * nt)

_POF_TD_PAM o_rf_CURS _CNTRL _PTD ipac ipnr nt y_sd_t =
    o_rf_CURS * _r _CNTRL * (_PTD + ipac + y_sd_t * ipnr * nt)

_POF_IP_PAM o_rf_CURS isc ipac ipnr nt y_sd_t =
    o_rf_CURS * isc * (ipac + y_sd_t * ipnr * nt)


-- Linear Amortizer
_POF_IED_LAM o_rf_CURS _CNTRL _NT _PDIED = _POF_IED_PAM o_rf_CURS _CNTRL _NT _PDIED

_POF_PR_LAM o_rf_CURS _CNTRL nsc prnxt =
    o_rf_CURS * _r _CNTRL * nsc * prnxt

_POF_MD_LAM o_rf_CURS nsc nt isct ipac feac = _POF_MD_PAM o_rf_CURS nsc nt isct ipac feac

_POF_PP_LAM o_rf_CURS pp_payoff = _POF_PP_PAM o_rf_CURS pp_payoff

_POF_PY_LAM _PYTP o_rf_CURS o_rf_RRMO _PYRT _cPYRT _CNTRL nt ipnr y_sd_t = _POF_PY_PAM _PYTP o_rf_CURS o_rf_RRMO _PYRT _cPYRT _CNTRL nt ipnr y_sd_t

_POF_FP_LAM _FEB _FER o_rf_CURS _CNTRL nt fac y_sd_t = _POF_FP_PAM _FEB _FER o_rf_CURS _CNTRL nt fac y_sd_t

_POF_PRD_LAM o_rf_CURS _CNTRL _PPRD ipac ipnr ipcb y_sd_t =
    _zero - o_rf_CURS * _r _CNTRL * (_PPRD + ipac + y_sd_t * ipnr * ipcb)

_POF_TD_LAM o_rf_CURS _CNTRL _PTD ipac ipnr ipcb y_sd_t =
    o_rf_CURS * _r _CNTRL * (_PTD + ipac + y_sd_t * ipnr * ipcb)

_POF_IP_LAM o_rf_CURS isc ipac ipnr ipcb y_sd_t =
    o_rf_CURS * isc * (ipac + y_sd_t * ipnr * ipcb)


-- Negative Amortizer
_POF_IED_NAM o_rf_CURS _CNTRL _NT _PDIED = _POF_IED_PAM o_rf_CURS _CNTRL _NT _PDIED

_POF_PR_NAM o_rf_CURS nsc prnxt ipac y_sd_t ipnr ipcb =
    o_rf_CURS * nsc * (prnxt - ipac - y_sd_t * ipnr * ipcb)

_POF_MD_NAM o_rf_CURS nsc nt isct ipac feac = _POF_MD_PAM o_rf_CURS nsc nt isct ipac feac

_POF_PP_NAM o_rf_CURS pp_payoff = _POF_PP_PAM o_rf_CURS pp_payoff

_POF_PY_NAM _PYTP o_rf_CURS o_rf_RRMO _PYRT _cPYRT _CNTRL nt ipnr y_sd_t =
    _POF_PY_PAM _PYTP o_rf_CURS o_rf_RRMO _PYRT _cPYRT _CNTRL nt ipnr y_sd_t

_POF_FP_NAM _FEB _FER o_rf_CURS _CNTRL nt fac y_sd_t = _POF_FP_PAM _FEB _FER o_rf_CURS _CNTRL nt fac y_sd_t

_POF_PRD_NAM o_rf_CURS _CNTRL _PPRD ipac ipnr ipcb y_sd_t = _POF_PRD_LAM o_rf_CURS _CNTRL _PPRD ipac ipnr ipcb y_sd_t

_POF_TD_NAM o_rf_CURS _CNTRL _PTD ipac ipnr ipcb y_sd_t = _POF_TD_LAM o_rf_CURS _CNTRL _PTD ipac ipnr ipcb y_sd_t

_POF_IP_NAM o_rf_CURS isc ipac ipnr ipcb y_sd_t = _POF_IP_LAM o_rf_CURS isc ipac ipnr ipcb y_sd_t
