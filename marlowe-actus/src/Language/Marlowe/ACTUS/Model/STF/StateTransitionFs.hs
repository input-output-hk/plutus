{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.STF.StateTransitionFs (stateTransitionFs) where

import           Data.Time                                              (Day)

import           Language.Marlowe.ACTUS.Definitions.BusinessEvents      (EventType (..))

import           Data.Maybe                                             (fromJust, fromMaybe)
import           Language.Marlowe.ACTUS.Definitions.ContractTerms       (CT (LAM, NAM, PAM), ContractTerms (..))
import           Language.Marlowe.ACTUS.Definitions.Schedule            (ShiftedDay (calculationDay))
import           Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule    (schedule)
import           Language.Marlowe.ACTUS.Model.STF.StateTransitionModel
import           Language.Marlowe.ACTUS.Model.Utility.ScheduleGenerator (inf, sup)
import           Language.Marlowe.ACTUS.Ops                             (YearFractionOps (_y))

import           Language.Marlowe                                       (Contract)
import           Language.Marlowe.ACTUS.MarloweCompat                   (constnt, enum, letval, marloweDate,
                                                                         stateTransitionMarlowe, useval)


stateTransitionFs :: EventType -> ContractTerms -> Integer -> Day -> Day -> Contract -> Contract
stateTransitionFs ev terms@ContractTerms{..} t prevDate curDate continue =
    let
        -- value wrappers:
        __IPANX = marloweDate <$> ct_IPANX
        __IPNR  = constnt <$> ct_IPNR
        __IPAC  = constnt <$> ct_IPAC
        __NT    = constnt (fromJust ct_NT)
        __FEB   = enum (ct_FEB)
        __FER   = constnt (fromJust ct_FER)
        __IPCB  = enum <$> ct_IPCB
        __IPCBA = constnt <$> ct_IPCBA
        (__RRLF, __RRLC, __RRPC, __RRPF, __RRMLT, __RRSP) =
                ( constnt (fromJust ct_RRLF)
                , constnt (fromJust ct_RRLC)
                , constnt (fromJust ct_RRPC)
                , constnt (fromJust ct_RRPF)
                , constnt (fromJust ct_RRMLT)
                , constnt (fromJust ct_RRSP)
                )
        __RRNXT            = constnt <$> ct_RRNXT
        __SCIED            = constnt (fromJust ct_SCIED)
        __o_rf_RRMO        = useval "o_rf_RRMO" t
        __o_rf_SCMO        = useval "o_rf_SCMO" t
        __pp_payoff        = useval "pp_payoff" t

        -- dates:
        time               = marloweDate $ curDate
        fpSchedule         = schedule FP terms
        tfp_minus          = fromMaybe curDate $ calculationDay <$> ((\sc -> sup sc curDate) =<< fpSchedule)
        tfp_plus           = fromMaybe curDate $ calculationDay <$> ((\sc -> inf sc curDate) =<< fpSchedule)
        y_tfpminus_t       = constnt $ _y (fromJust ct_DCC) tfp_minus curDate ct_MD
        y_tfpminus_tfpplus = constnt $ _y (fromJust ct_DCC) tfp_minus tfp_plus ct_MD
        y_ipanx_t          = constnt $ _y (fromJust ct_DCC) (fromJust ct_IPANX) curDate ct_MD
        y_sd_t             = constnt $ _y (fromJust ct_DCC) prevDate curDate ct_MD

        addComment cont    = case ev of
            IED -> letval "IED" t (constnt 0) cont
            MD  -> letval "MD" t (constnt 0) cont
            IP  -> letval ("IP:" ++ (show curDate) ++ (show prevDate)) t (constnt 0) cont
            RR  -> letval ("RR:" ++ (show curDate)) t (constnt 0) cont
            FP  -> letval ("FP:" ++ (show curDate)) t (constnt 0) cont
            _   -> cont
    in case contractType of
        PAM ->
            addComment $ stateTransitionMarlowe ev t continue $ \event st ->
                case event of
                    AD   -> _STF_AD_PAM st time y_sd_t
                    IED  -> _STF_IED_PAM st time y_ipanx_t __IPNR __IPANX ct_CNTRL __IPAC __NT
                    MD   -> _STF_MD_PAM st time
                    PP   -> _STF_PP_PAM st time __pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL
                    PY   -> _STF_PY_PAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL
                    FP   -> _STF_FP_PAM st time y_sd_t
                    PRD  -> _STF_PRD_PAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL
                    TD   -> _STF_TD_PAM st time
                    IP   -> _STF_IP_PAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL
                    IPCI -> _STF_IPCI_PAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL
                    RR   -> _STF_RR_PAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL __RRLF __RRLC __RRPC __RRPF __RRMLT __RRSP __o_rf_RRMO
                    RRF  -> _STF_RRF_PAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL __RRNXT
                    SC   -> _STF_SC_PAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL (fromJust ct_SCEF) __o_rf_SCMO __SCIED
                    CE   -> _STF_CE_PAM st time y_sd_t
                    _    -> st
        LAM ->
            addComment $ stateTransitionMarlowe ev t continue $ \event st ->
                case event of
                    AD   -> _STF_AD_LAM st time y_sd_t
                    IED  -> _STF_IED_LAM st time y_ipanx_t __IPNR __IPANX ct_CNTRL __IPAC __NT __IPCB __IPCBA
                    PR   -> _STF_PR_LAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL __IPCB
                    MD   -> _STF_MD_LAM st time
                    PP   -> _STF_PP_LAM st time __pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL __IPCB
                    PY   -> _STF_PY_LAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL
                    FP   -> _STF_FP_LAM st time y_sd_t
                    PRD  -> _STF_PRD_LAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL
                    TD   -> _STF_TD_LAM st time
                    IP   -> _STF_IP_LAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL
                    IPCI -> _STF_IPCI_LAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL __IPCB
                    IPCB -> _STF_IPCB_LAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL
                    RR   -> _STF_RR_LAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL __RRLF __RRLC __RRPC __RRPF __RRMLT __RRSP __o_rf_RRMO
                    RRF  -> _STF_RRF_LAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL __RRNXT
                    SC   -> _STF_SC_LAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL (fromJust ct_SCEF) __o_rf_SCMO __SCIED
                    CE   -> _STF_CE_LAM st time y_sd_t
                    _    -> st
        NAM ->
            addComment $ stateTransitionMarlowe ev t continue $ \event st ->
                case event of
                    AD   -> _STF_AD_NAM st time y_sd_t
                    IED  -> _STF_IED_NAM st time y_ipanx_t __IPNR __IPANX ct_CNTRL __IPAC __NT __IPCB __IPCBA
                    PR   -> _STF_PR_NAM st time __pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL __IPCB
                    MD   -> _STF_MD_NAM st time
                    PP   -> _STF_PP_NAM st time __pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL __IPCB
                    PY   -> _STF_PY_NAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL
                    FP   -> _STF_FP_NAM st time y_sd_t
                    PRD  -> _STF_PRD_NAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL
                    TD   -> _STF_TD_NAM st time
                    IP   -> _STF_IP_NAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL
                    IPCI -> _STF_IPCI_NAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL __IPCB
                    IPCB -> _STF_IPCB_NAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL
                    RR   -> _STF_RR_NAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL __RRLF __RRLC __RRPC __RRPF __RRMLT __RRSP __o_rf_RRMO
                    RRF  -> _STF_RRF_NAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL __RRNXT
                    SC   -> _STF_SC_NAM st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus __FEB __FER ct_CNTRL (fromJust ct_SCEF) __o_rf_SCMO __SCIED
                    CE   -> _STF_AD_NAM st time y_sd_t
                    _    -> st
