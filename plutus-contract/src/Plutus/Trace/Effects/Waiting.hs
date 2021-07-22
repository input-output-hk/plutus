{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Waiting for things to happen
module Plutus.Trace.Effects.Waiting(
    Waiting(..)
    , waitUntilSlot
    , waitUntilTime
    , nextSlot
    , waitNSlots
    , waitNMilliSeconds
    , handleWaiting
    ) where

import           Control.Monad.Freer           (Eff, Member, type (~>))
import           Control.Monad.Freer.Coroutine (Yield)
import           Control.Monad.Freer.TH        (makeEffect)
import           Data.Default                  (Default (def))
import           Ledger.Slot                   (Slot)
import           Ledger.Time                   (DiffMilliSeconds, POSIXTime, fromMilliSeconds)
import qualified Ledger.TimeSlot               as TimeSlot
import           Numeric.Natural               (Natural)
import           Plutus.Trace.Emulator.Types   (EmulatorMessage (NewSlot))
import           Plutus.Trace.Scheduler        (EmSystemCall, Priority (Sleeping), sleep)

data Waiting r where
    WaitUntilSlot :: Slot -> Waiting Slot

makeEffect ''Waiting

-- | Wait until the slot where the given time falls into and return latest time
-- we know has passed.
waitUntilTime :: Member Waiting effs => POSIXTime -> Eff effs POSIXTime
waitUntilTime time = do
  slot <- waitUntilSlot (TimeSlot.posixTimeToEnclosingSlot def time)
  return $ TimeSlot.slotToEndPOSIXTime def slot

-- | Wait until the beginning of the next slot, returning
--   the new slot number.
nextSlot :: Member Waiting effs => Eff effs Slot
nextSlot = waitUntilSlot 0

-- | Wait for a number of slots
waitNSlots ::
    forall effs.
    ( Member Waiting effs )
    => Natural
    -> Eff effs Slot
waitNSlots n
    | n > 1 = nextSlot >> waitNSlots (n - 1)
    | otherwise = nextSlot

-- | Convert the given 'n' milliseconds to a number of slots to wait.
--
-- Note: Currently, if n < length of a slot, then 'waitNMilliSeconds' has no
-- effect.
waitNMilliSeconds ::
    forall effs.
    ( Member Waiting effs )
    => DiffMilliSeconds
    -> Eff effs Slot
waitNMilliSeconds n =
    waitNSlots (fromIntegral $ TimeSlot.posixTimeToEnclosingSlot def $ fromMilliSeconds n)

handleWaiting ::
    forall effs effs2.
    ( Member (Yield (EmSystemCall effs2 EmulatorMessage) (Maybe EmulatorMessage)) effs
    )
    => Waiting
    ~> Eff effs
handleWaiting = \case
    WaitUntilSlot s -> go where
        go = sleep @effs2 Sleeping >>= \case { Just (NewSlot _ sl) | sl >= s -> pure sl; _ -> go }
