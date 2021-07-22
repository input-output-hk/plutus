{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData  #-}
module Plutus.PAB.Events.Contract(
  -- $contract-events
  ContractInstanceId(..)
  , IterationID
  ) where

import           Plutus.Contract.Resumable (IterationID)

import           Wallet.Types              (ContractInstanceId (..))

-- $contract-events
-- The events that compiled Plutus contracts are concerned with. For each type
-- of event there is a request constructor in 'ContractRequest' and a response
-- constructor in 'ContractResponse'.

{- Note [Contract Events]

Each contract instance has two event queues in the PAB: One for the requests
it makes, typed 'ContractOutboxMessage', and one for the responses it receives,
typed 'ContractInboxMessage'.

-}

{- Note [Contract Iterations]

Contracts are executed iteratively:

* We look at the current state of the contract to see which requests (hooks)
  it wants to make
* We process one of those requests and feed the result to the contract
* The contract produces a new state, which we again inspect to get the hooks
  that are currently active

In each iteration we only feed one event to the contract, so if there is more
than one hook then the other ones will be un-answered.  If the contract had
been waiting for the first of several possible events, then the other hooks
will disappear from the set of active hooks. But if it had been waiting for
all of those events to happen then the remaining hooks will appear in the
result, and we handle them in the next iteration.

So each iteration begins with a set of requests from the contract and ends with
one of those requests being handled.

-}
