{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-

Effects for running contract instances and for storing and loading their state.

-}
module Plutus.PAB.Effects.Contract(
    PABContract(..)
    , requests
    , ContractEffect(..)
    , exportSchema
    , initialState
    , updateContract
    -- * Storing and retrieving contract state
    , ContractStore(..)
    , putState
    , getState
    , getDefinition
    , getActiveContracts
    , putStartInstance
    , putStopInstance
    -- * Storing and retrieving definitions of contracts
    , ContractDefinition(..)
    , addDefinition
    , getDefinitions
    ) where

import           Control.Monad.Freer        (Eff, Member, send)
import           Data.Aeson                 (Value)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Proxy                 (Proxy (..))
import           Playground.Types           (FunctionSchema)
import           Plutus.Contract.Effects    (PABReq, PABResp)
import           Plutus.Contract.Resumable  (Request, Response)
import           Plutus.Contract.State      (ContractResponse)
import qualified Plutus.Contract.State      as C
import           Plutus.PAB.Webserver.Types (ContractActivationArgs)
import           Schema                     (FormSchema)
import           Wallet.Types               (ContractInstanceId)

-- | A class of contracts running in the PAB. The purpose of the type
--   parameter @contract@ is to allow for different ways of running
--   contracts, for example: A compiled executable running in a separate
--   process, or an "inline" contract that was compiled with the PAB and
--   runs in the same process.
--
--   The associated type families correspond to the type arguments needed
--   for the 'ContractRequest' and 'ContractResponse' types from
--   'Plutus.Contract.State'.
class PABContract contract where
    -- | Any data needed to identify the contract. For example, the location of the executable.
    type ContractDef contract

    -- | Contract state type
    type State contract

    -- | Extract the serialisable state from the contract instance state.
    serialisableState :: Proxy contract -> State contract -> ContractResponse Value Value Value PABReq

-- | The open requests of the contract instance.
requests :: forall contract. PABContract contract => State contract -> [Request PABReq]
requests = C.hooks . serialisableState (Proxy @contract)

-- | An effect for sending updates to contracts that implement @PABContract@
data ContractEffect t r where
    ExportSchema   :: PABContract t => ContractDef t -> ContractEffect t [FunctionSchema FormSchema] -- ^ The schema of the contract
    InitialState   :: PABContract t => ContractInstanceId -> ContractDef t -> ContractEffect t (State t) -- ^ The initial state of the contract's instance
    UpdateContract :: PABContract t => ContractInstanceId -> ContractDef t -> State t -> Response PABResp -> ContractEffect t (State t) -- ^ Send an update to the contract and return the new state.

-- | Get the schema of a contract given its definition.
exportSchema ::
    forall t effs.
    ( Member (ContractEffect t) effs
    , PABContract t
    )
    => ContractDef t
    -> Eff effs [FunctionSchema FormSchema]
exportSchema def =
    let command :: ContractEffect t [FunctionSchema FormSchema] = ExportSchema def
    in send command

-- | Get the initial state of a contract
initialState ::
    forall t effs.
    ( Member (ContractEffect t) effs
    , PABContract t
    )
    => ContractInstanceId
    -> ContractDef t
    -> Eff effs (State t)
initialState i def =
    let command :: ContractEffect t (State t) = InitialState i def
    in send command

-- | Send an update to the contract and return the new state.
updateContract ::
    forall t effs.
    ( Member (ContractEffect t) effs
    , PABContract t
    )
    => ContractInstanceId
    -> ContractDef t
    -> State t
    -> Response PABResp
    -> Eff effs (State t)
updateContract i def state request =
    let command :: ContractEffect t (State t) = UpdateContract i def state request
    in send command

-- | Storing and retrieving the state of a contract instance
data ContractStore t r where
    PutStartInstance :: ContractActivationArgs (ContractDef t) -> ContractInstanceId -> ContractStore t () -- ^ Record the starting of a new contract instance
    PutState :: ContractActivationArgs (ContractDef t) -> ContractInstanceId -> State t -> ContractStore t () -- ^ Record the updated state of the contract instance
    GetState :: ContractInstanceId -> ContractStore t (State t) -- ^ Retrieve the last recorded state of the contract instance
    PutStopInstance :: ContractInstanceId -> ContractStore t () -- ^ Record the fact that a contract instance has stopped
    GetActiveContracts :: ContractStore t (Map ContractInstanceId (ContractActivationArgs (ContractDef t))) -- ^ Get all active contracts with their activation args

putStartInstance ::
    forall t effs.
    ( Member (ContractStore t) effs
    )
    => ContractActivationArgs (ContractDef t)
    -> ContractInstanceId
    -> Eff effs ()
putStartInstance def i =
    let command :: ContractStore t () = PutStartInstance def i
    in send command

putStopInstance ::
    forall t effs.
    ( Member (ContractStore t) effs
    )
    => ContractInstanceId
    -> Eff effs ()
putStopInstance i =
    let command :: ContractStore t () = PutStopInstance i
    in send command

-- | Store the state of the contract instance
putState ::
    forall t effs.
    ( Member (ContractStore t) effs
    )
    => ContractActivationArgs (ContractDef t)
    -> ContractInstanceId
    -> State t
    -> Eff effs ()
putState def i state =
    let command :: ContractStore t () = PutState def i state
    in send command

-- | Load the state of the contract instance
getState ::
    forall t effs.
    ( Member (ContractStore t) effs
    )
    => ContractInstanceId
    -> Eff effs (State t)
getState i =
    let command :: ContractStore t (State t) = GetState i
    in send command

-- | All active contracts with their definitions
getActiveContracts ::
    forall t effs.
    ( Member (ContractStore t) effs
    )
    => Eff effs (Map ContractInstanceId (ContractActivationArgs (ContractDef t)))
getActiveContracts =
    let command :: ContractStore t (Map ContractInstanceId (ContractActivationArgs (ContractDef t))) = GetActiveContracts
    in send command

-- | Get the definition of a running contract
getDefinition ::
    forall t effs.
    ( Member (ContractStore t) effs)
    => ContractInstanceId
    -> Eff effs (Maybe (ContractActivationArgs (ContractDef t)))
getDefinition i = Map.lookup i <$> (getActiveContracts @t)

-- | Storing and retrieving definitions of contracts.
--   (Not all 't's support this)
data ContractDefinition t r where
    AddDefinition :: ContractDef t -> ContractDefinition t ()
    GetDefinitions :: ContractDefinition t [ContractDef t]

addDefinition ::
    forall t effs.
    ( Member (ContractDefinition t) effs
    )
    => ContractDef t
    -> Eff effs ()
addDefinition def =
    let command :: ContractDefinition t ()
        command = AddDefinition def
    in send command

getDefinitions ::
    forall t effs.
    ( Member (ContractDefinition t) effs
    )
    => Eff effs [ContractDef t]
getDefinitions = send @(ContractDefinition t) GetDefinitions
