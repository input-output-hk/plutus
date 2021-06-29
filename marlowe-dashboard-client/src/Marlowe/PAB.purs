module Marlowe.PAB
  ( PlutusApp(..)
  , transactionFee
  , contractCreationFee
  , PlutusAppId(..)
  , MarloweParams(..)
  , ValidatorHash
  , MarloweData(..)
  , ContractHistory(..)
  , CombinedWSStreamToServer(..)
  ) where

import Prelude
import API.Contract (class ContractActivationId, defaultActivateContract, defaultDeactivateContract, defaultGetContractInstanceClientState, defaultInvokeEndpoint, defaultGetWalletContractInstances, defaultGetAllContractInstances, defaultGetContractDefinitions)
import Data.BigInteger (BigInteger, fromInt)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.UUID (UUID)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Marlowe.Semantics (Contract, State, TransactionInput)
import Plutus.V1.Ledger.Value (CurrencySymbol)
import Wallet.Emulator.Wallet (Wallet) as Back
import Wallet.Types (ContractInstanceId) as Back

{-
Marlowe requires three Plutus "contracts" to run in the PAB, which we refer to here as "apps" so as
to avoid confusion with *Marlowe* contracts:

1. The `MarloweApp`. This is used to:
   - "create" Marlowe contracts (i.e. to install them on the blockchain and distribute the role tokens)
   - "apply-inputs" to a Marlowe contract (i.e. perform an action to move that contract forward)
   - "redeem" payments made to a role token
2. The `WalletCompanion`. Every wallet has one instance of this app installed in the PAB. It listens
   continuously for payments of role tokens to that wallet, and updates its observable state with an
   array of `(MarloweParams, MarloweData)`, one pair for each role token, hence one for each Marlowe
   contract for which this wallet has a role.
3. The `MarloweFollower`. Every wallet has zero or more istances of this app installed in the PAB. We
   use this to "follow" a Marlowe contract. Once a contract has been "created", and we have its
   `MarloweParams`, we use an instance of the `MarloweFollower` to track its history and status. Once
   we have called the "follow" endpoint of this app (passing it the `MarloweParams`), its observable
   state will tell us everything we need to know about the contract, and will be updated when it changes.

Note: to use this type directly in the `marlowe-pab` (the PAB bundled with the Marlowe apps), these
names need to match those in `marlowe/pab/Main.hs`. To use the ContractExe type with the paths to the
executables (with the PAB *without* the Marlowe apps bundled), we can call them whatever we like.
-}
data PlutusApp
  = MarloweApp
  | WalletCompanion
  | MarloweFollower

derive instance eqPlutusApp :: Eq PlutusApp

derive instance genericPlutusApp :: Generic PlutusApp _

instance encodePlutusApp :: Encode PlutusApp where
  encode value = genericEncode defaultOptions value

instance decodePlutusApp :: Decode PlutusApp where
  decode value = genericDecode defaultOptions value

instance plutusAppContractActivationId :: ContractActivationId PlutusApp where
  activateContract = defaultActivateContract
  deactivateContract = defaultDeactivateContract
  getContractInstanceClientState = defaultGetContractInstanceClientState
  invokeEndpoint = defaultInvokeEndpoint
  getWalletContractInstances = defaultGetWalletContractInstances
  getAllContractInstances = defaultGetAllContractInstances
  getContractDefinitions = defaultGetContractDefinitions

-- In the PAB, transactions have a fixed cost of 10 lovelace; in the real node,transaction fees will vary,
-- but this may still serve as a good approximation
transactionFee :: BigInteger
transactionFee = fromInt 10

-- FIXME: it appears that creating a contract currently requires three transactions, but I believe it
-- should be possible with one; check this with Alex
contractCreationFee :: BigInteger
contractCreationFee = transactionFee * (fromInt 3)

{-
A `PlutusAppId` is used to identify an instance of a Plutus "contract" in the PAB. In the PAB code it is
called `ContractInstanceId` - as above, we don't refer to "contracts" here so as to avoid confusion with
*Marlowe* contracts. This is converted to a `ContractInstanceId` that the PAB understands by the `Bridge`
module.
-}
newtype PlutusAppId
  = PlutusAppId UUID

derive instance newtypePlutusAppId :: Newtype PlutusAppId _

derive instance eqPlutusAppId :: Eq PlutusAppId

derive instance ordPlutusAppId :: Ord PlutusAppId

derive instance genericPlutusAppId :: Generic PlutusAppId _

-- note we need to encode this type, not to communicate with the PAB (we have the `ContractInstanceId`
-- for that), but to save `WalletData` to local storage
instance encodePlutusAppId :: Encode PlutusAppId where
  encode value = genericEncode defaultOptions value

instance decodePlutusAppId :: Decode PlutusAppId where
  decode value = genericDecode defaultOptions value

-- `MarloweParams` are used to identify a Marlowe contract in the PAB, and the wallet
-- companion contract state is a `Map MarloweParams MarloweData`. We are not currently
-- generating PureScript types to match the Haskell `MarloweParams` and `MarloweData`
-- types (side note: perhaps we should be). We have a `CurrencySymbol` type in
-- `Marlowe.Semantics`, but it is just an alias for String. We could use that here for
-- the `rolesCurrency` value, and convert using its Bridge instance when sharing data
-- with the PAB. However, we currently don't need to do anything with `MarloweParams` on
-- the frontend except save them and send them back to the PAB (to join a contract that
-- is already running), so it is simpler just to use the generated type in this case.
type MarloweParams
  = { rolePayoutValidatorHash :: ValidatorHash
    , rolesCurrency :: CurrencySymbol
    }

type ValidatorHash
  = String

type MarloweData
  = { marloweContract :: Contract
    , marloweState :: State
    }

-- This is the observable state of the `MarloweFollower`. The `MarloweParams` identify the
-- the Marlowe contract on the blockchain, the `MarloweData` represents the initial contract
-- and state, and the array of `TransactionInput`s records all the transactions of the
-- contract so far.  The value is `None` when the app is first activated, before the "follow"
-- endpoint has been called (and the PAB has had time to settle).
type ContractHistory
  = { chParams :: Maybe (Tuple MarloweParams MarloweData)
    , chHistory :: Array TransactionInput
    }

-- HACK: rolling my own websocket type to see if this helps
data CombinedWSStreamToServer
  = Subscribe (Either Back.ContractInstanceId Back.Wallet)
  | Unsubscribe (Either Back.ContractInstanceId Back.Wallet)

derive instance genericCombinedWSStreamToServer :: Generic CombinedWSStreamToServer _

instance encodeCombinedWSStreamToServer :: Encode CombinedWSStreamToServer where
  encode value = genericEncode defaultOptions value

instance decodeCombinedWSStreamToServer :: Decode CombinedWSStreamToServer where
  decode value = genericDecode defaultOptions value
