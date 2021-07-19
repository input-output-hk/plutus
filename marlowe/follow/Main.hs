{-# LANGUAGE TypeApplications #-}
module Main where

import           Data.Bifunctor          (first)
import           Data.Proxy              (Proxy (..))
import           Data.Text.Extras        (tshow)
import           Language.Marlowe.Client (MarloweFollowSchema, marloweFollowContract)
import           Plutus.Contract         (getWaited)
import           Plutus.PAB.ContractCLI  (commandLineApp')

main :: IO ()
main =
    commandLineApp'
        (Proxy @MarloweFollowSchema) -- see note ['ToSchema' and Marlowe]
        $ first tshow
        $ fmap getWaited
        $ marloweFollowContract

{- Note ['ToSchema' and Marlowe]

The PAB can automatically generate input forms for endpoints as long as their
types have instances of the 'Schema.ToSchema' class.

We can't derive instances of this class for union types such as
'Language.Marlowe.Semantics.Party' (see SCP-1162). Unfortunately this makes the
PAB UI unusable for most of the endpoints of the Marlowe contract.

This doesn't mean that Marlowe can't run on the PAB. It just means that there
is no schema for its endpoints, so we can't call them manually via the UI. We'd
have to call them programmatically with a call to the PAB's API.

-}
