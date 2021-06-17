{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans       #-}
module PlutusIR.Core.Instance.Flat () where

import           PlutusIR.Core.Type

import qualified PlutusCore         as PLC
import           PlutusCore.Flat    ()

import           Flat               (Flat)

{- Note: [Serialization of PIR]
The serialized version of Plutus-IR will be included in  the final
executable for helping debugging and testing and providing better error
reporting. It is not meant to be stored on the chain, which means that
the underlying representation can vary. The `Generic` instances of the
terms can thus be used as backwards compatibility is not required.
-}

instance ( PLC.Closed uni
         , uni `PLC.Everywhere` Flat
         , Flat a
         , Flat tyname
         , Flat name
         -- This was needed only for the Flat instance
         , Flat fun
         ) => Flat (Datatype tyname name uni fun a)

instance Flat Recursivity

instance Flat Strictness

instance ( PLC.Closed uni
         , uni `PLC.Everywhere` Flat
         , Flat fun
         , Flat a
         , Flat tyname
         , Flat name
         ) => Flat (Binding tyname name uni fun a)

instance ( PLC.Closed uni
         , uni `PLC.Everywhere` Flat
         , Flat fun
         , Flat a
         , Flat tyname
         , Flat name
         ) => Flat (Term tyname name uni fun a)

instance ( PLC.Closed uni
         , uni `PLC.Everywhere` Flat
         , Flat fun
         , Flat a
         , Flat tyname
         , Flat name
         ) => Flat (Program tyname name uni fun a)
