{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

module Language.PlutusCore.Evaluation.Machine.ExMemory
( Plain
, WithMemory
, ExMemory(..)
, ExCPU(..)
, GenericExMemoryUsage(..)
, ExMemoryUsage(..)
, withMemory
) where

{- Note [Memory accounting]
Each operation costs a certain amount of memory. Plutus counts this usage via
ExMemory units, which correspond to machine words (64bit). First, the cost of
the initial AST is added to the budget. Then each operation requires a certain
amount of memory. Builtins may require different amounts of memory depending on
the input size. Memory cost is only counted on creation of a value, so passing
it around won't increase the cost, because sharing is assumed. If a computation
runs out of Memory, it is aborted.
-}

import           Language.PlutusCore
import           PlutusPrelude

import           Control.Monad.RWS.Strict
import           Foreign.Storable
import qualified Data.Text                     as T
import qualified Data.ByteString.Lazy          as BSL

import           GHC.Generics

type Plain f = f TyName Name ()
type WithMemory f = f TyName Name ExMemory

newtype ExMemory = ExMemory Integer -- Counts size in machine words (64bit for the near future)
  deriving (Eq, Ord, Show)
  deriving newtype Num
  deriving (Semigroup, Monoid) via (Sum Integer)
newtype ExCPU = ExCPU Integer
  deriving (Eq, Ord, Show)
  deriving newtype Num

-- Based on https://github.com/ekmett/semigroups/blob/master/src/Data/Semigroup/Generic.hs
class GExMemoryUsage f where
  gmemoryUsage' :: f a -> ExMemory

gmemoryUsage :: (Generic a, GExMemoryUsage (Rep a)) => a -> ExMemory
gmemoryUsage x = gmemoryUsage' (from x)

instance GExMemoryUsage U1 where
  gmemoryUsage' _ = 1 -- No constructor

instance GExMemoryUsage V1 where
  gmemoryUsage' _ = 1 -- Empty datatype

instance ExMemoryUsage a => GExMemoryUsage (K1 i a) where
  gmemoryUsage' (K1 x) = memoryUsage x

instance GExMemoryUsage f => GExMemoryUsage (M1 i c f) where
  gmemoryUsage' (M1 x) = gmemoryUsage' x

instance (GExMemoryUsage f, GExMemoryUsage g) => GExMemoryUsage (f :*: g) where
  gmemoryUsage' (x1 :*: x2) = gmemoryUsage' x1 + gmemoryUsage' x2

instance (GExMemoryUsage f, GExMemoryUsage g) => GExMemoryUsage (f :+: g) where
  -- TODO I think this is supposed to count the max instead (?)
  gmemoryUsage' (L1 x) = gmemoryUsage' x
  gmemoryUsage' (R1 x) = gmemoryUsage' x

newtype GenericExMemoryUsage a = GenericExMemoryUsage { getGenericExMemoryUsage :: a }
instance (Generic a, GExMemoryUsage (Rep a)) => ExMemoryUsage (GenericExMemoryUsage a) where
  memoryUsage (GenericExMemoryUsage x) = gmemoryUsage x

class ExMemoryUsage a where
    memoryUsage :: a -> ExMemory -- ^ How much memory does 'a' use?

deriving via (GenericExMemoryUsage (Constant ann)) instance ExMemoryUsage ann => ExMemoryUsage (Constant ann)
deriving via (GenericExMemoryUsage (Name ann)) instance ExMemoryUsage ann => ExMemoryUsage (Name ann)
deriving via (GenericExMemoryUsage TypeBuiltin) instance ExMemoryUsage TypeBuiltin
deriving via (GenericExMemoryUsage (Type TyName ann)) instance ExMemoryUsage ann => ExMemoryUsage (Type TyName ann)
deriving via (GenericExMemoryUsage (Builtin ann)) instance ExMemoryUsage ann => ExMemoryUsage (Builtin ann)
deriving via (GenericExMemoryUsage (Kind ann)) instance ExMemoryUsage ann => ExMemoryUsage (Kind ann)
deriving via (GenericExMemoryUsage BuiltinName) instance ExMemoryUsage BuiltinName
deriving via (GenericExMemoryUsage DynamicBuiltinName) instance ExMemoryUsage DynamicBuiltinName
deriving via (GenericExMemoryUsage (Plain Term)) instance ExMemoryUsage (Plain Term)
deriving via (GenericExMemoryUsage (WithMemory Term)) instance ExMemoryUsage (WithMemory Term)
deriving newtype instance ExMemoryUsage ann => ExMemoryUsage (TyName ann)
deriving newtype instance ExMemoryUsage ExMemory
deriving newtype instance ExMemoryUsage Unique

instance ExMemoryUsage () where
  memoryUsage _ = 0 -- TODO or 1?

instance ExMemoryUsage Integer where
  memoryUsage _ = 2 -- TODO

instance ExMemoryUsage BSL.ByteString where
  memoryUsage bsl = ExMemory $ toInteger $ BSL.length bsl

instance ExMemoryUsage T.Text where
  memoryUsage text = memoryUsage $ T.unpack text -- TODO not accurate, as Text uses UTF-16

instance ExMemoryUsage Int where
  memoryUsage _ = 1

instance ExMemoryUsage String where
  memoryUsage string = ExMemory $ toInteger $ sum $ fmap sizeOf string

withMemory :: ExMemoryUsage (f a) => Functor f => f a -> f ExMemory
withMemory x = fmap (const (memoryUsage x)) x
