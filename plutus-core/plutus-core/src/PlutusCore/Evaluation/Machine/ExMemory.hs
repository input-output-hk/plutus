{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module PlutusCore.Evaluation.Machine.ExMemory
( CostingInteger
, ExMemory(..)
, ExCPU(..)
, GenericExMemoryUsage(..)
, ExMemoryUsage(..)
) where

import           PlutusCore.Core
import           PlutusCore.Data
import           PlutusCore.Name
import           PlutusCore.Pretty
import           PlutusPrelude

import           Control.Monad.RWS.Strict
import           Data.Aeson
import qualified Data.ByteString            as BS
import           Data.Proxy
import           Data.SatInt
import qualified Data.Text                  as T
import           GHC.Generics
import           GHC.Integer
import           GHC.Integer.Logarithms
import           GHC.Prim
import           Language.Haskell.TH.Syntax (Lift)
import           Universe

#include "MachDeps.h"

{- Note [Memory Usage for Plutus]

The base unit is 'ExMemory', which corresponds to machine words. For primitives,
we use static values for the size, see the corresponding instances. For
composite data types, the Generic instance is used, + 1 for the constructor tag.
For ADTs, the currently selected branch is counted, not the maximum value.
Memory usage of the annotation is not counted, because this should be
abstractly specifiable. It's an implementation detail.

-}

{- Note [Integer types for costing]
We care about the speed of our integer operations for costing, this has a significant effect on speed.
But we also need to care about overflow: the cost counters overflowing is a potential attack!

We have a few choices here for what to do with an overflow:
- Don't (this is what 'Integer' does, it's unbounded)
- Wrap (this is what 'Int'/'Int64' and friends do)
- Throw an overflow error (this is what 'Data.SafeInt' does)
- Saturate (i.e. return max/min bound, this is what 'Data.SatInt does)

In our case
- Not overflowing would be nice, but 'Integer' is significantly slower than the other types.
- Wrapping is quite dangerous, as it could lead to us getting attacked by someone wrapping
their cost around to something that looks below the budget.
- Throwing would be okay, but we'd have to worry about exception catching.
- Saturating is actually quite nice: we care about whether `a op b < budget`. So long as `budget < maxBound`,
  then `a op b < budget` will have the same truth value *regardless* of whether the operation overflows and saturates,
  since saturating implies `a op b >= maxBound > budget`. Plus, it means we don't need to deal with
  exceptions.

So we use 'Data.SatInt', a variant of 'Data.SafeInt' that does saturating arithmetic.

'SatInt' is quite fast, but not quite as fast as using 'Int64' directly (I don't know why that would be, apart from maybe
just the overflow checks), but the wrapping behaviour of 'Int64' is unacceptable..

One other wrinkle is that 'SatInt' is backed by an 'Int' (i.e. a machine integer
with platform-dependent size), rather than an 'Int64' since the primops that we
need are only available for 'Int' until GHC 9.2 or so. So on 32bit platforms, we
have much less headroom.

However we mostly care about 64bit platforms, so this isn't too much of a
problem. The only one where it could be a problem is GHCJS, which does present
as a 32bit platform. However, we won't care about *performance* on GHCJS, since
nobody will be running a node compiled to JS (and if they do, they deserve
terrible performance). So: if we are not on a 64bit platform, then we can just
fallback to the slower (but safe) 'Integer'.

-}

-- See Note [Integer types for costing]
-- See also Note [Budgeting units] in ExBudget.hs
type CostingInteger =
#if WORD_SIZE_IN_BITS < 64
    Integer
#else
    SatInt
#endif


-- $(if finiteBitSize (0::SatInt) < 64 then [t|Integer|] else [t|SatInt|])

-- | Counts size in machine words.
newtype ExMemory = ExMemory CostingInteger
  deriving (Eq, Ord, Show, Generic, Lift)
  deriving newtype (Num, NFData)
  deriving (Semigroup, Monoid) via (Sum CostingInteger)
  deriving (FromJSON, ToJSON) via CostingInteger
instance Pretty ExMemory where
    pretty (ExMemory i) = pretty (toInteger i)
instance PrettyBy config ExMemory where
    prettyBy _ m = pretty m

-- | Counts CPU units in picoseconds: maximum value for SatInt is 2^63 ps, or
-- appproximately 106 days.
newtype ExCPU = ExCPU CostingInteger
  deriving (Eq, Ord, Show, Generic, Lift)
  deriving newtype (Num, NFData)
  deriving (Semigroup, Monoid) via (Sum CostingInteger)
  deriving (FromJSON, ToJSON) via CostingInteger
instance Pretty ExCPU where
    pretty (ExCPU i) = pretty (toInteger i)
instance PrettyBy config ExCPU where
    prettyBy _ m = pretty m

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
  gmemoryUsage' (L1 x) = gmemoryUsage' x
  gmemoryUsage' (R1 x) = gmemoryUsage' x

newtype GenericExMemoryUsage a = GenericExMemoryUsage { getGenericExMemoryUsage :: a }
instance (Generic a, GExMemoryUsage (Rep a)) => ExMemoryUsage (GenericExMemoryUsage a) where
  memoryUsage (GenericExMemoryUsage x) = gmemoryUsage x

class ExMemoryUsage a where
    memoryUsage :: a -> ExMemory -- ^ How much memory does 'a' use?

deriving via (GenericExMemoryUsage (Either a b)) instance
    (ExMemoryUsage a, ExMemoryUsage b) => ExMemoryUsage (Either a b)
deriving via (GenericExMemoryUsage (a, b)) instance
    (ExMemoryUsage a, ExMemoryUsage b) => ExMemoryUsage (a, b)

deriving newtype instance ExMemoryUsage (BuiltinTag fun)
deriving via (GenericExMemoryUsage Name) instance ExMemoryUsage Name
deriving via (GenericExMemoryUsage (Type tyname uni ann)) instance
    (ExMemoryUsage tyname, ExMemoryUsage ann) => ExMemoryUsage (Type tyname uni ann)
deriving via (GenericExMemoryUsage (Kind ann)) instance ExMemoryUsage ann => ExMemoryUsage (Kind ann)
deriving via (GenericExMemoryUsage (Term tyname name uni fun ann)) instance
    ( ExMemoryUsage tyname, ExMemoryUsage name, ExMemoryUsage ann
    , Closed uni, uni `Everywhere` ExMemoryUsage, ExMemoryUsage fun
    ) => ExMemoryUsage (Term tyname name uni fun ann)
deriving newtype instance ExMemoryUsage TyName
instance ExMemoryUsage SatInt where
    memoryUsage n = memoryUsage (fromIntegral @SatInt @Int n)
deriving newtype instance ExMemoryUsage ExMemory
deriving newtype instance ExMemoryUsage Unique

-- See https://github.com/input-output-hk/plutus/issues/1861
instance ExMemoryUsage (SomeTypeIn uni) where
  memoryUsage _ = 1 -- TODO things like @list (list (list integer))@ take up a non-constant amount of space.

-- See https://github.com/input-output-hk/plutus/issues/1861
instance (Closed uni, uni `Everywhere` ExMemoryUsage) => ExMemoryUsage (Some (ValueOf uni)) where
  -- TODO this is just to match up with existing golden tests. We probably need to account for @uni@ as well.
  memoryUsage (Some (ValueOf uni x)) = bring (Proxy @ExMemoryUsage) uni (memoryUsage x)

instance ExMemoryUsage () where
  memoryUsage () = 1

instance ExMemoryUsage Integer where
  memoryUsage 0 = ExMemory 1  -- integerLog2# is unspecified for 0, but in practice returns -1
  memoryUsage i = ExMemory . fromIntegral $ (1 + smallInteger (integerLog2# (abs i) `quotInt#` integerToInt 64)) -- Assume 64bit size.

instance ExMemoryUsage BS.ByteString where
  memoryUsage bs = ExMemory . fromIntegral $ 1 + ((toInteger $ BS.length bs)-1) `quot` 8
-- We want things of length 0-8 to have size 1, 9-16 to have size 2, etc.
-- We use 'quot' to deal with the empty bytestring because 'div' would give -1.
-- Maybe we should just use 1 + (toInteger $ BS.length bs) `div` 8, which
-- would count one extra for things whose sizes are multiples of 8.

instance ExMemoryUsage T.Text where
  memoryUsage text = memoryUsage $ T.unpack text -- TODO not accurate, as Text uses UTF-16

instance ExMemoryUsage Int where
  memoryUsage _ = 1

instance ExMemoryUsage Char where
  memoryUsage _ = 1

instance ExMemoryUsage Bool where
  memoryUsage _ = 1

-- TODO: The generic instance will traverse the list every time, which is bad. We need some sensible
-- solution here in future.
instance ExMemoryUsage [a] where
  memoryUsage _ = 1

-- TODO; The generic instance will traverse the structure every time, which is bad. We need some sensible
-- solution here in future.
instance ExMemoryUsage Data where
  memoryUsage _ = 1
