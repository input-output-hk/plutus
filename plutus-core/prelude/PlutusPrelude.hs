{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module PlutusPrelude
    ( -- * Reexports from base
      (&)
    , (&&&)
    , (<&>)
    , toList
    , bool
    , first
    , second
    , on
    , isNothing
    , isJust
    , fromMaybe
    , guard
    , foldl'
    , fold
    , for
    , throw
    , join
    , (<=<)
    , (>=>)
    , ($>)
    , fromRight
    , isRight
    , void
    , through
    , coerce
    , Generic
    , NFData
    , Natural
    , NonEmpty (..)
    , Word8
    , Alternative (..)
    , Exception
    , PairT (..)
    , Coercible
    , Typeable
    -- * Lens
    , Lens'
    , lens
    , (^.)
    , view
    , (.~)
    , set
    , (%~)
    , over
    -- * Debugging
    , traceShowId
    , trace
    -- * Reexports from "Control.Composition"
    , (.*)
    -- * Custom functions
    , (<<$>>)
    , (<<*>>)
    , mtraverse
    , foldMapM
    , reoption
    , enumeration
    , renum
    , tabulateArray
    , tabulateArraySub
    , (?)
    , ensure
    , asksM
    -- * Pretty-printing
    , Doc
    , ShowPretty (..)
    , Pretty (..)
    , PrettyBy (..)
    , HasPrettyDefaults
    , PrettyDefaultBy
    , PrettyAny (..)
    , Render (..)
    , display
    -- * GHCi
    , printPretty
    -- * Text
    , showText
    ) where

import           Control.Applicative       (Alternative (..))
import           Control.Arrow             ((&&&))
import           Control.Composition       ((.*))
import           Control.DeepSeq           (NFData)
import           Control.Exception         (Exception, throw)
import           Control.Lens
import           Control.Monad.Reader
import           Data.Array
import           Data.Bifunctor            (first, second)
import           Data.Bool                 (bool)
import           Data.Coerce               (Coercible, coerce)
import           Data.Either               (fromRight, isRight)
import           Data.Foldable             (fold, toList)
import           Data.Function             (on)
import           Data.Functor              (($>))
import           Data.Functor.Compose
import           Data.List                 (foldl')
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Maybe                (fromMaybe, isJust, isNothing)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Traversable          (for)
import           Data.Typeable             (Typeable)
import           Data.Word                 (Word8)
import           Debug.Trace
import           GHC.Generics
import           GHC.Natural               (Natural)
import           Text.PrettyBy.Default
import           Text.PrettyBy.Internal

infixr 2 ?
infixl 4 <<$>>, <<*>>

-- | A newtype wrapper around @a@ whose point is to provide a 'Show' instance
-- for anything that has a 'Pretty' instance.
newtype ShowPretty a = ShowPretty
    { unShowPretty :: a
    } deriving (Eq)

instance Pretty a => Show (ShowPretty a) where
    show = display . unShowPretty

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    pretty (Left  x) = parens ("Left"  <+> pretty x)
    pretty (Right y) = parens ("Right" <+> pretty y)

-- | Default pretty-printing for the __spine__ of 'Either' (elements are pretty-printed the way
-- @PrettyBy config@ constraints specify it).
instance (PrettyBy config a, PrettyBy config b) => DefaultPrettyBy config (Either a b)

-- | An instance extending the set of types supporting default pretty-printing with 'Either'.
deriving via PrettyCommon (Either a b)
    instance PrettyDefaultBy config (Either a b) => PrettyBy config (Either a b)

(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) f = getCompose . fmap f . Compose

(<<*>>) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
f <<*>> a = getCompose $ Compose f <*> Compose a

-- | Makes an effectful function ignore its result value and return its input value.
through :: Functor f => (a -> f b) -> (a -> f a)
through f x = f x $> x

mtraverse :: (Monad m, Traversable m, Applicative f) => (a -> f (m b)) -> m a -> f (m b)
mtraverse f a = join <$> traverse f a

-- | Fold a monadic function over a 'Foldable'. The monadic version of 'foldMap'.
foldMapM :: (Foldable f, Monad m, Monoid b) => (a -> m b) -> f a -> m b
foldMapM f xs = foldr step return xs mempty where
    step x r z = f x >>= \y -> r $! z `mappend` y

-- | This function generalizes 'eitherToMaybe', 'eitherToList',
-- 'listToMaybe' and other such functions.
reoption :: (Foldable f, Alternative g) => f a -> g a
reoption = foldr (const . pure) empty

enumeration :: (Bounded a, Enum a) => [a]
enumeration = [minBound .. maxBound]

renum :: (Enum i, Enum j) => i -> j
renum = toEnum . fromEnum

-- | Basically a @Data.Functor.Representable@ instance for 'Array'.
-- We can't provide an actual instance because of the @Distributive@ superclass: @Array i@ is not
-- @Distributive@ unless we assume that indices in an array range over the entirety of @i@.
tabulateArray :: (Bounded i, Enum i, Ix i) => (i -> a) -> Array i a
tabulateArray f = listArray (minBound, maxBound) $ map f enumeration

tabulateArraySub :: forall i j a. (Bounded i, Enum i, Enum j, Ix j) => (i -> a) -> Array j a
tabulateArraySub f = listArray (renum $ minBound @i, renum $ maxBound @i) $ map f enumeration

newtype PairT b f a = PairT
    { unPairT :: f (b, a)
    }

instance Functor f => Functor (PairT b f) where
    fmap f (PairT p) = PairT $ fmap (fmap f) p

-- | @b ? x@ is equal to @pure x@ whenever @b@ holds and is 'empty' otherwise.
(?) :: Alternative f => Bool -> a -> f a
(?) b x = x <$ guard b

-- | @ensure p x@ is equal to @pure x@ whenever @p x@ holds and is 'empty' otherwise.
ensure :: Alternative f => (a -> Bool) -> a -> f a
ensure p x = p x ? x

-- | A monadic version of 'asks'.
asksM :: MonadReader r m => (r -> m a) -> m a
asksM k = ask >>= k

-- For GHCi to use this properly it needs to be in a registered package, hence
-- why we're naming such a trivial thing.
-- | A command suitable for use in GHCi as an interactive printer.
printPretty :: Pretty a => a -> IO ()
printPretty = print . pretty

showText :: Show a => a -> T.Text
showText = T.pack . show
