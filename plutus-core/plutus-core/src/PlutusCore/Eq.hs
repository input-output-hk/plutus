-- | An internal module that defines functions for deciding equality of values of data types
-- that encode things with binders.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module PlutusCore.Eq
    ( LR (..)
    , RL (..)
    , EqRename
    , ScopedEqRename
    , runEqRename
    , withTwinBindings
    , eqNameM
    , eqM
    ) where

import           PlutusPrelude

import           PlutusCore.Name
import           PlutusCore.Rename.Monad

import           Control.Lens
import           Control.Monad.Reader

{- Note [Modulo alpha]
The implemented algorithm of checking equality modulo alpha works as follows
(taking types as an example):

- traverse both the types simultaneously
- if the outermost constructors differ, return 'False'
- otherwise if the constructors are binders, then record that the two possibly distinct bound
    variables map to each other
- otherwise if the constructors are variables, look them up in the current scope
    * if both are in scope, then those are bound variables, so check that they map to each other
        (i.e. are introduced by twin binders)
    * if both are not in scope, then those are free variables, so check if their uniques are equal
    * if one is in scope and the other one is not, then return 'False'
- otherwise check equality of non-recursive constituents and recurse for recursive occurrences
-}

{- Note [Scope tracking]
Scopes (term level vs type level) are resolved automatically in a type-driven way much like we do
in the renamer. This allows not to think about scopes when implementing the actual equality checks
and makes it impossible to confuse the scopes and e.g. insert a type-level name into a term-level
scope. Also allows to define a function that records bindings and a function that checks equality
of two names in a general manner for both the scopes.
-}

{- Note [Side tracking]
The simplest way to track that twin variables map to each other is to have two contexts:

- left-hand side variables and what they map to
- right-hand side variables and what they map to

(we refer to a first argument as being on the left-hand side and to a second argument as being on the right-hand side)

E.g. when checking equality of these two types:

    all (x_3 :: *) (x_3 :: *). x_3 -> x_3
    all (y_4 :: *) (z_5 :: *). y_4 -> z_5

we first record that @x_3@ maps to @y_4@ and vice versa, then record that @x_3@ maps to @z_5@ and
vice versa. This way when we later check equality of @x_3@ and @y_4@ we know that it doesn't hold,
because even though @y_4@ maps to @x_3@, @x_3@ doesn't map to @y_4@, because that mapping was
overwritten by the @x_3@-to-@z_5@ one.

For storing the left-to-right and right-to-left mappings separately we use the 'Bilateral' data
type. Given that we track not only sides, but also scopes, we instantiate 'Bilateral' with either
type-level-only renamings (for checking equality of types) or scoped ones (for checking equality
of terms and programs). This amounts to the following generic monad:

    RenameT (Bilateral ren) m

i.e. regardless of what the underlying renaming is, it has to be bilateral.

We zoom into the sides of a bilateral renaming using the 'LR' and 'RL' newtype wrappers using the
same 'HasRenaming' machinery that we use for zooming into the scopes of a scoped renaming:

- the 'LR' wrapper allows to retrieve a lens focusing on the left  renaming
- the 'RL' wrapper allows to retrieve a lens focusing on the right renaming

I.e. you wrap a name into either 'LR' and 'RL' and depending on that you get focused on either
left-to-right or right-to-left part of a 'Bilateral' renaming.

So e.g.

    withRenamedName (LR name1) (LR name2)

reads as "record that @name1@ maps to @name2@ from left to right"

and

    lookupNameM $ RL name2

reads as "look the right-to-left mapping of @name2@ up".

I.e. we first resolve sides using explicit wrappers and then scope resolution happens automatically
on the basis of existing type information (e.g. 'TyName' is a type-level name, hence we need the
type-level renaming).
-}

-- See Note [Side tracking].
-- | From left to right.
newtype LR a = LR
    { unLR :: a
    } deriving (Generic)

-- See Note [Side tracking].
-- | From right to left.
newtype RL a = RL
    { unRL :: a
    } deriving (Generic)

-- See Note [Side tracking].
-- | A left @a@ and a right @a@.
data Bilateral a = Bilateral
    { _bilateralL :: a
    , _bilateralR :: a
    }

makeLenses ''Bilateral

instance Wrapped (LR a)
instance Wrapped (RL a)
instance HasUnique name unique => HasUnique (LR name) (LR unique)
instance HasUnique name unique => HasUnique (RL name) (RL unique)

instance Semigroup a => Semigroup (Bilateral a) where
    Bilateral l1 r1 <> Bilateral l2 r2 = Bilateral (l1 <> l2) (r1 <> r2)

instance Monoid a => Monoid (Bilateral a) where
    mempty = Bilateral mempty mempty

-- To rename from left to right is to update the left renaming.
instance HasRenaming ren unique => HasRenaming (Bilateral ren) (LR unique) where
    renaming = bilateralL . renaming . coerced @(Renaming unique)

-- To rename from right to left is to update the right renaming.
instance HasRenaming ren unique => HasRenaming (Bilateral ren) (RL unique) where
    renaming = bilateralR . renaming . coerced @(Renaming unique)

-- | The type of a runnable equality check. @Maybe ()@ is isomorphic to 'Bool' and we use it
-- instead of 'Bool', because this unlocks the convenient and readable do-notation and allows for
-- automatic short-circuiting, which would be tedious with @Rename (Bilateral ren) Bool@.
type EqRename ren = RenameT (Bilateral ren) Maybe ()
type ScopedEqRename = EqRename ScopedRenaming

-- | Run an 'EqRename' computation.
runEqRename :: Monoid ren => EqRename ren -> Bool
runEqRename = isJust . runRenameT

-- See Note [Modulo alpha].
-- | Record that two names map to each other.
withTwinBindings
    :: (HasRenaming ren unique, HasUnique name unique, MonadReader (Bilateral ren) m)
    => name -> name -> m c -> m c
withTwinBindings name1 name2 k =
    withRenamedName (LR name1) (LR name2) $
    withRenamedName (RL name2) (RL name1) k

-- See Note [Modulo alpha].
-- | Check equality of two names.
eqNameM
    :: (HasRenaming ren unique, HasUnique name unique, Eq unique)
    => name -> name -> EqRename ren
eqNameM name1 name2 = do
    mayUniq2' <- lookupNameM $ LR name1
    mayUniq1' <- lookupNameM $ RL name2
    let uniq1 = name1 ^. unique
        uniq2 = name2 ^. unique
    guard $ case (mayUniq1', mayUniq2') of
        (Nothing         , Nothing         ) -> uniq1 == uniq2
        (Just (RL uniq1'), Just (LR uniq2')) -> uniq1 == uniq1' && uniq2 == uniq2'
        (_               , _               ) -> False

-- | Check equality of things having an 'Eq' instance.
eqM :: Eq a => a -> a -> EqRename ren
eqM x1 x2 = guard $ x1 == x2
