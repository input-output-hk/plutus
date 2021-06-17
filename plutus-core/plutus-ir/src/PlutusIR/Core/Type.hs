{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module PlutusIR.Core.Type (
    TyName (..),
    Name (..),
    VarDecl (..),
    TyVarDecl (..),
    varDeclNameString,
    tyVarDeclNameString,
    Kind (..),
    Type (..),
    Datatype (..),
    datatypeNameString,
    Recursivity (..),
    Strictness (..),
    Binding (..),
    Term (..),
    Program (..)
    ) where

import           PlutusPrelude

import           PlutusCore          (Kind, Name, TyName, Type (..))
import qualified PlutusCore          as PLC
import           PlutusCore.Constant (AsConstant (..), FromConstant (..), throwNotAConstant)
import           PlutusCore.Core     (UniOf)
import           PlutusCore.Flat     ()
import           PlutusCore.MkPlc    (Def (..), TermLike (..), TyVarDecl (..), VarDecl (..))
import qualified PlutusCore.Name     as PLC


import qualified Data.Text           as T

-- Datatypes

{- Note: [Serialization of PIR]
The serialized version of Plutus-IR will be included in  the final
executable for helping debugging and testing and providing better error
reporting. It is not meant to be stored on the chain, which means that
the underlying representation can vary. The `Generic` instances of the
terms can thus be used as backwards compatibility is not required.
-}

data Datatype tyname name uni fun a = Datatype a (TyVarDecl tyname a) [TyVarDecl tyname a] name [VarDecl tyname name uni fun a]
    deriving (Functor, Show, Generic)

varDeclNameString :: VarDecl tyname Name uni fun a -> String
varDeclNameString = T.unpack . PLC.nameString . varDeclName

tyVarDeclNameString :: TyVarDecl TyName a -> String
tyVarDeclNameString = T.unpack . PLC.nameString . PLC.unTyName . tyVarDeclName

datatypeNameString :: Datatype TyName Name uni fun a -> String
datatypeNameString (Datatype _ tn _ _ _) = tyVarDeclNameString tn

-- Bindings

-- | Each multi-let-group has to be marked with its scoping:
-- * 'NonRec': the identifiers introduced by this multi-let are only linearly-scoped, i.e. an identifer cannot refer to itself or later-introduced identifiers of the group.
-- * 'Rec': an identifiers introduced by this multi-let group can use all other multi-lets  of the same group (including itself),
-- thus permitting (mutual) recursion.
data Recursivity = NonRec | Rec
    deriving (Show, Eq, Generic, Ord)

-- | Recursivity can form a 'Semigroup' / lattice, where 'NonRec' < 'Rec'.
-- The lattice is ordered by "power": a non-recursive binding group can be made recursive and it will still work, but not vice versa.
-- The semigroup operation is the "join" of the lattice.
instance Semigroup Recursivity where
  NonRec <> x = x
  Rec <> _    = Rec

data Strictness = NonStrict | Strict
    deriving (Show, Eq, Generic)

data Binding tyname name uni fun a = TermBind a Strictness (VarDecl tyname name uni fun a) (Term tyname name uni fun a)
                           | TypeBind a (TyVarDecl tyname a) (Type tyname uni a)
                           | DatatypeBind a (Datatype tyname name uni fun a)
    deriving (Functor, Show, Generic)

-- Terms

{- Note [PIR as a PLC extension]
PIR is designed to be an extension of PLC, which means that we try and be strict superset of it.

The main benefit of this is simplicity, but we hope that in future it will make sharing of
theoretical and practical material easier. In the long run it would be nice if PIR was a "true"
extension of PLC (perhaps in the sense of "Trees that Grow"), and then we could potentially
even share most of the typechecker.
-}

{- Note [Declarations in Plutus Core]
In Plutus Core declarations are usually *flattened*, i.e. `(lam x ty t)`, whereas
in Plutus IR they are *reified*, i.e. `(termBind (vardecl x ty) t)`.

Plutus IR needs reified declarations to make it easier to parse *lists* of declarations,
which occur in a few places.

However, we also include all of Plutus Core's term syntax in our term syntax (and we also use
its types). We therefore have somewhat inconsistent syntax since declarations in Plutus
Core terms will be flattened. This makes the embedding obvious and makes life easier
if we were ever to use the Plutus Core AST "for real".

It would be nice to resolve the inconsistency, but this would probably require changing
Plutus Core to use reified declarations.
-}

-- See note [PIR as a PLC extension]
data Term tyname name uni fun a =
                        -- Plutus Core (ish) forms, see note [Declarations in Plutus Core]
                          Let a Recursivity (NonEmpty (Binding tyname name uni fun a)) (Term tyname name uni fun a)
                        | Var a name
                        | TyAbs a tyname (Kind a) (Term tyname name uni fun a)
                        | LamAbs a name (Type tyname uni a) (Term tyname name uni fun a)
                        | Apply a (Term tyname name uni fun a) (Term tyname name uni fun a)
                        | Constant a (PLC.Some (PLC.ValueOf uni))
                        | Builtin a fun
                        | TyInst a (Term tyname name uni fun a) (Type tyname uni a)
                        | Error a (Type tyname uni a)
                        | IWrap a (Type tyname uni a) (Type tyname uni a) (Term tyname name uni fun a)
                        | Unwrap a (Term tyname name uni fun a)
                        deriving (Functor, Show, Generic)

type instance UniOf (Term tyname name uni fun ann) = uni

instance AsConstant (Term tyname name uni fun ann) where
    asConstant (Constant _ val) = pure val
    asConstant term             = throwNotAConstant term

instance FromConstant (Term tyname name uni fun ()) where
    fromConstant = Constant ()

instance TermLike (Term tyname name uni fun) tyname name uni fun where
    var      = Var
    tyAbs    = TyAbs
    lamAbs   = LamAbs
    apply    = Apply
    constant = Constant
    builtin  = Builtin
    tyInst   = TyInst
    unwrap   = Unwrap
    iWrap    = IWrap
    error    = Error
    termLet x (Def vd bind) = Let x NonRec (pure $ TermBind x Strict vd bind)
    typeLet x (Def vd bind) = Let x NonRec (pure $ TypeBind x vd bind)

-- no version as PIR is not versioned
data Program tyname name uni fun a = Program a (Term tyname name uni fun a) deriving Generic

type instance PLC.HasUniques (Term tyname name uni fun ann) = (PLC.HasUnique tyname PLC.TypeUnique, PLC.HasUnique name PLC.TermUnique)
type instance PLC.HasUniques (Program tyname name uni fun ann) = PLC.HasUniques (Term tyname name uni fun ann)
