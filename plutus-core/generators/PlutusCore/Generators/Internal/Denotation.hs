-- | This module defines tools for associating PLC terms with their corresponding
-- Haskell values.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeOperators             #-}

module PlutusCore.Generators.Internal.Denotation
    ( Denotation(..)
    , DenotationContextMember(..)
    , DenotationContext(..)
    , denoteVariable
    , insertVariable
    , insertBuiltin
    , typedBuiltins
    ) where

import           PlutusCore.Generators.Internal.Dependent

import           PlutusCore.Constant
import           PlutusCore.Core
import           PlutusCore.Default
import           PlutusCore.MkPlc
import           PlutusCore.Name

import           Data.Dependent.Map                       (DMap)
import qualified Data.Dependent.Map                       as DMap
import           Data.Functor.Compose
import           Data.Proxy

-- | Haskell denotation of a PLC object. An object can be a 'Builtin' or a variable for example.
data Denotation term object res = forall args. Denotation
    { _denotationObject :: object
      -- ^ A PLC object.
    , _denotationToTerm :: object -> term
      -- ^ How to embed the object into a term.
    , _denotationItself :: FoldArgs args res
      -- ^ The denotation of the object. E.g. the denotation of 'AddInteger' is '(+)'.
    , _denotationScheme :: TypeScheme term args res
      -- ^ The 'TypeScheme' of the object. See 'intIntInt' for example.
    }

-- | A member of a 'DenotationContext'.
-- @object@ is existentially quantified, so the only thing that can be done with it,
-- is turning it into a 'Term' using '_denotationToTerm'.
data DenotationContextMember term res =
    forall object. DenotationContextMember (Denotation term object res)

-- | A context of 'DenotationContextMember's.
-- Each row is a mapping from a type to a list of things that can return that type.
-- For example it can contain a mapping from @integer@ to
--   1. a bound variable of type @integer@
--   2. a bound variable of functional type with the result being @integer@
--   3. the 'AddInteger' 'Builtin' or any other 'Builtin' which returns an @integer@.
newtype DenotationContext term = DenotationContext
    { unDenotationContext :: DMap (AsKnownType term) (Compose [] (DenotationContextMember term))
    }

-- Here the only search that we need to perform is the search for things that return an appropriate
-- @r@, be them variables or functions. Better if we also take types of arguments into account,
-- but it is not required as we can always generate an argument out of thin air in a rank-0 setting
-- (without @Void@).

-- | The resulting type of a 'TypeScheme'.
typeSchemeResult :: TypeScheme term args res -> AsKnownType term res
typeSchemeResult (TypeSchemeResult _)     = AsKnownType
typeSchemeResult (TypeSchemeArrow _ schB) = typeSchemeResult schB
typeSchemeResult (TypeSchemeAll _ schK)   = typeSchemeResult $ schK Proxy

-- | Get the 'Denotation' of a variable.
denoteVariable
    :: KnownType (Term TyName Name uni fun ()) res
    => Name -> res -> Denotation (Term TyName Name uni fun ()) Name res
denoteVariable name meta = Denotation name (Var ()) meta (TypeSchemeResult Proxy)

-- | Insert the 'Denotation' of an object into a 'DenotationContext'.
insertDenotation
    :: (GShow (UniOf term), KnownType term res)
    => Denotation term object res -> DenotationContext term -> DenotationContext term
insertDenotation denotation (DenotationContext vs) = DenotationContext $
    DMap.insertWith'
        (\(Compose xs) (Compose ys) -> Compose $ xs ++ ys)
        AsKnownType
        (Compose [DenotationContextMember denotation])
        vs

-- | Insert a variable into a 'DenotationContext'.
insertVariable
    :: (GShow uni, KnownType (Term TyName Name uni fun ()) a)
    => Name
    -> a
    -> DenotationContext (Term TyName Name uni fun ())
    -> DenotationContext (Term TyName Name uni fun ())
insertVariable name = insertDenotation . denoteVariable name

-- | Insert a builtin into a 'DenotationContext'.
insertBuiltin
    :: DefaultFun
    -> DenotationContext (Term TyName Name DefaultUni DefaultFun ())
    -> DenotationContext (Term TyName Name DefaultUni DefaultFun ())
insertBuiltin fun =
    case toBuiltinMeaning fun of
        BuiltinMeaning sch meta _ ->
           case typeSchemeResult sch of
               AsKnownType ->
                   insertDenotation $ Denotation fun (mkBuiltin ()) meta sch

-- Builtins that may fail are commented out, because we cannot handle them right now.
-- Look for "UNDEFINED BEHAVIOR" in "PlutusCore.Generators.Internal.Dependent".
-- | A 'DenotationContext' that consists of 'DefaultFun's.
typedBuiltins
    :: DenotationContext (Term TyName Name DefaultUni DefaultFun ())
typedBuiltins
    = insertBuiltin AddInteger
    . insertBuiltin SubtractInteger
    . insertBuiltin MultiplyInteger
--     . insertBuiltin DivideInteger
--     . insertBuiltin RemainderInteger
--     . insertBuiltin QuotientInteger
--     . insertBuiltin ModInteger
    . insertBuiltin LessThanInteger
    . insertBuiltin LessThanEqualsInteger
    . insertBuiltin GreaterThanInteger
    . insertBuiltin GreaterThanEqualsInteger
    . insertBuiltin EqualsInteger
    . insertBuiltin Concatenate
    . insertBuiltin TakeByteString
    . insertBuiltin DropByteString
    . insertBuiltin Sha2_256
    . insertBuiltin Sha3_256
--     . insertBuiltin VerifySignature
    . insertBuiltin EqualsByteString
    $ DenotationContext mempty
