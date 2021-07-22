-- | Kind/type inference/checking.

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module PlutusCore.TypeCheck
    ( Typecheckable
    -- * Configuration.
    , BuiltinTypes (..)
    , TypeCheckConfig (..)
    , tccBuiltinTypes
    , builtinMeaningsToTypes
    , getDefTypeCheckConfig
    -- * Kind/type inference/checking.
    , inferKind
    , checkKind
    , inferType
    , checkType
    , inferTypeOfProgram
    , checkTypeOfProgram
    ) where

import           PlutusPrelude

import           PlutusCore.Constant
import           PlutusCore.Core
import           PlutusCore.Error
import           PlutusCore.Name
import           PlutusCore.Normalize
import           PlutusCore.Quote
import           PlutusCore.Rename
import           PlutusCore.TypeCheck.Internal

import           Control.Monad.Except
import           Data.Array
import           Universe

type Typecheckable uni fun = (ToKind uni, HasUniApply uni, ToBuiltinMeaning uni fun)

-- | Extract the 'TypeScheme' from a 'BuiltinMeaning' and convert it to the
-- corresponding 'Type' for each built-in function.
builtinMeaningsToTypes
    :: forall term uni fun ann err m.
       (MonadError err m, AsTypeError err term uni fun ann, Typecheckable uni fun)
    => ann -> m (BuiltinTypes uni fun)
builtinMeaningsToTypes ann =
    runQuoteT . fmap (BuiltinTypes . Just) . sequence . tabulateArraySub @fun $ \fun -> do
        let ty = typeOfBuiltinFunction fun
        _ <- inferKind (TypeCheckConfig $ BuiltinTypes Nothing) $ ann <$ ty
        pure <$> normalizeType ty

-- | Get the default type checking config.
getDefTypeCheckConfig
    :: (MonadError err m, AsTypeError err term uni fun ann, Typecheckable uni fun)
    => ann -> m (TypeCheckConfig uni fun)
getDefTypeCheckConfig ann = TypeCheckConfig <$> builtinMeaningsToTypes ann

-- | Infer the kind of a type.
inferKind
    :: (MonadQuote m, MonadError err m, AsTypeError err term uni fun ann, ToKind uni)
    => TypeCheckConfig uni fun -> Type TyName uni ann -> m (Kind ())
inferKind config = runTypeCheckM config . inferKindM

-- | Check a type against a kind.
-- Infers the kind of the type and checks that it's equal to the given kind
-- throwing a 'TypeError' (annotated with the value of the @ann@ argument) otherwise.
checkKind
    :: (MonadQuote m, MonadError err m, AsTypeError err term uni fun ann, ToKind uni)
    => TypeCheckConfig uni fun -> ann -> Type TyName uni ann -> Kind () -> m ()
checkKind config ann ty = runTypeCheckM config . checkKindM ann ty

-- | Infer the type of a term.
inferType
    :: ( MonadError err m, MonadQuote m
       , AsTypeError err (Term TyName Name uni fun ()) uni fun ann, ToKind uni, HasUniApply uni
       , GEq uni, Ix fun
       )
    => TypeCheckConfig uni fun -> Term TyName Name uni fun ann -> m (Normalized (Type TyName uni ()))
inferType config = rename >=> runTypeCheckM config . inferTypeM

-- | Check a term against a type.
-- Infers the type of the term and checks that it's equal to the given type
-- throwing a 'TypeError' (annotated with the value of the @ann@ argument) otherwise.
checkType
    :: ( MonadError err m, MonadQuote m
       , AsTypeError err (Term TyName Name uni fun ()) uni fun ann, ToKind uni, HasUniApply uni
       , GEq uni, Ix fun
       )
    => TypeCheckConfig uni fun
    -> ann
    -> Term TyName Name uni fun ann
    -> Normalized (Type TyName uni ())
    -> m ()
checkType config ann term ty = do
    termRen <- rename term
    runTypeCheckM config $ checkTypeM ann termRen ty

-- | Infer the type of a program.
inferTypeOfProgram
    :: ( MonadError err m, MonadQuote m
       , AsTypeError err (Term TyName Name uni fun ()) uni fun ann, ToKind uni, HasUniApply uni
       , GEq uni, Ix fun
       )
    => TypeCheckConfig uni fun
    -> Program TyName Name uni fun ann
    -> m (Normalized (Type TyName uni ()))
inferTypeOfProgram config (Program _ _ term) = inferType config term

-- | Check a program against a type.
-- Infers the type of the program and checks that it's equal to the given type
-- throwing a 'TypeError' (annotated with the value of the @ann@ argument) otherwise.
checkTypeOfProgram
    :: ( MonadError err m, MonadQuote m
       , AsTypeError err (Term TyName Name uni fun ()) uni fun ann, ToKind uni, HasUniApply uni
       , GEq uni, Ix fun
       )
    => TypeCheckConfig uni fun
    -> ann
    -> Program TyName Name uni fun ann
    -> Normalized (Type TyName uni ())
    -> m ()
checkTypeOfProgram config ann (Program _ _ term) = checkType config ann term
