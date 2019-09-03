{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module Language.PlutusCore.Constant.Function
    ( typeSchemeToType
    , nameMeaningToType
    , insertNameDefinition
    , typeOfTypedBuiltinName
    ) where

import           Language.PlutusCore.Constant.Typed
import           Language.PlutusCore.Name
import           Language.PlutusCore.Quote
import           Language.PlutusCore.Type

import qualified Data.Map                           as Map
import           Data.Proxy
import qualified Data.Text                          as Text
import           GHC.TypeLits

-- | Convert a 'TypeScheme' to the corresponding 'Type'.
-- Basically, a map from the PHOAS representation to the FOAS one.
typeSchemeToType :: TypeScheme uni a r -> Type TyName uni ()
typeSchemeToType = undefined {- runQuote . go 0 where
    go :: Int -> TypeScheme uni a r -> Quote (Type TyName uni ())
    go _ (TypeSchemeResult pR)          = pure $ toTypeAst pR
    go i (TypeSchemeArrow pA schB)    =
        TyFun () (toTypeAst pA) <$> go i schB
    go i (TypeSchemeAllType proxy schK) = case proxy of
        (_ :: Proxy '(text, uniq)) -> do
            let text = Text.pack $ symbolVal @text Proxy
                uniq = fromIntegral $ natVal @uniq Proxy
                a    = TyName $ Name () text $ Unique uniq
            TyForall () a (Type ()) <$> go i (schK Proxy) -}

-- | Extract the 'TypeScheme' from a 'NameMeaning' and
-- convert it to the corresponding 'Type'.
nameMeaningToType :: NameMeaning uni -> Type TyName uni ()
nameMeaningToType (NameMeaning sch _) = typeSchemeToType sch

-- | Insert a 'NameDefinition' into a 'NameMeanings'.
insertNameDefinition :: NameDefinition uni -> NameMeanings uni -> NameMeanings uni
insertNameDefinition (NameDefinition name mean) (NameMeanings nameMeans) =
    NameMeanings $ Map.insert name mean nameMeans

-- | Return the 'Type' of a 'TypedBuiltinName'.
typeOfTypedBuiltinName :: TypedBuiltinName uni a r -> Type TyName uni ()
typeOfTypedBuiltinName (TypedBuiltinName _ scheme) = typeSchemeToType scheme
