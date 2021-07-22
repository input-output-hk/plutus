{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Check.Spec (tests) where

import           PlutusPrelude

import           PlutusCore
import qualified PlutusCore.Check.Normal   as Normal
import qualified PlutusCore.Check.Uniques  as Uniques
import qualified PlutusCore.Check.Value    as VR
import           PlutusCore.Generators
import           PlutusCore.Generators.AST
import           PlutusCore.MkPlc

import           Control.Monad.Except
import           Data.Either
import           Hedgehog                  hiding (Var)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "checks"
    [ testProperty "renaming ensures global uniqueness" propRenameCheck
    , shadowed
    , multiplyDefined
    , incoherentUse
    , values
    , normalTypes
    , normalTypesCheck
    ]

data Tag = Tag Int | Ignore deriving (Show, Eq, Ord)

checkTermUniques :: (Ord a, MonadError (UniqueError a) m) => Term TyName Name uni fun a -> m ()
checkTermUniques = Uniques.checkTerm (\case FreeVariable{} -> False; _ -> True)

shadowed :: TestTree
shadowed =
    let
        u = Unique (-1)
        checked = runExcept $ runQuoteT $ do
            ty <- freshTyName "ty"
            let n = Name "yo" u
            let term =
                    LamAbs (Tag 1) n (TyVar Ignore ty) $
                    LamAbs (Tag 2) n (TyVar Ignore ty) $
                    Var Ignore n
            checkTermUniques term
        assertion = checked @?= Left (MultiplyDefined u (Tag 1) (Tag 2))
    in testCase "shadowed" assertion

multiplyDefined :: TestTree
multiplyDefined =
    let
        u = Unique (-1)
        checked = runExcept $ runQuoteT $ do
            ty <- freshTyName "ty"
            let n = Name "yo" u
            let term =
                    Apply Ignore
                    (LamAbs (Tag 1) n (TyVar Ignore ty) (Var Ignore n))
                    (LamAbs (Tag 2) n (TyVar Ignore ty) (Var Ignore n))
            checkTermUniques term
        assertion = checked @?= Left (MultiplyDefined u (Tag 1) (Tag 2))
    in testCase "multiplyDefined" assertion

incoherentUse :: TestTree
incoherentUse =
    let
        u = Unique 0
        checked = runExcept $ runQuoteT $ do
            let n = Name "yo" u
            let ty = TyName n
            let term =
                    LamAbs (Tag 1) n (TyVar (Tag 2) ty) $
                    TyInst Ignore (Var (Tag 3) n) (TyVar (Tag 4) ty)
            checkTermUniques term
        assertion = checked @?= Left (IncoherentUsage u (Tag 1) (Tag 2))
    in testCase "incoherentUse" assertion

propRenameCheck :: Property
propRenameCheck = property $ do
    prog <- forAllPretty $ runAstGen genProgram
    renamed <- runQuoteT $ rename prog
    annotateShow $ ShowPretty renamed
    Hedgehog.evalExceptT $ checkUniques renamed
        where
            checkUniques :: (Ord a, MonadError (UniqueError a) m) => Program TyName Name uni fun a -> m ()
            -- the renamer will fix incoherency between *bound* variables, but it ignores free variables, so
            -- we can still get incoherent usage errors, ignore them for now
            checkUniques = Uniques.checkProgram (\case { FreeVariable{} -> False; IncoherentUsage {} -> False; _ -> True})

values :: TestTree
values = runQuote $ do
    aN <- freshTyName "a"
    let aV = TyVar () aN
        val = mkConstant @Integer @DefaultUni () 2
        nonVal = Error () aV
    pure $ testGroup "values" [
          testCase "wrapNonValue" $ VR.isTermValue (IWrap () aV aV nonVal) @?= False
        , testCase "wrapValue" $ VR.isTermValue (IWrap () aV aV val) @?= True

        , testCase "absNonValue" $ VR.isTermValue (TyAbs () aN (Type ()) nonVal) @?= True
        , testCase "absValue" $ VR.isTermValue (TyAbs () aN (Type()) val) @?= True

        , testCase "error" $ VR.isTermValue (Error () aV) @?= False
        , testCase "lam" $ VR.isTermValue (LamAbs () (Var () aN) aV nonVal) @?= True
        , testCase "app" $ VR.isTermValue (Apply () val val) @?= False
        , testCase "unwrap" $ VR.isTermValue (Unwrap () val) @?= False
        , testCase "inst" $ VR.isTermValue (TyInst () val aV) @?= False
        , testCase "constant" $ VR.isTermValue (mkConstant @Integer @DefaultUni () 1) @?= True
        , testCase "builtin" $ VR.isTermValue (mkBuiltin () AddInteger) @?= False
      ]

normalTypes :: TestTree
normalTypes = runQuote $ do
    aN <- freshTyName "a"
    let integer = mkTyBuiltin @_ @Integer @DefaultUni ()
        neutral = TyVar () aN
        normal = integer
        nonNormal = TyApp () (TyLam () aN (Type ()) neutral) normal
    pure $ testGroup "normal types" [
          testCase "var" $ Normal.isNormalType neutral @?= True

        , testCase "funNormal" $ Normal.isNormalType (TyFun () normal normal) @?= True
        , testCase "funNotNormal" $ Normal.isNormalType (TyFun () normal nonNormal) @?= False

        , testCase "lamNormal" $ Normal.isNormalType (TyLam () aN (Type ()) normal) @?= True
        , testCase "lamNonNormal" $ Normal.isNormalType (TyLam () aN (Type ()) nonNormal) @?= False

        , testCase "forallNormal" $ Normal.isNormalType (TyForall () aN (Type ()) normal) @?= True
        , testCase "forallNonNormal" $ Normal.isNormalType (TyForall () aN (Type ()) nonNormal) @?= False

        , testCase "ifixNormal" $ Normal.isNormalType (TyIFix () normal normal) @?= True
        , testCase "ifixNonNormal" $ Normal.isNormalType (TyIFix () nonNormal normal) @?= False

        , testCase "appNormal" $ Normal.isNormalType (TyApp () neutral normal) @?= True
        , testCase "appNonNormal" $ Normal.isNormalType (TyApp () nonNormal normal) @?= False

        , testCase "builtin" $ Normal.isNormalType integer @?= True
      ]

normalTypesCheck :: TestTree
normalTypesCheck = runQuote $ do
    aN <- freshTyName "a"
    xN <- freshName "x"
    let integer = mkTyBuiltin @_ @Integer ()
        aV = TyVar () aN
        xV = Var () xN
        normal = integer
        nonNormal = TyApp () (TyLam () aN (Type ()) aV) normal
    pure $ testGroup "normalized types check" [
        testCase "lamNormal" $ isRight (checkNormal (LamAbs () xN normal xV)) @? "Normalization"
        , testCase "lamNonNormal" $ isLeft (checkNormal (LamAbs () xN nonNormal xV)) @? "Normalization"

        , testCase "abs" $ isRight (checkNormal (TyAbs () aN (Type ()) xV)) @? "Normalization"

        , testCase "wrapNormal" $ isRight (checkNormal (IWrap () normal normal xV)) @? "Normalization"
        , testCase "wrapNonNormal" $ isLeft (checkNormal (IWrap () nonNormal nonNormal xV)) @? "Normalization"

        , testCase "unwrap" $ isRight (checkNormal (Unwrap () xV)) @? "Normalization"

        , testCase "app" $ isRight (checkNormal (Apply () xV xV)) @? "Normalization"

        , testCase "errorNormal" $ isRight (checkNormal (Error () normal)) @? "Normalization"
        , testCase "errorNonNormal" $ isLeft (checkNormal (Error () nonNormal)) @? "Normalization"

        , testCase "constant" $ isRight (checkNormal (mkConstant @Integer () 2)) @? "Normalization"
        , testCase "builtin" $ isRight (checkNormal (mkBuiltin () AddInteger)) @? "Normalization"
      ]
        where
            checkNormal :: Term TyName Name DefaultUni DefaultFun () -> Either (Normal.NormCheckError TyName Name DefaultUni DefaultFun ()) ()
            checkNormal = Normal.checkTerm
