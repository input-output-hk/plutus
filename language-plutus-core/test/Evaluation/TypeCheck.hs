{-# LANGUAGE OverloadedStrings #-}
module Evaluation.TypeCheck
    ( test_typecheck
    ) where

import           PlutusPrelude
import           Language.PlutusCore
import           Language.PlutusCore.Constant
import           Evaluation.Terms

import           Data.Foldable
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as Bsl
import           Test.Tasty
import           Test.Tasty.HUnit

assertFreshWellTyped :: HasCallStack => Fresh (Term TyName Name ()) -> Assertion
assertFreshWellTyped getTerm =
    let term = unsafeRunFresh getTerm in
        for_ (typecheckTerm term) $ \err -> assertFailure $ concat
            [ "Ill-typed: ", prettyString term, "\n"
            , "Due to: ", prettyString err
            ]

assertFreshIllTyped :: HasCallStack => Fresh (Term TyName Name ()) -> Assertion
assertFreshIllTyped getTerm =
    let term = unsafeRunFresh getTerm in
        case typecheckTerm term of
            Nothing -> assertFailure $ "Well-typed: " ++ prettyString term
            Just _  -> return ()

typecheckProgram :: Program TyName Name () -> Maybe Error
typecheckProgram
    = either Just (\_ -> Nothing)
    . printType
    . Bsl.fromStrict
    . Text.encodeUtf8
    . prettyText

typecheckTerm :: Term TyName Name () -> Maybe Error
typecheckTerm = typecheckProgram . Program () (Version () 0 1 0)

-- | Self-application. An example of ill-typed term.
--
-- > /\ (A :: *) -> \(x : A) -> x x
getBuiltinSelfApply :: Fresh (Term TyName Name ())
getBuiltinSelfApply = do
    a <- freshTyName () "a"
    x <- freshName () "x"
    return
        . TyAbs () a (Type ())
        . LamAbs () x (TyVar () a)
        . Apply () (Var () x)
        $ Var () x

test_typecheckPrelude :: TestTree
test_typecheckPrelude = testCase "Prelude" $ foldMap assertFreshWellTyped
    [ getBuiltinConst
    , getBuiltinUnitval
    , getBuiltinTrue
    , getBuiltinFalse
    , getBuiltinIf
    ]

test_typecheckTerms :: TestTree
test_typecheckTerms = testCase "terms" $ foldMap assertFreshWellTyped
    [ getBuiltinUnroll
    , getBuiltinFix
    , getBuiltinChurchZero
    , getBuiltinChurchSucc
    , getBuiltinZero
    , getBuiltinSucc
    , getBuiltinFoldrNat
    , getBuiltinFoldNat
    , getBuiltinNil
    , getBuiltinCons
    , getBuiltinFoldrList
    , getBuiltinFoldList
    , getBuiltinSum 1
    ]

test_typecheckIllTyped :: TestTree
test_typecheckIllTyped = testCase "ill-typed" $ foldMap assertFreshIllTyped
    [ getBuiltinSelfApply
    ]

test_typecheck :: TestTree
test_typecheck = testGroup "typecheck"
    [ test_typecheckPrelude
    , test_typecheckTerms
    , test_typecheckIllTyped
    ]
