{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Evaluation.Builtins.MakeRead
    ( test_makeRead
    ) where

import qualified PlutusCore                              as TPLC
import           PlutusCore.Constant
import           PlutusCore.Default
import           PlutusCore.Evaluation.Machine.Exception
import           PlutusCore.Evaluation.Result
import           PlutusCore.MkPlc                        hiding (error)
import           PlutusCore.Pretty
import           PlutusCore.StdLib.Data.Unit

import           UntypedPlutusCore                       as UPLC (Name, Term, TyName)

import           Evaluation.Builtins.Common

import           Hedgehog                                hiding (Size, Var)
import qualified Hedgehog.Gen                            as Gen
import qualified Hedgehog.Range                          as Range
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

-- | Convert a Haskell value to a PLC term and then convert back to a Haskell value
-- of a different type.
readMakeHetero
    :: ( KnownType (TPLC.Term TyName Name DefaultUni DefaultFun ()) a
       , KnownType (UPLC.Term Name DefaultUni DefaultFun ()) b
       )
    => a -> EvaluationResult b
readMakeHetero x = do
    xTerm <- makeKnownNoEmit @(TPLC.Term TyName Name DefaultUni DefaultFun ()) x
    case extractEvaluationResult <$> typecheckReadKnownCek TPLC.defaultCekParameters xTerm of
        Left err          -> error $ "Type error" ++ displayPlcCondensedErrorClassic err
        Right (Left err)  -> error $ "Evaluation error: " ++ show err
        Right (Right res) -> res

-- | Convert a Haskell value to a PLC term and then convert back to a Haskell value
-- of the same type.
readMake
    :: ( KnownType (TPLC.Term TyName Name DefaultUni DefaultFun ()) a
       , KnownType (UPLC.Term Name DefaultUni DefaultFun ()) a
       )
    => a -> EvaluationResult a
readMake = readMakeHetero

builtinRoundtrip
    :: ( KnownType (TPLC.Term TyName Name DefaultUni DefaultFun ()) a
       , KnownType (UPLC.Term Name DefaultUni DefaultFun ()) a
       , Show a, Eq a
       )
    => Gen a -> Property
builtinRoundtrip genX = property $ do
    x <- forAll genX
    case readMake x of
        EvaluationFailure    -> fail "EvaluationFailure"
        EvaluationSuccess x' -> x === x'

test_stringRoundtrip :: TestTree
test_stringRoundtrip =
    testProperty "stringRoundtrip" . builtinRoundtrip $
        Gen.string (Range.linear 0 20) Gen.unicode

-- | Generate a bunch of 'String's, put each of them into a 'Term', apply a builtin over
-- each of these terms such that being evaluated it calls a Haskell function that appends a char to
-- the contents of an external 'IORef' and assemble all the resulting terms together in a single
-- term where all characters are passed to lambdas and ignored, so that only 'unitval' is returned
-- in the end.
-- After evaluation of the CEK machine finishes, read the 'IORef' and check that you got the exact
-- same sequence of 'String's that was originally generated.
-- Calls 'unsafePerformIO' internally while evaluating the term, because the CEK machine can only
-- handle pure things and 'unsafePerformIO' is the way to pretend an effectful thing is pure.
test_collectStrings :: TestTree
test_collectStrings = testProperty "collectStrings" . property $ do
    strs <- forAll . Gen.list (Range.linear 0 10) $ Gen.string (Range.linear 0 20) Gen.unicode
    let step arg rest = mkIterApp () sequ
            [ apply () (mkBuiltin () Trace) $ mkConstant @String @DefaultUni () arg
            , rest
            ]
        term = foldr step unitval strs
    strs' <- case typecheckEvaluateCek TPLC.defaultCekParameters term of
        Left _                             -> failure
        Right (EvaluationFailure, _)       -> failure
        Right (EvaluationSuccess _, strs') -> return strs'
    strs === strs'

test_noticeEvaluationFailure :: TestTree
test_noticeEvaluationFailure =
    testCase "noticeEvaluationFailure" . assertBool "'EvaluationFailure' ignored" $
        isEvaluationFailure $ do
            _ <- readMake True
            _ <- readMakeHetero @(EvaluationResult ()) @() EvaluationFailure
            readMake 'a'

test_makeRead :: TestTree
test_makeRead =
    testGroup "makeRead"
        [ test_stringRoundtrip
        , test_collectStrings
        , test_noticeEvaluationFailure
        ]
