-- | Tests for all kinds of built-in functions.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}

-- Sure GHC, I'm enabling the extension just so that you can warn be about its usages.
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Evaluation.Builtins.Definition
    ( test_definition
    ) where

import           PlutusCore
import           PlutusCore.Constant
import           PlutusCore.Data
import           PlutusCore.Evaluation.Machine.MachineParameters
import           PlutusCore.Generators.Interesting
import           PlutusCore.MkPlc                                hiding (error)

import           PlutusCore.Examples.Builtins
import           PlutusCore.Examples.Data.Data
import           PlutusCore.StdLib.Data.Bool
import qualified PlutusCore.StdLib.Data.Function                 as Plc
import           PlutusCore.StdLib.Data.Integer
import qualified PlutusCore.StdLib.Data.List                     as Builtin
import qualified PlutusCore.StdLib.Data.ScottList                as Scott

import           Evaluation.Builtins.Common

import           UntypedPlutusCore.Evaluation.Machine.Cek

import           Control.Exception
import           Data.Either
import           Data.Proxy
import           Hedgehog                                        hiding (Opaque, Size, Var)
import qualified Hedgehog.Gen                                    as Gen
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

defaultCekParametersExt
    :: MachineParameters CekMachineCosts CekValue DefaultUni (Either DefaultFun ExtensionFun)
defaultCekParametersExt =
    toMachineParameters $ CostModel defaultCekMachineCosts (defaultBuiltinCostModel, ())

-- | Check that 'Factorial' from the above computes to the same thing as
-- a factorial defined in PLC itself.
test_Factorial :: TestTree
test_Factorial =
    testCase "Factorial" $ do
        let ten = mkConstant @Integer @DefaultUni () 10
            lhs = typecheckEvaluateCek defaultCekParametersExt $
                    apply () (mkBuiltin () $ Right Factorial) ten
            rhs = typecheckEvaluateCek defaultCekParametersExt $
                    apply () (mapFun Left factorial) ten
        assertBool "type checks" $ isRight lhs
        lhs @?= rhs

-- | Check that 'Const' from the above computes to the same thing as
-- a const defined in PLC itself.
test_Const :: TestTree
test_Const =
    testProperty "Const" . property $ do
        c <- forAll Gen.unicode
        b <- forAll Gen.bool
        let tC = mkConstant () c
            tB = mkConstant () b
            char = toTypeAst @_ @DefaultUni @Char Proxy
            runConst con = mkIterApp () (mkIterInst () con [char, bool]) [tC, tB]
            lhs = typecheckReadKnownCek defaultCekParametersExt $ runConst $ mkBuiltin () (Right Const)
            rhs = typecheckReadKnownCek defaultCekParametersExt $ runConst $ mapFun Left Plc.const
        lhs === Right (Right c)
        lhs === rhs

-- | Test that a polymorphic built-in function doesn't subvert the CEK machine.
-- See https://github.com/input-output-hk/plutus/issues/1882
test_Id :: TestTree
test_Id =
    testCase "Id" $ do
        let zer = mkConstant @Integer @DefaultUni () 0
            oneT = mkConstant @Integer @DefaultUni () 1
            oneU = mkConstant @Integer @DefaultUni () 1
            -- id {integer -> integer} ((\(i : integer) (j : integer) -> i) 1) 0
            term =
                mkIterApp () (tyInst () (mkBuiltin () $ Right Id) (TyFun () integer integer))
                    [ apply () constIntegerInteger oneT
                    , zer
                    ] where
                          constIntegerInteger = runQuote $ do
                              i <- freshName "i"
                              j <- freshName "j"
                              return
                                  . LamAbs () i integer
                                  . LamAbs () j integer
                                  $ Var () i
        typecheckEvaluateCekNoEmit defaultCekParametersExt term @?= Right (EvaluationSuccess oneU)

-- | Test that a polymorphic built-in function can have a higher-kinded type variable in its
-- signature.
test_IdFInteger :: TestTree
test_IdFInteger =
    testCase "IdFInteger" $ do
        let one = mkConstant @Integer @DefaultUni () 1
            ten = mkConstant @Integer @DefaultUni () 10
            res = mkConstant @Integer @DefaultUni () 55
            -- sum (idFInteger {list} (enumFromTo 1 10))
            term
                = apply () (mapFun Left Scott.sum)
                . apply () (tyInst () (mkBuiltin () $ Right IdFInteger) Scott.listTy)
                $ mkIterApp () (mapFun Left Scott.enumFromTo) [one, ten]
        typecheckEvaluateCekNoEmit defaultCekParametersExt term @?= Right (EvaluationSuccess res)

test_IdList :: TestTree
test_IdList =
    testCase "IdList" $ do
        let tyAct = typeOfBuiltinFunction @DefaultUni IdList
            tyExp = let a = TyName . Name "a" $ Unique 0
                        listA = TyApp () Scott.listTy (TyVar () a)
                    in TyForall () a (Type ()) $ TyFun () listA listA
            one = mkConstant @Integer @DefaultUni () 1
            ten = mkConstant @Integer @DefaultUni () 10
            res = mkConstant @Integer @DefaultUni () 55
            -- sum (idList {integer} (enumFromTo 1 10))
            term
                = apply () (mapFun Left Scott.sum)
                . apply () (tyInst () (mkBuiltin () $ Right IdList) integer)
                $ mkIterApp () (mapFun Left Scott.enumFromTo) [one, ten]
        tyAct @?= tyExp
        typecheckEvaluateCekNoEmit defaultCekParametersExt term @?= Right (EvaluationSuccess res)

{- Note [Higher-rank built-in functions]
We can't unlift a monomorphic function passed to a built-in function, let alone unlift a polymorphic
one, however we can define a built-in function that accepts an 'Opaque' term of a polymorphic type.
However, as is always the case with an 'Opaque' term, we can't inspect it or use it in any
non-opaque way, so a function of type

    all (f :: * -> *). (all (a :: *). f a) -> all (a :: *). f a

can be assigned the following meaning on the Haskell side:

    \x -> x

but we have no way of providing a meaning for a built-in function with the following signature:

    all (f :: * -> *). all (a :: *). (all (a :: *). f a) -> f a

That's because the meaning function would have to instantiate the @all (a :: *). f a@ argument
somehow to get an @f a@, but that is exactly "using a term in a non-opaque way".

Basically, since we are either generic over @term@ or, like in the example below, use 'CekValue',
there's is no sensible way of instantiating a passed polymorphic argument (or applying a passed
argument when it's a function, for another example).
-}

-- | Test that opaque terms with higher-rank types are allowed.
test_IdRank2 :: TestTree
test_IdRank2 =
    testCase "IdRank2" $ do
        let res = mkConstant @Integer @DefaultUni () 0
            -- sum (idRank2 {list} nil {integer})
            term
                = apply () (mapFun Left Scott.sum)
                . tyInst () (apply () (tyInst () (mkBuiltin () $ Right IdRank2) Scott.listTy) Scott.nil)
                $ integer
        typecheckEvaluateCekNoEmit defaultCekParametersExt term @?= Right (EvaluationSuccess res)

-- | Test that an exception thrown in the builtin application code does not get caught in the CEK
-- machine and blows in the caller face instead. Uses a one-argument built-in function.
test_FailingSucc :: TestTree
test_FailingSucc =
    testCase "FailingSucc" $ do
        let term =
                apply () (mkBuiltin () $ Right FailingSucc) $
                    mkConstant @Integer @DefaultUni () 0
        typeErrOrEvalExcOrRes :: Either _ (Either BuiltinErrorCall _) <-
            -- Here we rely on 'typecheckAnd' lazily running the action after type checking the term.
            traverse (try . evaluate) $ typecheckEvaluateCekNoEmit defaultCekParametersExt term
        typeErrOrEvalExcOrRes @?= Right (Left BuiltinErrorCall)

-- | Test that evaluating a PLC builtin application that is expensive enough to exceed the budget
-- does not result in actual evaluation of the application on the Haskell side and instead we get an
-- 'EvaluationFailure'. Uses a one-argument built-in function.
test_ExpensiveSucc :: TestTree
test_ExpensiveSucc =
    testCase "ExpensiveSucc" $ do
        let term =
                apply () (mkBuiltin () $ Right ExpensiveSucc) $
                    mkConstant @Integer @DefaultUni () 0
        typeErrOrEvalExcOrRes :: Either _ (Either BuiltinErrorCall _) <-
            traverse (try . evaluate) $ typecheckEvaluateCekNoEmit defaultCekParametersExt term
        typeErrOrEvalExcOrRes @?= Right (Right EvaluationFailure)

-- | Test that an exception thrown in the builtin application code does not get caught in the CEK
-- machine and blows in the caller face instead. Uses a two-arguments built-in function.
test_FailingPlus :: TestTree
test_FailingPlus =
    testCase "FailingPlus" $ do
        let term =
                mkIterApp () (mkBuiltin () $ Right FailingPlus)
                    [ mkConstant @Integer @DefaultUni () 0
                    , mkConstant @Integer @DefaultUni () 1
                    ]
        typeErrOrEvalExcOrRes :: Either _ (Either BuiltinErrorCall _) <-
            -- Here we rely on 'typecheckAnd' lazily running the action after type checking the term.
            traverse (try . evaluate) $ typecheckEvaluateCekNoEmit defaultCekParametersExt term
        typeErrOrEvalExcOrRes @?= Right (Left BuiltinErrorCall)

-- | Test that evaluating a PLC builtin application that is expensive enough to exceed the budget
-- does not result in actual evaluation of the application on the Haskell side and instead we get an
-- 'EvaluationFailure'. Uses a two-arguments built-in function.
test_ExpensivePlus :: TestTree
test_ExpensivePlus =
    testCase "ExpensivePlus" $ do
        let term =
                mkIterApp () (mkBuiltin () $ Right ExpensivePlus)
                    [ mkConstant @Integer @DefaultUni () 0
                    , mkConstant @Integer @DefaultUni () 1
                    ]
        typeErrOrEvalExcOrRes :: Either _ (Either BuiltinErrorCall _) <-
            traverse (try . evaluate) $ typecheckEvaluateCekNoEmit defaultCekParametersExt term
        typeErrOrEvalExcOrRes @?= Right (Right EvaluationFailure)

-- | Test that @Null@, @Head@ and @Tail@ are enough to get pattern matching on built-in lists.
test_BuiltinList :: TestTree
test_BuiltinList =
    testCase "BuiltinList" $ do
        let xs  = [1..10]
            res = mkConstant @Integer @DefaultUni () $ foldr (-) 0 xs
            term
                = mkIterApp () (mkIterInst () Builtin.foldrList [integer, integer])
                    [ mkBuiltin () SubtractInteger
                    , mkConstant @Integer () 0
                    , mkConstant @[Integer] () xs
                    ]
        typecheckEvaluateCekNoEmit defaultCekParameters term @?= Right (EvaluationSuccess res)

-- | Test that right-folding a built-in list with built-in 'Cons' recreates that list.
test_IdBuiltinList :: TestTree
test_IdBuiltinList =
    testCase "IdBuiltinList" $ do
        let xsTerm :: TermLike term tyname name DefaultUni fun => term ()
            xsTerm = mkConstant @[Integer] () [1..10]
            listOfInteger = mkTyBuiltin @_ @[Integer] ()
            term
                = mkIterApp () (mkIterInst () (mapFun Left Builtin.foldrList) [integer, listOfInteger])
                    [ tyInst () (mkBuiltin () $ Right Cons) integer
                    , mkConstant @[Integer] () []
                    , xsTerm
                    ]
        typecheckEvaluateCekNoEmit defaultCekParametersExt term @?= Right (EvaluationSuccess xsTerm)

test_BuiltinPair :: TestTree
test_BuiltinPair =
    testCase "BuiltinPair" $ do
        let arg = mkConstant @(Integer, Bool) @DefaultUni () (1, False)
            inst efun = mkIterInst () (mkBuiltin () efun) [integer, bool]
            swapped = apply () (inst $ Right Swap) arg
            fsted   = apply () (inst $ Left FstPair) arg
            snded   = apply () (inst $ Left SndPair) arg
        -- Swap {integer} {bool} (1, False) ~> (False, 1)
        typecheckEvaluateCekNoEmit defaultCekParametersExt swapped @?=
            Right (EvaluationSuccess $ mkConstant @(Bool, Integer) () (False, 1))
        -- Fst {integer} {bool} (1, False) ~> 1
        typecheckEvaluateCekNoEmit defaultCekParametersExt fsted @?=
            Right (EvaluationSuccess $ mkConstant @Integer () 1)
        -- Snd {integer} {bool} (1, False) ~> False
        typecheckEvaluateCekNoEmit defaultCekParametersExt snded @?=
            Right (EvaluationSuccess $ mkConstant @Bool () False)

test_SwapEls :: TestTree
test_SwapEls =
    testCase "SwapEls" $ do
        let xs = zip [1..10] $ cycle [False, True]
            res = mkConstant @Integer @DefaultUni () $
                    foldr (\p r -> r + (if snd p then -1 else 1) * fst p) 0 xs
            el = mkTyBuiltin @_ @(Integer, Bool) ()
            instProj proj = mkIterInst () (mkBuiltin () proj) [integer, bool]
            fun = runQuote $ do
                    p <- freshName "p"
                    r <- freshName "r"
                    return
                        . lamAbs () p el
                        . lamAbs () r integer
                        $ mkIterApp () (mkBuiltin () AddInteger)
                            [ Var () r
                            , mkIterApp () (mkBuiltin () MultiplyInteger)
                                [ mkIterApp () (tyInst () (mkBuiltin () IfThenElse) integer)
                                    [ apply () (instProj SndPair) $ Var () p
                                    , mkConstant @Integer () (-1)
                                    , mkConstant @Integer () 1
                                    ]
                                , apply () (instProj FstPair) $ Var () p
                                ]
                            ]
            term
                = mkIterApp () (mkIterInst () Builtin.foldrList [el, integer])
                    [ fun
                    , mkConstant @Integer () 0
                    , mkConstant () xs
                    ]
        typecheckEvaluateCekNoEmit defaultCekParameters term @?= Right (EvaluationSuccess res)

-- | Test that right-folding a built-in 'Data' with the constructors of 'Data' recreates the
-- original value.
test_IdBuiltinData :: TestTree
test_IdBuiltinData =
    testCase "IdBuiltinData" $ do
        let dTerm :: TermLike term tyname name DefaultUni fun => term ()
            dTerm = mkConstant @Data () $ Map [(I 42, Constr 4 [List [B "abc", Constr 2 []], I 0])]
            emb = mkBuiltin () . Left
            term = mkIterApp () ofoldrData
                [ emb ConstrData
                , emb MapData
                , emb ListData
                , emb IData
                , emb BData
                , dTerm
                ]
        typecheckEvaluateCekNoEmit defaultCekParametersExt term @?= Right (EvaluationSuccess dTerm)

test_definition :: TestTree
test_definition =
    testGroup "definition"
        [ test_Factorial
        , test_Const
        , test_Id
        , test_IdFInteger
        , test_IdList
        , test_IdRank2
        , test_FailingSucc
        , test_ExpensiveSucc
        , test_FailingPlus
        , test_ExpensivePlus
        , test_BuiltinList
        , test_IdBuiltinList
        , test_BuiltinPair
        , test_SwapEls
        , test_IdBuiltinData
        ]
