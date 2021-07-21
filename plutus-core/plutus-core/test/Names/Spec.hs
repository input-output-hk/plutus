{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Names.Spec where

import           PlutusPrelude

import           PlutusCore
import           PlutusCore.Check.Scoping
import           PlutusCore.DeBruijn
import           PlutusCore.Mark
import           PlutusCore.Pretty
import           PlutusCore.Rename.Internal

import           PlutusCore.Generators
import           PlutusCore.Generators.AST         as AST
import           PlutusCore.Generators.Interesting

import           Control.Monad.Reader
import           Control.Monad.State
import           Hedgehog                          hiding (Var)
import qualified Hedgehog.Gen                      as Gen
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

prop_DeBruijn :: Gen (TermOf (Term TyName Name DefaultUni DefaultFun ()) a) -> Property
prop_DeBruijn gen = property . generalizeT $ do
    TermOf body _ <- forAllNoShowT gen
    let
        forward = deBruijnTerm
        backward
            :: Either FreeVariableError (Term NamedTyDeBruijn NamedDeBruijn DefaultUni DefaultFun a)
            -> Either FreeVariableError (Term TyName Name DefaultUni DefaultFun a)
        backward e = e >>= (\t -> runQuoteT $ unDeBruijnTerm t)
    Hedgehog.tripping body forward backward

test_DeBruijnInteresting :: TestTree
test_DeBruijnInteresting =
    testGroup "de Bruijn transformation round-trip" $
        fromInterestingTermGens $ \name -> testProperty name . prop_DeBruijn

test_mangle :: TestTree
test_mangle = testProperty "equality does not survive mangling" . property $ do
    (term, termMangled) <- forAll . Gen.just . runAstGen $ do
        term <- AST.genTerm
        mayTermMang <- mangleNames term
        pure $ do
            termMang <- mayTermMang
            Just (term, termMang)
    Hedgehog.assert $ term /= termMangled && termMangled /= term

-- See Note [Marking].
-- | A version of 'RenameT' that fails to take free variables into account.
newtype NoMarkRenameT ren m a = NoMarkRenameT
    { unNoMarkRenameT :: RenameT ren m a
    } deriving newtype
        ( Functor, Applicative, Alternative, Monad
        , MonadReader ren
        , MonadQuote
        )

noMarkRenameProgram
    :: (MonadQuote m, HasUniques (Program tyname name uni fun ann))
    => Program tyname name uni fun ann -> m (Program tyname name uni fun ann)
noMarkRenameProgram = runRenameT . unNoMarkRenameT . renameProgramM

-- | A version of 'RenameT' that does not perform any renaming at all.
newtype NoRenameT ren m a = NoRenameT
    { unNoRenameT :: m a
    } deriving newtype
        ( Functor, Applicative, Alternative, Monad
        , MonadQuote
        )

instance (Monad m, Monoid ren) => MonadReader ren (NoRenameT ren m) where
    ask = pure mempty
    local _ = id

noRenameProgram
    :: (MonadQuote m, HasUniques (Program tyname name uni fun ann))
    => Program tyname name uni fun ann -> m (Program tyname name uni fun ann)
noRenameProgram = through markNonFreshProgram >=> unNoRenameT . renameProgramM

-- | A broken version of 'RenameT' whose 'local' updates the scope globally (as opposed to locally).
newtype BrokenRenameT ren m a = BrokenRenameT
    { unBrokenRenameT :: StateT ren m a
    } deriving newtype
        ( Functor, Applicative, Alternative, Monad
        , MonadState ren
        , MonadQuote
        )

instance Monad m => MonadReader ren (BrokenRenameT ren m) where
    ask = get
    local f a = modify f *> a

runBrokenRenameT :: (Monad m, Monoid ren) => BrokenRenameT ren m a -> m a
runBrokenRenameT = flip evalStateT mempty . unBrokenRenameT

brokenRenameProgram
    :: (MonadQuote m, HasUniques (Program tyname name uni fun ann))
    => Program tyname name uni fun ann -> m (Program tyname name uni fun ann)
brokenRenameProgram = through markNonFreshProgram >=> runBrokenRenameT . renameProgramM

-- | Check that the given 'Property' fails.
checkBadRename :: Property -> IO ()
checkBadRename prop = check prop >>= \res -> res @?= False

-- | Test equality of a program and its renamed version, given a renamer.
prop_equalityFor
    :: program ~ Program TyName Name DefaultUni DefaultFun ()
    => (program -> Quote program) -> Property
prop_equalityFor ren = property $ do
    prog <- forAllPretty $ runAstGen genProgram
    let progRen = runQuote $ ren prog
    Hedgehog.assert $ progRen == prog && prog == progRen

test_equalityRename :: TestTree
test_equalityRename =
    testProperty "equality survives renaming" $
        prop_equalityFor rename

test_equalityBrokenRename :: TestTree
test_equalityBrokenRename =
    testCase "no marking in renaming destroys scoping" $
        checkBadRename $ prop_equalityFor brokenRenameProgram

test_equalityNoMarkRename :: TestTree
test_equalityNoMarkRename =
    testCase "equality does not survive renaming without marking" $
        checkBadRename $ prop_equalityFor noMarkRenameProgram

-- | Test scoping for a renamer.
prop_scopingFor
    :: program ~ Program TyName Name DefaultUni DefaultFun NameAnn
    => (program -> Quote program) -> Property
prop_scopingFor ren = property $ do
    prog <- forAllPretty $ runAstGen genProgram
    case checkRespectsScoping (runQuote . ren) prog of
        Left err -> fail $ show err
        Right () -> success

test_scopingRename :: TestTree
test_scopingRename =
    testProperty "renaming does not destroy scoping" $
        prop_scopingFor rename

test_scopingBrokenRename :: TestTree
test_scopingBrokenRename =
    testCase "wrong renaming destroys scoping" $
        checkBadRename $ prop_scopingFor brokenRenameProgram

test_scopingNoRename :: TestTree
test_scopingNoRename =
    testCase "no renaming does not result in global uniqueness" $
        checkBadRename $ prop_scopingFor noRenameProgram

test_scopingNoMarkRename :: TestTree
test_scopingNoMarkRename =
    testCase "equality does not survive wrong renaming" $
        checkBadRename $ prop_scopingFor noMarkRenameProgram

test_alphaEquality :: TestTree
test_alphaEquality = testCase "alphaEquality" $ do
    let
        xName = Name "x" (Unique 0)
        yName = Name "y" (Unique 1)

        varX = Var () xName
        varY = Var () yName

        varType = TyVar () (TyName (Name "a" (Unique 2)))

        lamX = LamAbs () xName varType varX
        lamY = LamAbs () yName varType varY

        term0, term1 :: Term TyName Name DefaultUni DefaultFun ()

        -- [(lam x a x) x]
        term0 = Apply () lamX varX
        -- [(lam y a y) x]
        term1 = Apply () lamY varX

    term0 @?= term1

test_rebindShadowedVariable :: TestTree
test_rebindShadowedVariable = testCase "rebindShadowedVariable" $ do
    let xName = TyName (Name "x" (Unique 0))
        yName = TyName (Name "y" (Unique 1))
        zName = TyName (Name "z" (Unique 2))

        varX = TyVar () xName
        varY = TyVar () yName
        varZ = TyVar () zName

        typeKind = Type ()

        l1, r1, l2, r2 :: Type TyName DefaultUni ()

        -- (all x (type) (fun (all y (type) y) x))
        l1 = TyForall () xName typeKind (TyFun () (TyForall () yName typeKind varY) varX)
        -- (all x (type) (fun (all x (type) x) x))
        r1 = TyForall () xName typeKind (TyFun () (TyForall () xName typeKind varX) varX)

        -- (all x (type) (all x (type) (fun x x)))
        l2 = TyForall () xName typeKind (TyForall () xName typeKind (TyFun () varX varX))
        -- (all y (type) (all z (type) (fun y z)))
        r2 = TyForall () yName typeKind (TyForall () zName typeKind (TyFun () varY varZ))

        err = concat
            [ displayPlcDebug l2
            , " and "
            , displayPlcDebug r2
            , " are supposed not to be equal, but they are equal"
            ]

    l1 @?= r1
    assertBool err $ l2 /= r2

test_rebindCapturedVariable :: TestTree
test_rebindCapturedVariable = testCase "rebindCapturedVariable" $ do
    let wName = TyName (Name "w" (Unique 0))
        xName = TyName (Name "x" (Unique 1))
        yName = TyName (Name "y" (Unique 2))
        zName = TyName (Name "z" (Unique 3))

        varW = TyVar () wName
        varX = TyVar () xName
        varY = TyVar () yName
        varZ = TyVar () zName

        typeKind = Type ()

        typeL1, typeR1, typeL2, typeR2 :: Type TyName DefaultUni ()

        -- (all y (type) (all z (type) (fun y z)))
        typeL1 = TyForall () yName typeKind (TyForall () zName typeKind (TyFun () varY varZ))
        -- (all x (type) (all y (type) (fun x y)))
        typeR1 = TyForall () xName typeKind (TyForall () yName typeKind (TyFun () varX varY))

        -- (all z (type) (fun (all w (all x (type) (fun w x))))) z)
        typeL2
            = TyForall () zName typeKind
            $ TyFun ()
                (TyForall () wName typeKind $ TyForall () xName typeKind (TyFun () varW varX))
                varZ
        -- (all x (type) (fun (all x (all y (type) (fun x y))))) x)
        typeR2
            = TyForall () xName typeKind
            $ TyFun ()
                (TyForall () xName typeKind $ TyForall () yName typeKind (TyFun () varX varY))
                varX

    [typeL1, typeL2] @?= [typeR1, typeR2]

test_names :: TestTree
test_names =
    testGroup "names"
        [ test_DeBruijnInteresting
        , test_mangle
        , test_equalityRename
        , test_equalityBrokenRename
        , test_equalityNoMarkRename
        , test_scopingRename
        , test_scopingBrokenRename
        , test_scopingNoRename
        , test_scopingNoMarkRename
        , test_alphaEquality
        , test_rebindShadowedVariable
        , test_rebindCapturedVariable
        ]
