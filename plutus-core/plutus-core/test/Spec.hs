{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main
    ( main
    ) where

import           PlutusPrelude

import qualified Check.Spec                        as Check
import           CostModelInterface.Spec
import           Evaluation.Spec                   (test_evaluation)
import           Normalization.Check
import           Normalization.Type
import           Pretty.Readable
import           TypeSynthesis.Spec                (test_typecheck)

import           PlutusCore
import           PlutusCore.DeBruijn
import           PlutusCore.Generators
import           PlutusCore.Generators.AST         as AST
import           PlutusCore.Generators.Interesting
import qualified PlutusCore.Generators.NEAT.Spec   as NEAT
import           PlutusCore.Generators.Scoping
import           PlutusCore.Mark
import           PlutusCore.MkPlc
import           PlutusCore.Pretty
import           PlutusCore.Rename.Internal

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.Text                         as T
import           Data.Text.Encoding                (encodeUtf8)
import           Flat                              (flat)
import qualified Flat
import           Hedgehog                          hiding (Var)
import qualified Hedgehog.Gen                      as Gen
import qualified Hedgehog.Range                    as Range
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

main :: IO ()
main = do
    plcFiles <- findByExtension [".plc"] "plutus-core/test/data"
    rwFiles <- findByExtension [".plc"] "plutus-core/test/scopes"
    typeFiles <- findByExtension [".plc"] "plutus-core/test/types"
    typeErrorFiles <- findByExtension [".plc"] "plutus-core/test/type-errors"
    defaultMain (allTests plcFiles rwFiles typeFiles typeErrorFiles)

compareName :: Name -> Name -> Bool
compareName = (==) `on` nameString

compareTyName :: TyName -> TyName -> Bool
compareTyName (TyName n) (TyName n') = compareName n n'

compareTerm
    :: (GEq uni, Closed uni, uni `Everywhere` Eq, Eq fun, Eq a)
    => Term TyName Name uni fun a -> Term TyName Name uni fun a -> Bool
compareTerm (Var _ n) (Var _ n')                   = compareName n n'
compareTerm (TyAbs _ n k t) (TyAbs _ n' k' t')     = compareTyName n n' && k == k' && compareTerm t t'
compareTerm (LamAbs _ n ty t) (LamAbs _ n' ty' t') = compareName n n' && compareType ty ty' && compareTerm t t'
compareTerm (Apply _ t t'') (Apply _ t' t''')      = compareTerm t t' && compareTerm t'' t'''
compareTerm (Constant _ x) (Constant _ y)          = x == y
compareTerm (Builtin _ bi) (Builtin _ bi')         = bi == bi'
compareTerm (TyInst _ t ty) (TyInst _ t' ty')      = compareTerm t t' && compareType ty ty'
compareTerm (Unwrap _ t) (Unwrap _ t')             = compareTerm t t'
compareTerm (IWrap _ pat1 arg1 t1) (IWrap _ pat2 arg2 t2) =
    compareType pat1 pat2 && compareType arg1 arg2 && compareTerm t1 t2
compareTerm (Error _ ty) (Error _ ty')             = compareType ty ty'
compareTerm _ _                                    = False

compareType
    :: (GEq uni, Closed uni, uni `Everywhere` Eq, Eq a)
    => Type TyName uni a -> Type TyName uni a -> Bool
compareType (TyVar _ n) (TyVar _ n')                  = compareTyName n n'
compareType (TyFun _ t s) (TyFun _ t' s')             = compareType t t' && compareType s s'
compareType (TyIFix _ pat1 arg1) (TyIFix _ pat2 arg2) = compareType pat1 pat2 && compareType arg1 arg2
compareType (TyForall _ n k t) (TyForall _ n' k' t')  = compareTyName n n' && k == k' && compareType t t'
compareType (TyBuiltin _ x) (TyBuiltin _ y)           = x == y
compareType (TyLam _ n k t) (TyLam _ n' k' t')        = compareTyName n n' && k == k' && compareType t t'
compareType (TyApp _ t t') (TyApp _ t'' t''')         = compareType t t'' && compareType t' t'''
compareType _ _                                       = False

compareProgram
    :: (GEq uni, Closed uni, uni `Everywhere` Eq, Eq fun, Eq a)
    => Program TyName Name uni fun a -> Program TyName Name uni fun a -> Bool
compareProgram (Program _ v t) (Program _ v' t') = v == v' && compareTerm t t'

-- | A 'Program' which we compare using textual equality of names rather than alpha-equivalence.
newtype TextualProgram a = TextualProgram { unTextualProgram :: Program TyName Name DefaultUni DefaultFun a } deriving Show

instance Eq a => Eq (TextualProgram a) where
    (TextualProgram p1) == (TextualProgram p2) = compareProgram p1 p2

propFlat :: Property
propFlat = property $ do
    prog <- forAllPretty $ runAstGen genProgram
    Hedgehog.tripping prog Flat.flat Flat.unflat

{- The lexer contains some quite complex regular expressions for literal
  constants, allowing escape sequences inside quoted strings among other
  things.  The lexer returns 'TkLiteralConst' tokens and then individual
  built-in types interpret these using their own parsing functions via the
  'Parsable' class.  The following tests check that (A) the lexer/parser can
  handle the output of the prettyprinter on constants from types in the default
  universe, and (B) that parsing is left inverse to printing for both constants
  and programs.  We have unit tests for the unit and boolean types, and property
  tests for the full set of types in the default universe. -}

type DefaultTerm  a = Term TyName Name DefaultUni DefaultFun a
type DefaultError a = Error DefaultUni DefaultFun a

parseTm :: BSL.ByteString -> Either (DefaultError AlexPosn) (DefaultTerm AlexPosn)
parseTm = runQuote . runExceptT . parseTerm @(DefaultError AlexPosn)

reprint :: PrettyPlc a => a -> BSL.ByteString
reprint = BSL.fromStrict . encodeUtf8 . displayPlcDef

{-| Test that the lexer/parser can successfully consume the output from the
   prettyprinter for the unit and boolean types.  We use a unit test here
   because there are only three possiblities (@()@, @false@, and @true@). -}
testLexConstant :: Assertion
testLexConstant =
    mapM_ (\t -> (fmap void . parseTm . reprint $ t) @?= Right t) smallConsts
        where
          smallConsts :: [DefaultTerm ()]
          smallConsts =
              [ mkConstant () ()
              , mkConstant () False
              , mkConstant () True
              ]

{- Generate constant terms for each type in the default universe. The lexer should
  be able to consume escape sequences in characters and strings, both standard
  ASCII escape sequences and Unicode ones.  Hedgehog has generators for both of
  these, but the Unicode one essentially never generates anything readable: all
  of the output looks like '\857811'.  To get good coverage of the different
  possible formats we have generators for Unicode characters and ASCII
  characters, and also Latin-1 ones (characters 0-255, including standard ASCII
  from 0-127); there is also a generator for UTF8-encoded Unicode. -}
-- TODO: replace PlutusCore.Generators.AST.genConstant with this.  We
-- can't do that at the moment because genConstant is used by the tests for the
-- plutus-ir parser, and that can't handle the full range of constants at the
-- moment.
genConstantForTest :: AstGen (Some (ValueOf DefaultUni))
genConstantForTest = Gen.frequency
    [ (3,  someValue <$> pure ())
    , (3,  someValue <$> Gen.bool)
    , (5,  someValue <$> Gen.integral (Range.linear (-k1) k1)) -- Smallish Integers
    , (5,  someValue <$> Gen.integral (Range.linear (-k2) k2)) -- Big Integers, generally not Ints
    , (10, someValue <$> Gen.ascii)                            -- Character: 'c', '\n', '\SYN' etc.
    , (3,  someValue <$> Gen.latin1)                           -- Character: ascii and '\128'..'\255'
    , (3,  someValue <$> Gen.unicode)   -- Unicode character: typically '\857811' etc. Almost never generates anything readable.
    , (10, someValue <$> Gen.string (Range.linear 0 100) Gen.ascii)    -- eg "\SOc_\t\GS'v\DC4FP@-pN`\na\SI\r"
    , (3,  someValue <$> Gen.string (Range.linear 0 100) Gen.latin1)   -- eg "\246'X\b<\195]\171Y"
    , (3,  someValue <$> Gen.utf8   (Range.linear 0 100) Gen.unicode)  -- eg "\243\190\180\141"
    , (3,  someValue <$> Gen.string (Range.linear 0 100) Gen.unicode)  -- eg "\1108177\609033\384623"
    , (10, someValue <$> Gen.bytes  (Range.linear 0 100))              -- Bytestring
    ]
    where k1 = 1000000 :: Integer
          k2 = m*m
          m = fromIntegral (maxBound::Int) :: Integer

{-| Check that printing followed by parsing is the identity function on
  constants.  This is quite fast, so we do it 1000 times to get good coverage
  of the various generators. -}
propLexConstant :: Property
propLexConstant = withTests (1000 :: Hedgehog.TestLimit) . property $ do
    term <- forAllPretty $ Constant () <$> runAstGen genConstantForTest
    Hedgehog.tripping term reprint (fmap void . parseTm)

-- | Generate a random 'Program', pretty-print it, and parse the pretty-printed
-- text, hopefully returning the same thing.
propParser :: Property
propParser = property $ do
    prog <- TextualProgram <$> forAllPretty (runAstGen genProgram)
    Hedgehog.tripping prog (reprint . unTextualProgram)
                (\p -> fmap (TextualProgram . void) $ runQuote $ runExceptT $ parseProgram @(DefaultError AlexPosn) p)

propMangle :: Property
propMangle = property $ do
    (term, termMangled) <- forAll . Gen.just . runAstGen $ do
        term <- AST.genTerm
        mayTermMang <- mangleNames term
        pure $ do
            termMang <- mayTermMang
            Just (term, termMang)
    Hedgehog.assert $ term /= termMangled && termMangled /= term

newtype RenameT_BAD ren m a = RenameT_BAD
    { unRenameT_BAD :: StateT ren m a
    } deriving newtype
        ( Functor, Applicative, Alternative, Monad
        , MonadState ren
        , MonadQuote
        )

runRenameT_BAD :: (Monad m, Monoid ren) => RenameT_BAD ren m a -> m a
runRenameT_BAD = flip evalStateT mempty . unRenameT_BAD

instance Monad m => MonadReader ren (RenameT_BAD ren m) where
    ask = get
    local f a = modify f *> a

renameProgram_BAD
    :: (MonadQuote m, HasUniques (Program tyname name uni fun ann))
    => Program tyname name uni fun ann -> m (Program tyname name uni fun ann)
renameProgram_BAD = through markNonFreshProgram >=> runRenameT_BAD . renameProgramM

checkBadRenamer :: Property -> IO ()
checkBadRenamer prop = check prop >>= \res -> res @?= False

propRenameFor
    :: program ~ Program TyName Name DefaultUni DefaultFun ()
    => (program -> Quote program) -> Property
propRenameFor ren = property $ do
    prog <- forAllPretty $ runAstGen genProgram
    let progRen = runQuote $ ren prog
    Hedgehog.assert $ progRen == prog && prog == progRen

propRename :: Property
propRename = propRenameFor rename

propRenameBadRenamer :: IO ()
propRenameBadRenamer = checkBadRenamer $ propRenameFor renameProgram_BAD

propScopingFor
    :: program ~ Program TyName Name DefaultUni DefaultFun NameAnn
    => (program -> Quote program) -> Property
propScopingFor ren = property $ do
    prog <- forAllPretty $ runAstGen genProgram
    case checkRespectsScoping (runQuote . ren) prog of
        Left err -> fail $ show err
        Right () -> success

propScoping :: Property
propScoping = propScopingFor rename

propScopingBadRenamer :: IO ()
propScopingBadRenamer = checkBadRenamer $ propScopingFor renameProgram_BAD

propDeBruijn :: Gen (TermOf (DefaultTerm ()) a) -> Property
propDeBruijn gen = property . generalizeT $ do
    (TermOf body _) <- forAllNoShowT gen
    let
        forward = deBruijnTerm
        backward
            :: Except FreeVariableError (Term NamedTyDeBruijn NamedDeBruijn DefaultUni DefaultFun a)
            -> Except FreeVariableError (DefaultTerm a)
        backward e = e >>= (\t -> runQuoteT $ unDeBruijnTerm t)
    Hedgehog.tripping body forward backward

allTests :: [FilePath] -> [FilePath] -> [FilePath] -> [FilePath] -> TestTree
allTests plcFiles rwFiles typeFiles typeErrorFiles =
  testGroup "all tests"
    [ tests
    , testCase "lexing constants from small types" testLexConstant
    , testProperty "lexing constants" propLexConstant
    , testProperty "parser round-trip" propParser
    , testProperty "serialization round-trip (Flat)" propFlat
    , testProperty "equality does not survive mangling" propMangle
    , testProperty "equality survives renaming" propRename
    , testCase "equality does not survive wrong renaming" propRenameBadRenamer
    , testProperty "renaming does not destroy scoping" propScoping
    , testCase "wrong renaming destroys scoping" propScopingBadRenamer
    , testGroup "de Bruijn transformation round-trip" $
          fromInterestingTermGens $ \name -> testProperty name . propDeBruijn
    , testsGolden plcFiles
    , testsRewrite rwFiles
    , testsType typeFiles
    , testsType typeErrorFiles
    , test_Pretty
    , test_typeNormalization
    , test_typecheck
    , test_evaluation
    , test_normalizationCheck
    , test_costModelInterface
    , Check.tests
    , NEAT.tests NEAT.defaultGenOptions
    ]


type TestFunction a = BSL.ByteString -> Either (DefaultError a) T.Text

asIO :: Pretty a => TestFunction a -> FilePath -> IO BSL.ByteString
asIO f = fmap (either errorgen (BSL.fromStrict . encodeUtf8) . f) . BSL.readFile

errorgen :: PrettyPlc a => a -> BSL.ByteString
errorgen = BSL.fromStrict . encodeUtf8 . displayPlcDef

asGolden :: Pretty a => TestFunction a -> TestName -> TestTree
asGolden f file = goldenVsString file (file ++ ".golden") (asIO f file)

-- TODO: evaluation tests should go under the 'Evaluation' module,
-- normalization tests -- under 'Normalization', etc.

testsType :: [FilePath] -> TestTree
testsType = testGroup "golden type synthesis tests" . fmap (asGolden printType)

testsGolden :: [FilePath] -> TestTree
testsGolden
    = testGroup "golden tests"
    . fmap (asGolden (format $ defPrettyConfigPlcClassic defPrettyConfigPlcOptions))

testsRewrite :: [FilePath] -> TestTree
testsRewrite
    = testGroup "golden rewrite tests"
    . fmap (asGolden (format $ debugPrettyConfigPlcClassic defPrettyConfigPlcOptions))

testEqTerm :: Bool
testEqTerm =
    let
        xName = Name "x" (Unique 0)
        yName = Name "y" (Unique 1)

        varX = Var () xName
        varY = Var () yName

        varType = TyVar () (TyName (Name "a" (Unique 2)))

        lamX = LamAbs () xName varType varX
        lamY = LamAbs () yName varType varY

        term0, term1 :: DefaultTerm ()

        -- [(lam x a x) x]
        term0 = Apply () lamX varX
        -- [(lam y a y) x]
        term1 = Apply () lamY varX

    in
        term0 == term1

testRebindShadowedVariable :: Bool
testRebindShadowedVariable =
    let
        xName = TyName (Name "x" (Unique 0))
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

    in
        l1 == r1 && l2 /= r2

testRebindCapturedVariable :: Bool
testRebindCapturedVariable =
    let
        wName = TyName (Name "w" (Unique 0))
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
    in [typeL1, typeL2] == [typeR1, typeR2]

tests :: TestTree
tests = testCase "example programs" $ fold
    [ fmt "(program 0.1.0 [(builtin addInteger) x y])" @?= Right "(program 0.1.0\n  [ [ (builtin addInteger) x ] y ]\n)"
    , fmt "(program 0.1.0 doesn't)" @?= Right "(program 0.1.0\n  doesn't\n)"
    , fmt "{- program " @?= Left (LexErr "Error in nested comment at line 1, column 12")
    , testRebindShadowedVariable @?= True
    , testRebindCapturedVariable @?= True
    , testEqTerm @?= True
    ]
    where
        fmt :: BSL.ByteString -> Either (ParseError AlexPosn) T.Text
        fmt = format cfg
        cfg = defPrettyConfigPlcClassic defPrettyConfigPlcOptions
