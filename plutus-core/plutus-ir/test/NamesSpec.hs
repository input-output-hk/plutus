module NamesSpec
    ( names
    ) where

import           PlutusIR.Generators.AST
import           PlutusIR.Transform.Rename ()

import           PlutusCore.Check.Scoping
import           PlutusCore.Generators
import           PlutusCore.Quote
import           PlutusCore.Rename

import           Hedgehog
import           Test.Tasty
import           Test.Tasty.Hedgehog

scopingRename :: TestTree
scopingRename =
    testProperty "renaming does not destroy scoping" . withTests 1000 . property $ do
        prog <- forAllPretty $ runAstGen genProgram
        case checkRespectsScoping (runQuote . rename) prog of
            Left err -> fail $ show err
            Right () -> success

-- (program (let (nonrec) (termbind (strict) (vardecl a a) a) a))

names :: TestTree
names =
    testGroup "names"
        [ scopingRename
        ]
