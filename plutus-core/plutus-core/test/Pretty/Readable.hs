module Pretty.Readable (test_Pretty) where

import           PlutusCore.Default
import           PlutusCore.FsTree
import           PlutusCore.Pretty

import           PlutusCore.Examples.Everything (examples)
import           PlutusCore.StdLib.Everything   (stdLib)

import           Common

import           Test.Tasty

prettyConfigReadable :: PrettyConfigPlc
prettyConfigReadable
    = PrettyConfigPlc defPrettyConfigPlcOptions
    . PrettyConfigPlcReadable
    $ botPrettyConfigReadable defPrettyConfigName ShowKindsYes

testReadable :: PrettyPlc a => TestName -> a -> TestNested
testReadable name = nestedGoldenVsDoc name . prettyBy prettyConfigReadable

test_PrettyReadable :: TestTree
test_PrettyReadable =
    testGroup "Bundles"
        [ folder stdLib
        , folder examples
        ]
  where
    folder :: (Pretty fun, Enum fun) => PlcFolderContents DefaultUni fun -> TestTree
    folder
        = runTestNestedIn ["plutus-core", "test", "Pretty", "Golden"]
        . testNested "Readable"
        . foldPlcFolderContents testNested testReadable testReadable

test_Pretty :: TestTree
test_Pretty =
    testGroup "pretty"
        [ test_PrettyReadable
        ]
