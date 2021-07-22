-- | This module exports all available examples via a data type which allows to test
-- various procedures (pretty-printing, type checking, etc) over the entire set of examples
-- in a convenient way: each time a function / data type is added to examples, none of the
-- tests is required to be adapted, instead you just add the new definition to 'examples'
-- defined below and all the tests see it automatically.

{-# LANGUAGE ScopedTypeVariables #-}

module PlutusCore.Examples.Everything
    ( examples
    , builtins
    ) where

import           PlutusPrelude

import           PlutusCore.Core
import           PlutusCore.Default
import           PlutusCore.FsTree
import           PlutusCore.MkPlc

import           PlutusCore.StdLib.Type

import           PlutusCore.Examples.Builtins
import           PlutusCore.Examples.Data.Data
import           PlutusCore.Examples.Data.InterList
import           PlutusCore.Examples.Data.List
import           PlutusCore.Examples.Data.Pair
import           PlutusCore.Examples.Data.Shad
import           PlutusCore.Examples.Data.TreeForest
import           PlutusCore.Examples.Data.Vec

-- | All examples exported as a single value.
examples :: PlcFolderContents DefaultUni (Either DefaultFun ExtensionFun)
examples =
    FolderContents
      [ treeFolderContents "Examples"
          [ treeFolderContents "Data"
              [ plcTermFile "ofoldrData" ofoldrData
              ]
          , treeFolderContents "InterList"
              [ plcTypeFile "InterList"      $ _recursiveType interListData
              , plcTermFile "InterNil"       interNil
              , plcTermFile "InterCons"      interCons
              , plcTermFile "FoldrInterList" foldrInterList
              ]
          , treeFolderContents "List"
              [ plcTermFile "omapList" omapList
              ]
          , treeFolderContents "Pair"
              [ plcTermFile "obothPair" obothPair
              ]
          , treeFolderContents "TreeForest"
              [ plcTypeFile "Tree"       $ _recursiveType treeData
              , plcTypeFile "Forest"     $ _recursiveType forestData
              , plcTermFile "TreeNode"   treeNode
              , plcTermFile "ForestNil"  forestNil
              , plcTermFile "ForestCons" forestCons
              ]
          , treeFolderContents "Vec"
              [ plcTypeFile "zeroT"            zeroT
              , plcTypeFile "succT"            succT
              , plcTypeFile "plusT"            plusT
              , plcTypeFile "churchVec"        churchVec
              , plcTermFile "churchNil"        churchNil
              , plcTermFile "churchCons"       churchCons
              , plcTermFile "churchConcat"     churchConcat
              , plcTypeFile "scottVec"         scottVec
              , plcTermFile "scottNil"         scottNil
              , plcTermFile "scottCons"        scottCons
              , plcTermFile "scottHead"        scottHead
              , plcTermFile "scottSumHeadsOr0" $ mapFun Left scottSumHeadsOr0
              ]
          , treeFolderContents "Shad"
              [ plcTypeFile "shad"   shad
              , plcTermFile "mkShad" mkShad
              ]
          , treeFolderContents "RecUnit"
              [ plcTypeFile "recUnit"    recUnit
              , plcTermFile "runRecUnit" runRecUnit
              ]
          ]
      ]

builtins :: PlcFolderContents DefaultUni ExtensionFun
builtins =
    FolderContents
        [ treeFolderContents "Builtins" $
            map (\fun -> plcTermFile (show fun) $ mkBuiltin () fun) enumeration
        ]
