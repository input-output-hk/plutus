{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE StrictData           #-}

module PlutusCore.Evaluation.Machine.BuiltinCostModel
    ( BuiltinCostModel
    , BuiltinCostModelBase(..)
    , CostingFun(..)
    , ModelAddedSizes(..)
    , ModelSubtractedSizes(..)
    , ModelOrientation(..)
    , ModelLinearSize(..)
    , ModelMultipliedSizes(..)
    , ModelMinSize(..)
    , ModelMaxSize(..)
    , ModelSplitConst(..)
    , ModelOneArgument(..)
    , ModelTwoArguments(..)
    , ModelThreeArguments(..)
    , runCostingFunOneArgument
    , runCostingFunTwoArguments
    , runCostingFunThreeArguments
    , Hashable
    )
where

import           PlutusPrelude                          hiding (toList)

import           PlutusCore.Evaluation.Machine.ExBudget
import           PlutusCore.Evaluation.Machine.ExMemory

import           Barbies
import           Data.Aeson
import           Data.Default.Class
import           Data.Hashable
import qualified Data.Kind                              as Kind
import           Deriving.Aeson
import           Language.Haskell.TH.Syntax             hiding (Name, newName)

type BuiltinCostModel = BuiltinCostModelBase CostingFun

-- See  Note [Budgeting units] in ExBudgeting.hs

-- | The main model which contains all data required to predict the cost of
-- builtin functions. See Note [Creation of the Cost Model] for how this is
-- generated. Calibrated for the CEK machine.
data BuiltinCostModelBase f =
    BuiltinCostModelBase
    { paramAddInteger           :: f ModelTwoArguments
    , paramSubtractInteger      :: f ModelTwoArguments
    , paramMultiplyInteger      :: f ModelTwoArguments
    , paramDivideInteger        :: f ModelTwoArguments
    , paramQuotientInteger      :: f ModelTwoArguments
    , paramRemainderInteger     :: f ModelTwoArguments
    , paramModInteger           :: f ModelTwoArguments
    , paramPowModInteger        :: f ModelThreeArguments
    , paramInvertInteger        :: f ModelTwoArguments
    , paramProbablyPrimeInteger :: f ModelTwoArguments
    , paramLessThanInteger      :: f ModelTwoArguments
    , paramLessThanEqInteger    :: f ModelTwoArguments
    , paramGreaterThanInteger   :: f ModelTwoArguments
    , paramGreaterThanEqInteger :: f ModelTwoArguments
    , paramEqInteger            :: f ModelTwoArguments
    , paramConcatenate          :: f ModelTwoArguments
    , paramTakeByteString       :: f ModelTwoArguments
    , paramDropByteString       :: f ModelTwoArguments
    , paramSHA2                 :: f ModelOneArgument
    , paramSHA3                 :: f ModelOneArgument
    , paramVerifySignature      :: f ModelThreeArguments
    , paramEqByteString         :: f ModelTwoArguments
    , paramLtByteString         :: f ModelTwoArguments
    , paramGtByteString         :: f ModelTwoArguments
    , paramIfThenElse           :: f ModelThreeArguments
    }
    deriving (Generic, FunctorB, TraversableB, ConstraintsB)

deriving via CustomJSON '[FieldLabelModifier (StripPrefix "param", CamelToSnake)]
             (BuiltinCostModelBase CostingFun) instance ToJSON (BuiltinCostModelBase CostingFun)
deriving via CustomJSON '[FieldLabelModifier (StripPrefix "param", CamelToSnake)]
             (BuiltinCostModelBase CostingFun) instance FromJSON (BuiltinCostModelBase CostingFun)

type AllArgumentModels (constraint :: Kind.Type -> Kind.Constraint) f =
    (constraint (f ModelOneArgument), constraint (f ModelTwoArguments), constraint (f ModelThreeArguments))

-- HLS doesn't like the AllBF from Barbies.
deriving instance AllArgumentModels NFData  f => NFData  (BuiltinCostModelBase f)
deriving instance AllArgumentModels Default f => Default (BuiltinCostModelBase f)
deriving instance AllArgumentModels Lift    f => Lift    (BuiltinCostModelBase f)
deriving instance AllArgumentModels Show    f => Show    (BuiltinCostModelBase f)
deriving instance AllArgumentModels Eq      f => Eq      (BuiltinCostModelBase f)

-- TODO there's probably a nice way to abstract over the number of arguments here. Feel free to implement it.

data CostingFun model = CostingFun
    { costingFunCpu    :: model
    , costingFunMemory :: model
    }
    deriving (Show, Eq, Generic, Lift, Default, NFData)
    deriving (FromJSON, ToJSON) via CustomJSON
        '[FieldLabelModifier (StripPrefix "costingFun", CamelToSnake)] (CostingFun model)

data ModelOneArgument =
    ModelOneArgumentConstantCost CostingInteger
    | ModelOneArgumentLinearCost ModelLinearSize
    deriving (Show, Eq, Generic, Lift, NFData)
    deriving (FromJSON, ToJSON) via CustomJSON
        '[SumTaggedObject "type" "arguments", ConstructorTagModifier (StripPrefix "ModelOneArgument", CamelToSnake)] ModelOneArgument
instance Default ModelOneArgument where
    def = ModelOneArgumentConstantCost 0

runCostingFunOneArgument :: CostingFun ModelOneArgument -> ExMemory -> ExBudget
runCostingFunOneArgument
    (CostingFun cpu mem) mem1 =
        ExBudget (ExCPU $ runOneArgumentModel cpu mem1) (ExMemory $ runOneArgumentModel mem mem1)

runOneArgumentModel :: ModelOneArgument -> ExMemory -> CostingInteger
runOneArgumentModel (ModelOneArgumentConstantCost c) _ = c
runOneArgumentModel (ModelOneArgumentLinearCost (ModelLinearSize intercept slope _)) (ExMemory s) =
    s * slope + intercept

-- | s * (x + y) + I
data ModelAddedSizes = ModelAddedSizes
    { modelAddedSizesIntercept :: CostingInteger
    , modelAddedSizesSlope     :: CostingInteger
    } deriving (Show, Eq, Generic, Lift, NFData)
    deriving (FromJSON, ToJSON) via CustomJSON
        '[FieldLabelModifier (StripPrefix "modelAddedSizes", CamelToSnake)] ModelAddedSizes

-- | s * (x - y) + I
data ModelSubtractedSizes = ModelSubtractedSizes
    { modelSubtractedSizesIntercept :: CostingInteger
    , modelSubtractedSizesSlope     :: CostingInteger
    , modelSubtractedSizesMinimum   :: CostingInteger
    } deriving (Show, Eq, Generic, Lift, NFData)
    deriving (FromJSON, ToJSON) via CustomJSON
        '[FieldLabelModifier (StripPrefix "modelSubtractedSizes", CamelToSnake)] ModelSubtractedSizes

data ModelOrientation =
    ModelOrientationX
    | ModelOrientationY
    deriving (Show, Eq, Generic, Lift, NFData)
    deriving (FromJSON, ToJSON) via CustomJSON
        '[SumTaggedObject "type" "arguments", ConstructorTagModifier (StripPrefix "ModelOrientation", CamelToSnake)] ModelOrientation

data ModelLinearSize = ModelLinearSize
    { modelLinearSizeIntercept   :: CostingInteger
    , modelLinearSizeSlope       :: CostingInteger
    , modelLinearSizeOrientation :: ModelOrientation -- ^ x or y?
    } deriving (Show, Eq, Generic, Lift, NFData)
    deriving (FromJSON, ToJSON) via CustomJSON
        '[FieldLabelModifier (StripPrefix "modelLinearSize", CamelToSnake)] ModelLinearSize

-- | s * (x * y) + I
data ModelMultipliedSizes = ModelMultipliedSizes
    { modelMultipliedSizesIntercept :: CostingInteger
    , modelMultipliedSizesSlope     :: CostingInteger
    } deriving (Show, Eq, Generic, Lift, NFData)
    deriving (FromJSON, ToJSON) via CustomJSON
        '[FieldLabelModifier (StripPrefix "modelMultipliedSizes", CamelToSnake)] ModelMultipliedSizes

-- | s * min(x, y) + I
data ModelMinSize = ModelMinSize
    { modelMinSizeIntercept :: CostingInteger
    , modelMinSizeSlope     :: CostingInteger
    } deriving (Show, Eq, Generic, Lift, NFData)
    deriving (FromJSON, ToJSON) via CustomJSON
        '[FieldLabelModifier (StripPrefix "modelMinSize", CamelToSnake)] ModelMinSize

-- | s * max(x, y) + I
data ModelMaxSize = ModelMaxSize
    { modelMaxSizeIntercept :: CostingInteger
    , modelMaxSizeSlope     :: CostingInteger
    } deriving (Show, Eq, Generic, Lift, NFData)
    deriving (FromJSON, ToJSON) via CustomJSON
        '[FieldLabelModifier (StripPrefix "modelMaxSize", CamelToSnake)] ModelMaxSize

-- | (if (x > y) then s * (x + y) else 0) + I
data ModelSplitConst = ModelSplitConst
    { modelSplitConstIntercept :: CostingInteger
    , modelSplitConstSlope     :: CostingInteger
    } deriving (Show, Eq, Generic, Lift, NFData)
    deriving (FromJSON, ToJSON) via CustomJSON
        '[FieldLabelModifier (StripPrefix "ModelSplitConst", CamelToSnake)] ModelSplitConst

data ModelTwoArguments =
      ModelTwoArgumentsConstantCost    CostingInteger
    | ModelTwoArgumentsAddedSizes      ModelAddedSizes
    | ModelTwoArgumentsSubtractedSizes ModelSubtractedSizes
    | ModelTwoArgumentsMultipliedSizes ModelMultipliedSizes
    | ModelTwoArgumentsMinSize         ModelMinSize
    | ModelTwoArgumentsMaxSize         ModelMaxSize
    | ModelTwoArgumentsSplitConstMulti ModelSplitConst
    | ModelTwoArgumentsLinearSize      ModelLinearSize
    deriving (Show, Eq, Generic, Lift, NFData)
    deriving (FromJSON, ToJSON) via CustomJSON
        '[SumTaggedObject "type" "arguments", ConstructorTagModifier (StripPrefix "ModelTwoArguments", CamelToSnake)] ModelTwoArguments

instance Default ModelTwoArguments where
    def = ModelTwoArgumentsConstantCost 0

runCostingFunTwoArguments :: CostingFun ModelTwoArguments -> ExMemory -> ExMemory -> ExBudget
runCostingFunTwoArguments (CostingFun cpu mem) mem1 mem2 =
    ExBudget (ExCPU (runTwoArgumentModel cpu mem1 mem2)) (ExMemory (runTwoArgumentModel mem mem1 mem2))

runTwoArgumentModel :: ModelTwoArguments -> ExMemory -> ExMemory -> CostingInteger
runTwoArgumentModel
    (ModelTwoArgumentsConstantCost c) _ _ = c
runTwoArgumentModel
    (ModelTwoArgumentsAddedSizes (ModelAddedSizes intercept slope)) (ExMemory size1) (ExMemory size2) =
        (size1 + size2) * slope + intercept -- TODO is this even correct? If not, adjust the other implementations too.
runTwoArgumentModel
    (ModelTwoArgumentsSubtractedSizes (ModelSubtractedSizes intercept slope minSize)) (ExMemory size1) (ExMemory size2) =
        (max minSize (size1 - size2)) * slope + intercept
runTwoArgumentModel
    (ModelTwoArgumentsMultipliedSizes (ModelMultipliedSizes intercept slope)) (ExMemory size1) (ExMemory size2) =
        (size1 * size2) * slope + intercept
runTwoArgumentModel
    (ModelTwoArgumentsMinSize (ModelMinSize intercept slope)) (ExMemory size1) (ExMemory size2) =
        (min size1 size2) * slope + intercept
runTwoArgumentModel
    (ModelTwoArgumentsMaxSize (ModelMaxSize intercept slope)) (ExMemory size1) (ExMemory size2) =
        (max size1 size2) * slope + intercept
runTwoArgumentModel
    (ModelTwoArgumentsSplitConstMulti (ModelSplitConst intercept slope)) (ExMemory size1) (ExMemory size2) =
        x * slope + intercept
        where x = if size1 > size2 then size1 * size2 else 0
runTwoArgumentModel
    (ModelTwoArgumentsLinearSize (ModelLinearSize intercept slope ModelOrientationX)) (ExMemory size1) (ExMemory _) =
        size1 * slope + intercept
runTwoArgumentModel
    (ModelTwoArgumentsLinearSize (ModelLinearSize intercept slope ModelOrientationY)) (ExMemory _) (ExMemory size2) =
        size2 * slope + intercept

data ModelThreeArguments =
    ModelThreeArgumentsConstantCost CostingInteger
  | ModelThreeArgumentsAddedSizes ModelAddedSizes
    deriving (Show, Eq, Generic, Lift, NFData)
    deriving (FromJSON, ToJSON) via CustomJSON
        '[SumTaggedObject "type" "arguments", ConstructorTagModifier (StripPrefix "ModelThreeArguments", CamelToSnake)] ModelThreeArguments

instance Default ModelThreeArguments where
    def = ModelThreeArgumentsConstantCost 0

runThreeArgumentModel :: ModelThreeArguments -> ExMemory -> ExMemory -> ExMemory -> CostingInteger
runThreeArgumentModel (ModelThreeArgumentsConstantCost c) _ _ _ = c
runThreeArgumentModel (ModelThreeArgumentsAddedSizes (ModelAddedSizes intercept slope)) (ExMemory size1) (ExMemory size2) (ExMemory size3) =
    (size1 + size2 + size3) * slope + intercept

runCostingFunThreeArguments :: CostingFun ModelThreeArguments -> ExMemory -> ExMemory -> ExMemory -> ExBudget
runCostingFunThreeArguments (CostingFun cpu mem) mem1 mem2 mem3 =
    ExBudget (ExCPU $ runThreeArgumentModel cpu mem1 mem2 mem3) (ExMemory $ runThreeArgumentModel mem mem1 mem2 mem3)
