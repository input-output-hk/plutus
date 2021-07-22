{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# LANGUAGE StrictData             #-}

{- Note [Strict Data for budgeting]

Without the StrictData pragma here, we get a memory leak during evaluation
because large unevaluated arthimetic expressions build up.  Strictness is only
really required for ExBudget, but it's simpler if we jut make
everything strict, and it doesn't seem to do any harm.
-}

-- TODO: revise this.
{- Note [Budgeting]

When running Plutus code on the chain, you're running code on other peoples
machines, so you'll have to pay for it. And it has to be possible to determine
how much money that should be before sending the transaction with the Plutus
code to the chain. If you spend too little, your transaction will be rejected.
If you spend too much, you're wasting money. So it must be possible to estimate
how much a script will cost. The easiest way to do so is to run the script
locally and determine the cost. The functional nature of Plutus allows for the
assumption it will cost a similar amount locally as on the chain. See
'CekBudgetMode'.

Additionally, it's helpful to know which parts of the script cost how much. We
assume it's useful to have a list of costs per term executed, so it's possible
to estimate which parts of the script cost how much. The 'ExTally' has not been
determined to be useful, but it was easy to implement.

We're tracking execution cost via both memory (via 'ExMemory') and CPU (via
'ExCPU'). Node operators are more interested in space limits than time limits -
the memory upper limit will be reached faster than the time limit (which would
be until next block). The two resources are then converted to the main currency
of the chain based on protocol parameters - that way it's possible to adjust the
actual fees without changing the code.

When tracking memory, we ignore garbage collection - only total memory
allocation is counted. This decision decouples us from the implementation of the
GC itself. Additionally, sharing of references is assumed. If a builtin
generates a new value, every reference of that value (e.g. in different CEK
environments) is assumed to point to the same value, without any copies. So the
total memory of the program is bounded to the original program + anything the
builtins produce + the machine space used by the CEK machine itself. The CEK
environment costs are included in the stack frame costs of the CEK machine,
they're linear.

The tracking of the costs themselves does not cost any CPU or memory. Currently
that's an implementation detail. We may have to readjust this depending on real
world experience.

The CEK machine does budgeting in these steps:
- The memory cost of the initial AST is added to the budget. See Note [Memory
  Usage for Plutus]. This operation currently does not cost any CPU. It
  currently costs as much memory as the AST itself, before aborting. See
  https://github.com/input-output-hk/plutus/issues/1799 for more discussion.
- Then each machine reduction step requires a certain amount of memory and CPU.
- The builtin operations may require different amounts of memory and CPU,
  depending on the input size.
- If a computation runs out of Memory or CPU, it is aborted, via the same
  mechanism when 'error' is called.

Tracking CEK machine layers is rather straightforward, albeit these numbers
still have to be filled in. For builtins (e.g. +, etc.) the cost tracking can be
a bit more complicated, as the required resources may depend on the size of the
inputs (E.g. multiplying numbers, where the output will be around 6 words if
both inputs are at 3 words each). These cost estimations will also have factors
attached which can be configured at runtime via protocol parameters - so it's
possible to adjust them at runtime.

-}

{-| Note [Budgeting units]

 We use picoseconds for measuring times and words for measuring memory usage.
 Some care is required with time units because different units are used in
 different places.

 * The basic data for models of execution time is produced by Criterion
   benchmarks (run via plutus-core:cost-model-budgeting-bench) and saved in
   'benching.csv'.  At this point the time units are seconds.

 * The data in 'benching.csv' is used by plutus-core:update-cost-model to create
   cost-prediction models for the built-in functions, and data describing these
   is written to builtinCostModel.json.  This process involves several steps:

     * The CostModelCreation module reads in the data from 'benching.csv' and
       runs R code in 'models.R' to fit linear models to the benchmark results
       for each builtin.  This process (and its results) necessarily invloves
       the use of floating-point numbers.

       Builtin execution times are typically of the order of 10^(-6) or 10^(-7)
       seconds, and the benching data is converted to milliseconds in 'models.R'
       because it's sometimes useful to work with the data interactively and this
       makes the numbers a lot more human-readable.

     * The coefficents from the R models are returned to the Haskell code in
       CostModelCreation and written out to costModel.json.  To avoid the use of
       floats in JSON and in cost prediction at runtime (which might be
       machine-dependent if floats were used), numbers are multiplied by 10^6
       and rounded to the nearest integer, shfting from the millisecond scale to
       the picosecond scale.  This rescaling is safe because all of our models
       are (currently) linear in their inputs.

 * When the Plutus Core evaluator is compiled, the JSON data in
   'builtinCostModel.json' is read in and used to create the defaultCostModel
   object.  This also includes information about the costs of basic CEK machine
   operations obtained from 'cekMachineCosts.json' (currently generated manually).

 * When the Plutus Core evaluator is run, the code in
   PlutusCore.Evaluation.Machine.BuiltinCostModel uses the data in
   defaultCostModel to create Haskell versions of the cost models which estimate
   the execution time of a built-in function given the sizes of its inputs.
   This (and the memory usage) are fed into a budgeting process which measures
   the ongoing resource consumption during script execution.

   All budget calculations are (at least on 64-bit machines) done using the
   'SatInt' type which deals with overflow by truncating excessivly large values
   to the maximum 'SatInt' value, 2^63-1.  In picoseconds this is about 106
   days, which should suffice for any code we expect to run.  Memory budgeting
   is entirely in terms of machine words, and floating-point issues are
   irrelevant.

 Some precision is lost during the conversion from R's floating-point models to
 the integral numbers used in the Haskell models.  However, experimentation
 shows that the difference is very small.  The tests in plutus-core:
 cost-model-test run the R models and the Haskell models with a large number of
 random inputs and check that they agree to within one part in 10,000, which
 is well within the accuracy we require for the cost model.
-}

module PlutusCore.Evaluation.Machine.ExBudget
    ( ExBudget(..)
    , ExBudgetBuiltin(..)
    , ExRestrictingBudget(..)
    , enormousBudget
    )
where

import           PlutusPrelude                          hiding (toList)

import           PlutusCore.Core

import           Data.Semigroup
import           Data.Text.Prettyprint.Doc
import           Deriving.Aeson
import           Language.Haskell.TH.Lift               (Lift)
import           PlutusCore.Evaluation.Machine.ExMemory

-- | A class for injecting a 'Builtin' into an @exBudgetCat@.
-- We need it, because the constant application machinery calls 'spendBudget' before reducing a
-- constant application and we want to be general over @exBudgetCat@ there, but still track the
-- built-in functions category, hence the ad hoc polymorphism.
class ExBudgetBuiltin fun exBudgetCat where
    exBudgetBuiltin :: BuiltinTag fun -> exBudgetCat

-- | A dummy 'ExBudgetBuiltin' instance to be used in monads where we don't care about costing.
instance ExBudgetBuiltin fun () where
    exBudgetBuiltin _ = ()

data ExBudget = ExBudget { _exBudgetCPU :: ExCPU, _exBudgetMemory :: ExMemory }
    deriving stock (Eq, Show, Generic, Lift)
    deriving anyclass (PrettyBy config, NFData)
    deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier (CamelToSnake)] ExBudget

-- These functions are performance critical, so we can't use GenericSemigroupMonoid, and we insist that they be inlined.
instance Semigroup ExBudget where
    {-# INLINE (<>) #-}
    (ExBudget cpu1 mem1) <> (ExBudget cpu2 mem2) = ExBudget (cpu1 <> cpu2) (mem1 <> mem2)
    -- This absolutely must be inlined so that the 'fromIntegral' calls can get optimized away, or it destroys performance
    {-# INLINE stimes #-}
    stimes r (ExBudget (ExCPU cpu) (ExMemory mem)) = ExBudget (ExCPU (fromIntegral r * cpu)) (ExMemory (fromIntegral r * mem))

instance Monoid ExBudget where
    mempty = ExBudget mempty mempty

instance Pretty ExBudget where
    pretty (ExBudget cpu memory) = parens $ fold
        [ "{ cpu: ", pretty cpu, line
        , "| mem: ", pretty memory, line
        , "}"
        ]

newtype ExRestrictingBudget = ExRestrictingBudget
    { unExRestrictingBudget :: ExBudget
    } deriving (Show, Eq)
      deriving newtype (Semigroup, Monoid)
      deriving newtype (Pretty, PrettyBy config, NFData)

-- | When we want to just evaluate the program we use the 'Restricting' mode with an enormous
-- budget, so that evaluation costs of on-chain budgeting are reflected accurately in benchmarks.
enormousBudget :: ExRestrictingBudget
enormousBudget = ExRestrictingBudget $ ExBudget (ExCPU maxInt) (ExMemory maxInt)
                 where maxInt = fromIntegral (maxBound ::Int)
