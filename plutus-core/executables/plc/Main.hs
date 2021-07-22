{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module Main (main) where

import           Common
import           Parsers
import qualified PlutusCore                       as PLC
import qualified PlutusCore.Evaluation.Machine.Ck as Ck
import qualified PlutusCore.Pretty                as PP

import qualified UntypedPlutusCore                as UPLC (eraseProgram)

import           Data.Function                    ((&))
import           Data.Functor                     (void)
import qualified Data.Text.IO                     as T

import           Control.DeepSeq                  (rnf)
import           Options.Applicative
import           System.Exit                      (exitSuccess)

plcHelpText :: String
plcHelpText = helpText "Typed Plutus Core"

plcInfoCommand :: ParserInfo Command
plcInfoCommand = plutus plcHelpText

data TypecheckOptions = TypecheckOptions Input Format
data EvalOptions      = EvalOptions Input Format PrintMode TimingMode
data EraseOptions     = EraseOptions Input Format Output Format PrintMode


-- Main commands
data Command = Apply     ApplyOptions
             | Typecheck TypecheckOptions
             | Convert   ConvertOptions
             | Print     PrintOptions
             | Example   ExampleOptions
             | Erase     EraseOptions
             | Eval      EvalOptions

---------------- Option parsers ----------------

typecheckOpts :: Parser TypecheckOptions
typecheckOpts = TypecheckOptions <$> input <*> inputformat

eraseOpts :: Parser EraseOptions
eraseOpts = EraseOptions <$> input <*> inputformat <*> output <*> outputformat <*> printmode

evalOpts :: Parser EvalOptions
evalOpts =
  EvalOptions <$> input <*> inputformat <*> printmode <*> timingmode

plutus ::
  -- | The @helpText@
  String ->
  ParserInfo Command
plutus langHelpText =
    info
      (plutusOpts <**> helper)
      (fullDesc <> header "Typed Plutus Core Tool" <> progDesc langHelpText)

plutusOpts :: Parser Command
plutusOpts = hsubparser (
       command "apply"
           (info (Apply <$> applyOpts)
            (progDesc $ "Given a list of input scripts f g1 g2 ... gn, output a script consisting of (... ((f g1) g2) ... gn); "
            ++ "for example, 'plc apply --if flat Validator.flat Datum.flat Redeemer.flat Context.flat --of flat -o Script.flat'"))
    <> command "print"
           (info (Print <$> printOpts)
            (progDesc "Parse a program then prettyprint it."))
    <> command "convert"
           (info (Convert <$> convertOpts)
            (progDesc "Convert a program between various formats"))
    <> command "example"
           (info (Example <$> exampleOpts)
            (progDesc $ "Show a program example. "
                     ++ "Usage: first request the list of available examples (optional step), "
                     ++ "then request a particular example by the name of a term. "
                     ++ "Note that evaluating a generated example may result in 'Failure'."))
    <> command "typecheck"
           (info (Typecheck <$> typecheckOpts)
            (progDesc "Typecheck a typed Plutus Core program."))
    <> command "erase"
           (info (Erase <$> eraseOpts)
            (progDesc "Convert a typed Plutus Core program to an untyped one."))
    <> command "evaluate"
           (info (Eval <$> evalOpts)
            (progDesc "Evaluate a typed Plutus Core program using the CK machine."))
  )

---------------- Script application ----------------

-- | Apply one script to a list of others.
runApply :: ApplyOptions -> IO ()
runApply (ApplyOptions inputfiles ifmt outp ofmt mode) = do
  scripts <- mapM ((getProgram ifmt ::  Input -> IO (PlcProg PLC.AlexPosn)) . FileInput) inputfiles
  let appliedScript =
        case map (\case p -> () <$ p) scripts of
          []          -> errorWithoutStackTrace "No input files"
          progAndargs -> foldl1 PLC.applyProgram progAndargs
  writeProgram outp ofmt mode appliedScript

---------------- Typechecking ----------------

runTypecheck :: TypecheckOptions -> IO ()
runTypecheck (TypecheckOptions inp fmt) = do
  prog <- getProgram fmt inp
  case PLC.runQuoteT $ do
    tcConfig <- PLC.getDefTypeCheckConfig ()
    PLC.typecheckPipeline tcConfig (void prog)
    of
      Left (e :: PLC.Error PLC.DefaultUni PLC.DefaultFun ()) ->
        errorWithoutStackTrace $ PP.displayPlcDef e
      Right ty                                               ->
        T.putStrLn (PP.displayPlcDef ty) >> exitSuccess

---------------- Evaluation ----------------

runEval :: EvalOptions -> IO ()
runEval (EvalOptions inp ifmt printMode timingMode) = do
  prog <- getProgram ifmt inp
  let evaluate = Ck.evaluateCkNoEmit PLC.defaultBuiltinsRuntime
      term = void . PLC.toTerm $ prog
      !_ = rnf term
      -- Force evaluation of body to ensure that we're not timing parsing/deserialisation.
      -- The parser apparently returns a fully-evaluated AST, but let's be on the safe side.
  case timingMode of
    NoTiming -> evaluate term & handleEResult printMode
    Timing n -> timeEval n evaluate term >>= handleTimingResults term

----------------- Print examples -----------------------

runPlcPrintExample ::
    ExampleOptions -> IO ()
runPlcPrintExample = runPrintExample getPlcExamples

---------------- Erasure ----------------

-- | Input a program, erase the types, then output it
runErase :: EraseOptions -> IO ()
runErase (EraseOptions inp ifmt outp ofmt mode) = do
  typedProg <- (getProgram ifmt inp :: IO (PlcProg PLC.AlexPosn))
  let untypedProg = () <$ UPLC.eraseProgram typedProg
  case ofmt of
    Textual       -> writeToFileOrStd outp mode untypedProg
    Flat flatMode -> writeFlat outp flatMode untypedProg

---------------- Parse and print a PLC source file ----------------

runPrint :: PrintOptions -> IO ()
runPrint (PrintOptions inp mode) =
    (parseInput inp :: IO (PlcProg PLC.AlexPosn) ) >>= print . getPrintMethod mode

---------------- Conversions ----------------

-- | Convert between textual and FLAT representations.
runConvert :: ConvertOptions -> IO ()
runConvert (ConvertOptions inp ifmt outp ofmt mode) = do
    program <- (getProgram ifmt inp :: IO (PlcProg PLC.AlexPosn))
    writeProgram outp ofmt mode program

---------------- Driver ----------------
main :: IO ()
main = do
    options <- customExecParser (prefs showHelpOnEmpty) plcInfoCommand
    case options of
        Apply     opts -> runApply        opts
        Typecheck opts -> runTypecheck    opts
        Eval      opts -> runEval         opts
        Example   opts -> runPlcPrintExample opts
        Erase     opts -> runErase        opts
        Print     opts -> runPrint        opts
        Convert   opts -> runConvert      opts
