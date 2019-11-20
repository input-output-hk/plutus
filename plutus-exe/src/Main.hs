{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified Language.PlutusCore                        as PLC
import qualified Language.PlutusCore.CBOR                   as PLC ()
import qualified Language.PlutusCore.DeBruijn               as D
import qualified Language.PlutusCore.Evaluation.CkMachine   as PLC
import qualified Language.PlutusCore.Generators             as PLC
import qualified Language.PlutusCore.Generators.Interesting as PLC
import qualified Language.PlutusCore.Generators.Test        as PLC
import qualified Language.PlutusCore.Interpreter.CekMachine as PLC
import qualified Language.PlutusCore.Interpreter.LMachine   as PLC
import qualified Language.PlutusCore.Pretty                 as PLC
import qualified Language.PlutusCore.StdLib.Data.Bool       as PLC
import qualified Language.PlutusCore.StdLib.Data.ChurchNat  as PLC
import qualified Language.PlutusCore.StdLib.Data.Integer    as PLC
import qualified Language.PlutusCore.StdLib.Data.Unit       as PLC

import qualified Language.PlutusCore.Untyped.CBOR                   as U ()
import qualified Language.PlutusCore.Untyped.Evaluation.CkMachine   as U
import qualified Language.PlutusCore.Untyped.Pretty                 as U
import qualified Language.PlutusCore.Untyped.Term                   as U
import qualified Language.PlutusCore.Interpreter.Untyped.CekMachine as U


import           Codec.Serialise                            (serialise)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Except                 (runExceptT)
import           Data.Bifunctor                             (second)
import           Data.Foldable                              (traverse_)

import qualified Data.ByteString.Lazy                       as BSL
import qualified Data.ByteString.Lazy.Char8                 as BSC
import qualified Data.Text                                  as T
import           Data.Text.Encoding                         (encodeUtf8)
import qualified Data.Text.IO                               as T
import           Data.Text.Prettyprint.Doc

import           System.Exit

import           Options.Applicative

data Input = FileInput FilePath | StdInput

getInput :: Input -> IO String
getInput (FileInput s) = readFile s
getInput StdInput      = getContents

input :: Parser Input
input = fileInput <|> stdInput

fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input file" )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read from stdin" )

data NormalizationMode = Required | NotRequired deriving (Show, Read)
data TypecheckOptions = TypecheckOptions Input NormalizationMode
data EvalMode = CK | CEK | UCK | UCEK | L deriving (Show, Read, Eq)
data EvalOptions = EvalOptions Input EvalMode
type ExampleName = T.Text
data ExampleMode = ExampleSingle ExampleName | ExampleAvailable
newtype ExampleOptions = ExampleOptions ExampleMode
data SerialisationMode = Typed | TypedAnon | Untyped | UntypedAnon | UntypedAnon2 | UntypedAnonDeBruijn
data SerialisationOptions = SerialisationOptions Input SerialisationMode
data Command = Typecheck TypecheckOptions | Eval EvalOptions | Example ExampleOptions | Serialise SerialisationOptions

plutus :: ParserInfo Command
plutus = info (plutusOpts <**> helper) (progDesc "Plutus Core tool")

plutusOpts :: Parser Command
plutusOpts = hsubparser (
    command "typecheck" (info (Typecheck <$> typecheckOpts) (progDesc "Typecheck a Plutus Core program"))
    <> command "evaluate" (info (Eval <$> evalOpts) (progDesc "Evaluate a Plutus Core program"))
    <> command "example" (info (Example <$> exampleOpts) (progDesc "Show a Plutus Core program example. Usage: first request the list of available examples (optional step), then request a particular example by the name of a type/term. Note that evaluating a generated example may result in 'Failure'"))
    <> command "cbor" (info (Serialise <$> serialisationOpts) (progDesc "Parse a Plutus Core program and output CBOR to standard output. "))
  )

normalizationMode :: Parser NormalizationMode
normalizationMode = option auto
  (  long "normalized-types"
  <> metavar "MODE"
  <> value NotRequired
  <> showDefault
  <> help "Whether type annotations must be normalized or not (one of Required or NotRequired)" )

typecheckOpts :: Parser TypecheckOptions
typecheckOpts = TypecheckOptions <$> input <*> normalizationMode

evalMode :: Parser EvalMode
evalMode = option auto
  (  long "mode"
  <> short 'm'
  <> metavar "MODE"
  <> value CEK
  <> showDefault
  <> help "Evaluation mode (one of CK, CEK, UCK, UCEK, or L)" )

evalOpts :: Parser EvalOptions
evalOpts = EvalOptions <$> input <*> evalMode

exampleMode :: Parser ExampleMode
exampleMode = exampleAvailable <|> exampleSingle

exampleAvailable :: Parser ExampleMode
exampleAvailable = flag' ExampleAvailable
  (  long "available"
  <> short 'a'
  <> help "Show available examples")

exampleName :: Parser ExampleName
exampleName = strOption
  (  long "single"
  <> metavar "NAME"
  <> short 's'
  <> help "Show a single example")

exampleSingle :: Parser ExampleMode
exampleSingle = ExampleSingle <$> exampleName

exampleOpts :: Parser ExampleOptions
exampleOpts = ExampleOptions <$> exampleMode

serialisationOpts :: Parser SerialisationOptions
serialisationOpts = SerialisationOptions <$> input <*> serialisationMode

serialisationMode :: Parser SerialisationMode
serialisationMode = subparser (
    command "typed"         (info (pure Typed)        (progDesc "Output CBOR for typed AST"))
 <> command "typed-anon"    (info (pure TypedAnon)    (progDesc "Output CBOR for typed AST with empty names"))
 <> command "untyped"       (info (pure Untyped)      (progDesc "Output CBOR for type-erased AST"))
 <> command "untyped-anon"  (info (pure UntypedAnon)  (progDesc "Output CBOR for type-erased AST with empty names"))
 <> command "untyped-anon2" (info (pure UntypedAnon2) (progDesc "Output CBOR for type-erased AST with no names"))
 <> command "untyped-anon-debruijn" (info (pure UntypedAnonDeBruijn) (progDesc "Output CBOR for type-erased AST with anonymous deBruijn names"))
  )



runTypecheck :: TypecheckOptions -> IO ()
runTypecheck (TypecheckOptions inp mode) = do
    contents <- getInput inp
    let bsContents = (BSL.fromStrict . encodeUtf8 . T.pack) contents
    let cfg = PLC.defOnChainConfig & PLC.tccDoNormTypes .~ case mode of
                NotRequired -> True
                Required    -> False
    case (PLC.runQuoteT . PLC.parseTypecheck cfg) bsContents of
        Left (e :: PLC.Error PLC.AlexPosn) -> do
            T.putStrLn $ PLC.prettyPlcDefText e
            exitFailure
        Right ty -> do
            T.putStrLn $ PLC.prettyPlcDefText ty
            exitSuccess

runEval :: EvalOptions -> IO ()
runEval (EvalOptions inp mode) = do
    contents <- getInput inp
    let bsContents = (BSL.fromStrict . encodeUtf8 . T.pack) contents
    if (mode == UCK || mode == UCEK)
    then  
        let evalFn = case mode of
                       UCK  -> U.runCk . U.eraseProgram
                       UCEK -> U.unsafeRunCek mempty . U.eraseProgram
                       _ -> undefined  -- oh dear
        in case evalFn . void <$> PLC.runQuoteT (PLC.parseScoped bsContents) of
          Left (e :: PLC.Error PLC.AlexPosn) ->
              do
--              T.putStrLn $ U.prettyPlcDefText e
                putStrLn $ show e
                -- FIXME.  PLC.Error is Language.PlutusCore.Error.Error
                -- There's a lot of stuff at the end of Error.hs that we'd have to copy.
                exitFailure
          Right v ->
              do
                T.putStrLn $ U.prettyPlcDefText v
                exitSuccess
    else
        let evalFn = case mode of
                       CK  -> PLC.runCk
                       CEK -> PLC.unsafeRunCek mempty
                       L   -> PLC.runL mempty
                       _ -> undefined
        in case evalFn . void <$> PLC.runQuoteT (PLC.parseScoped bsContents) of
          Left (e :: PLC.Error PLC.AlexPosn) ->
              do
                T.putStrLn $ PLC.prettyPlcDefText e
                exitFailure
          Right v ->
              do
                T.putStrLn $ PLC.prettyPlcDefText v
                exitSuccess

deBrProg :: PLC.Program PLC.TyName PLC.Name ann -> PLC.Program D.TyDeBruijn D.DeBruijn ann
deBrProg p =
   case runExceptT $ D.deBruijnProgram p of
     Left e -> error e
     Right y -> case y of
                  Left freeVarError -> error ("Error: " ++ show freeVarError)
                  Right t -> t

runSerialise :: SerialisationOptions -> IO ()
runSerialise (SerialisationOptions is mode) = do
    contents <- getInput is
    let bsContents = (BSL.fromStrict . encodeUtf8 . T.pack) contents
    let serialiseFn = case mode of
                        Typed        -> serialise
                        TypedAnon    -> serialise . U.anonProgram
                        Untyped      -> serialise . U.eraseProgram
                        UntypedAnon  -> serialise . U.eraseProgram . U.anonProgram
                        UntypedAnon2 -> serialise . U.nameToIntProgram . U.eraseProgram
                        UntypedAnonDeBruijn -> serialise . U.deBruijnToIntProgram . U.eraseProgram . deBrProg
    case serialiseFn . void <$> PLC.runQuoteT (PLC.parseScoped bsContents) of
      Left (e :: PLC.Error PLC.AlexPosn) ->
          do
             putStrLn $ show e
             exitFailure
      Right y ->
          do
             BSC.putStrLn y
             exitSuccess
       
data TypeExample = TypeExample (PLC.Kind ()) (PLC.Type PLC.TyName ())
data TermExample = TermExample (PLC.Type PLC.TyName ()) (PLC.Term PLC.TyName PLC.Name ())
data SomeExample = SomeTypeExample TypeExample | SomeTermExample TermExample

prettySignature :: ExampleName -> SomeExample -> Doc ann
prettySignature name (SomeTypeExample (TypeExample kind _)) =
    pretty name <+> "::" <+> PLC.prettyPlcDef kind
prettySignature name (SomeTermExample (TermExample ty _)) =
    pretty name <+> ":"  <+> PLC.prettyPlcDef ty

prettyExample :: SomeExample -> Doc ann
prettyExample (SomeTypeExample (TypeExample _ ty))   = PLC.prettyPlcDef ty
prettyExample (SomeTermExample (TermExample _ term)) =
    PLC.prettyPlcDef $ PLC.Program () (PLC.defaultVersion ()) term

toTermExample :: PLC.Term PLC.TyName PLC.Name () -> TermExample
toTermExample term = TermExample ty term where
    program = PLC.Program () (PLC.defaultVersion ()) term
    ty = case PLC.runQuote . runExceptT $ PLC.typecheckPipeline PLC.defOffChainConfig program of
        Left (err :: PLC.Error ()) -> error $ PLC.prettyPlcDefString err
        Right vTy                  -> PLC.unNormalized vTy

getInteresting :: IO [(ExampleName, PLC.Term PLC.TyName PLC.Name ())]
getInteresting =
    sequence $ PLC.fromInterestingTermGens $ \name gen -> do
        PLC.TermOf term _ <- PLC.getSampleTermValue gen
        pure (T.pack name, term)

simpleExamples :: [(ExampleName, SomeExample)]
simpleExamples =
    [ ("succInteger", SomeTermExample $ toTermExample PLC.succInteger)
    , ("unit"       , SomeTypeExample $ TypeExample (PLC.Type ()) PLC.unit)
    , ("unitval"    , SomeTermExample $ toTermExample PLC.unitval)
    , ("bool"       , SomeTypeExample $ TypeExample (PLC.Type ()) PLC.bool)
    , ("true"       , SomeTermExample $ toTermExample PLC.true)
    , ("false"      , SomeTermExample $ toTermExample PLC.false)
    , ("churchNat"  , SomeTypeExample $ TypeExample (PLC.Type ()) PLC.churchNat)
    , ("churchZero" , SomeTermExample $ toTermExample PLC.churchZero)
    , ("churchSucc" , SomeTermExample $ toTermExample PLC.churchSucc)
    ]

getAvailableExamples :: IO [(ExampleName, SomeExample)]
getAvailableExamples = do
    interesting <- getInteresting
    pure $ simpleExamples ++ map (second $ SomeTermExample . toTermExample) interesting

-- The implementation is a little hacky: we generate interesting examples when the list of examples
-- is requsted and at each lookup of a particular example. I.e. each time we generate distinct
-- terms. But types of those terms must not change across requests, so we're safe.
runExample :: ExampleOptions -> IO ()
runExample (ExampleOptions ExampleAvailable)     = do
    examples <- getAvailableExamples
    traverse_ (T.putStrLn . PLC.docText . uncurry prettySignature) examples
runExample (ExampleOptions (ExampleSingle name)) = do
    examples <- getAvailableExamples
    T.putStrLn $ case lookup name examples of
        Nothing -> "Unknown name: " <> name
        Just ex -> PLC.docText $ prettyExample ex

main :: IO ()
main = do
    options <- customExecParser (prefs showHelpOnEmpty) plutus
    case options of
        Typecheck tos -> runTypecheck tos
        Eval eos      -> runEval eos
        Example eos   -> runExample eos
        Serialise sos -> runSerialise sos
