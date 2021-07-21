{-# LANGUAGE LambdaCase #-}

-- | Common option parsers for executables

module Parsers where

import           Common

import           Options.Applicative

-- | Parser for an input stream. If none is specified, default to stdin: this makes use in pipelines easier
input :: Parser Input
input = fileInput <|> stdInput <|> pure StdInput

fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "input"
  <> short 'i'
  <> metavar "FILENAME"
  <> help "Input file" )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read from stdin (default)" )

-- | Parser for an output stream. If none is specified, default to stdout: this makes use in pipelines easier
output :: Parser Output
output = fileOutput <|> stdOutput <|> pure StdOutput

fileOutput :: Parser Output
fileOutput = FileOutput <$> strOption
  (  long "output"
  <> short 'o'
  <> metavar "FILENAME"
  <> help "Output file" )

stdOutput :: Parser Output
stdOutput = flag' StdOutput
  (  long "stdout"
  <> help "Write to stdout (default)" )

formatHelp :: String
formatHelp =
  "textual, flat (de Bruijn indices), or flat-named (names)"

formatReader :: String -> Maybe Format
formatReader =
    \case
         "textual"       -> Just Textual
         "flat-named"    -> Just (Flat Named)
         "flat"          -> Just (Flat DeBruijn)
         "flat-deBruijn" -> Just (Flat DeBruijn)
         _               -> Nothing

inputformat :: Parser Format
inputformat = option (maybeReader formatReader)
  (  long "if"
  <> long "input-format"
  <> metavar "FORMAT"
  <> value Textual
  <> showDefault
  <> help ("Input format: " ++ formatHelp))

outputformat :: Parser Format
outputformat = option (maybeReader formatReader)
  (  long "of"
  <> long "output-format"
  <> metavar "FORMAT"
  <> value Textual
  <> showDefault
  <> help ("Output format: " ++ formatHelp))
-- -x -> run 100 times and print the mean time
timing1 :: Parser TimingMode
timing1 = flag NoTiming (Timing 100)
  (  short 'x'
  <> help "Report mean execution time of program over 100 repetitions"
  )

-- -X N -> run N times and print the mean time
timing2 :: Parser TimingMode
timing2 = Timing <$> option auto
  (  long "time-execution"
  <> short 'X'
  <> metavar "N"
  <> help "Report mean execution time of program over N repetitions. Use a large value of N if possible to get accurate results."
  )

-- We really do need two separate parsers here.
-- See https://github.com/pcapriotti/optparse-applicative/issues/194#issuecomment-205103230
timingmode :: Parser TimingMode
timingmode = timing1 <|> timing2

files :: Parser Files
files = some (argument str (metavar "[FILES...]"))

applyOpts :: Parser ApplyOptions
applyOpts = ApplyOptions <$>  files <*> inputformat <*> output <*> outputformat <*> printmode

printmode :: Parser PrintMode
printmode = option auto
  (  long "print-mode"
  <> metavar "MODE"
  <> value Debug
  <> showDefault
  <> help ("Print mode for textual output (ignored elsewhere): Classic -> plcPrettyClassicDef, Debug -> plcPrettyClassicDebug, "
        ++ "Readable -> prettyPlcReadableDef, ReadableDebug -> prettyPlcReadableDebug" ))

printOpts :: Parser PrintOptions
printOpts = PrintOptions <$> input <*> printmode

convertOpts :: Parser ConvertOptions
convertOpts = ConvertOptions <$> input <*> inputformat <*> output <*> outputformat <*> printmode

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
