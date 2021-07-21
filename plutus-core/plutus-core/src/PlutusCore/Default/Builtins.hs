{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module PlutusCore.Default.Builtins where

import           PlutusCore.Constant
import           PlutusCore.Data
import           PlutusCore.Default.Universe
import           PlutusCore.Evaluation.Machine.BuiltinCostModel
import           PlutusCore.Evaluation.Machine.ExMemory
import           PlutusCore.Evaluation.Result
import           PlutusCore.NumberTheory                        (invert, powMod, probablyPrime)
import           PlutusCore.Pretty

import           Control.DeepSeq
import           Crypto
import qualified Data.ByteString                                as BS
import qualified Data.ByteString.Char8                          as BSC
import qualified Data.ByteString.Hash                           as Hash
import           Data.Char
import           Data.Ix
import           Data.Word                                      (Word8)
import           Flat
import           Flat.Decoder
import           Flat.Encoder                                   as Flat

-- See Note [Pattern matching on built-in types].
-- TODO: should we have the commonest builtins at the front to have more compact encoding?
-- | Default built-in functions.
data DefaultFun
    = AddInteger
    | SubtractInteger
    | MultiplyInteger
    | DivideInteger
    | QuotientInteger
    | RemainderInteger
    | ModInteger
    | PowModInteger
    | InvertInteger
    | ProbablyPrimeInteger
    | LessThanInteger
    | LessThanEqualsInteger
    | GreaterThanInteger
    | GreaterThanEqualsInteger
    | EqualsInteger
    | Concatenate
    | TakeByteString
    | DropByteString
    | Sha2_256
    | Sha3_256
    | VerifySignature
    | EqualsByteString
    | LessThanByteString
    | GreaterThanByteString
    | IfThenElse
    | CharToString
    | Append
    | EqualsString
    | EncodeUtf8
    | DecodeUtf8
    | Trace
    | FstPair
    | SndPair
    | NullList
    | HeadList
    | TailList
    | ChooseList
    | ConstrData
    | MapData
    | ListData
    | IData
    | BData
    | UnConstrData
    | UnMapData
    | UnListData
    | UnIData
    | UnBData
    | EqualsData
    -- It is convenient to have a "choosing" function for a data type that has more than two
    -- constructors to get pattern matching over it and we may end up having multiple such data
    -- types, hence we include the name of the data type as a suffix.
    | ChooseData
    | ChooseUnit
    -- Constructors that we need for constructing e.g. Data. Polymorphic builtin
    -- constructors are often problematic (See note [Representable built-in functions over polymorphic built-in types])
    | MkPairData
    | MkNilData
    | MkNilPairData
    | MkCons
    -- TODO. These are only used for costing calibration and shouldn't be included in the defaults.
    | Nop1
    | Nop2
    | Nop3
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData, Hashable, Ix, PrettyBy PrettyConfigPlc)

instance Pretty DefaultFun where
    pretty fun = pretty $ case show fun of
        ""    -> ""  -- It's really weird to have a function's name displayed as an empty string,
                     -- but if it's what the 'Show' instance does, the user has asked for it.
        c : s -> toLower c : s

instance ExMemoryUsage DefaultFun where
    memoryUsage _ = 1

-- | Turn a function into another function that returns 'EvaluationFailure' when its second argument
-- is 0 or calls the original function otherwise and wraps the result in 'EvaluationSuccess'.
-- Useful for correctly handling `div`, `mod`, etc.
nonZeroSecondArg :: (Integer -> Integer -> Integer) -> Integer -> Integer -> EvaluationResult Integer
nonZeroSecondArg _ _ 0 = EvaluationFailure
nonZeroSecondArg f x y = EvaluationSuccess $ f x y

-- | Turn a function into another function that returns 'EvaluationFailure' when its third argument
-- is 0 or calls the original function otherwise and wraps the result in 'EvaluationSuccess'.
-- Useful for correctly handling `powMod`, etc.
nonZeroThirdArg :: (Integer -> Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> EvaluationResult Integer
nonZeroThirdArg _ _ _ 0 = EvaluationFailure
nonZeroThirdArg f x y z = EvaluationSuccess $ f x y z

instance uni ~ DefaultUni => ToBuiltinMeaning uni DefaultFun where
    type CostingPart uni DefaultFun = BuiltinCostModel
    toBuiltinMeaning
        :: forall term. HasConstantIn uni term
        => DefaultFun -> BuiltinMeaning term BuiltinCostModel
    toBuiltinMeaning AddInteger =
        makeBuiltinMeaning
            ((+) @Integer)
            (runCostingFunTwoArguments . paramAddInteger)
    toBuiltinMeaning SubtractInteger =
        makeBuiltinMeaning
            ((-) @Integer)
            (runCostingFunTwoArguments . paramSubtractInteger)
    toBuiltinMeaning MultiplyInteger =
        makeBuiltinMeaning
            ((*) @Integer)
            (runCostingFunTwoArguments . paramMultiplyInteger)
    toBuiltinMeaning DivideInteger =
        makeBuiltinMeaning
            (nonZeroSecondArg div)
            (runCostingFunTwoArguments . paramDivideInteger)
    toBuiltinMeaning QuotientInteger =
        makeBuiltinMeaning
            (nonZeroSecondArg quot)
            (runCostingFunTwoArguments . paramQuotientInteger)
    toBuiltinMeaning RemainderInteger =
        makeBuiltinMeaning
            (nonZeroSecondArg rem)
            (runCostingFunTwoArguments . paramRemainderInteger)
    toBuiltinMeaning ModInteger =
        makeBuiltinMeaning
            (nonZeroSecondArg mod)
            (runCostingFunTwoArguments . paramModInteger)
    toBuiltinMeaning PowModInteger =
        makeBuiltinMeaning
            (nonZeroThirdArg powMod)
            (runCostingFunThreeArguments . paramPowModInteger)
    toBuiltinMeaning InvertInteger =
        makeBuiltinMeaning
            (nonZeroSecondArg invert)
            (runCostingFunTwoArguments . paramInvertInteger)
    toBuiltinMeaning ProbablyPrimeInteger =
        makeBuiltinMeaning
            (nonZeroSecondArg probablyPrime)
            (runCostingFunTwoArguments . paramProbablyPrimeInteger)
    toBuiltinMeaning LessThanInteger =
        makeBuiltinMeaning
            ((<) @Integer)
            (runCostingFunTwoArguments . paramLessThanInteger)
    toBuiltinMeaning LessThanEqualsInteger =
        makeBuiltinMeaning
            ((<=) @Integer)
            (runCostingFunTwoArguments . paramLessThanEqInteger)
    toBuiltinMeaning GreaterThanInteger =
        makeBuiltinMeaning
            ((>) @Integer)
            (runCostingFunTwoArguments . paramGreaterThanInteger)
    toBuiltinMeaning GreaterThanEqualsInteger =
        makeBuiltinMeaning
            ((>=) @Integer)
            (runCostingFunTwoArguments . paramGreaterThanEqInteger)
    toBuiltinMeaning EqualsInteger =
        makeBuiltinMeaning
            ((==) @Integer)
            (runCostingFunTwoArguments . paramEqInteger)
    toBuiltinMeaning Concatenate =
        makeBuiltinMeaning
            BS.append
            (runCostingFunTwoArguments . paramConcatenate)
    toBuiltinMeaning TakeByteString =
        makeBuiltinMeaning
            BS.take
            (runCostingFunTwoArguments . paramTakeByteString)
    toBuiltinMeaning DropByteString =
        makeBuiltinMeaning
            BS.drop
            (runCostingFunTwoArguments . paramDropByteString)
    toBuiltinMeaning Sha2_256 =
        makeBuiltinMeaning
            Hash.sha2
            (runCostingFunOneArgument . paramSHA2)
    toBuiltinMeaning Sha3_256 =
        makeBuiltinMeaning
            Hash.sha3
            (runCostingFunOneArgument . paramSHA3)
    toBuiltinMeaning VerifySignature =
        makeBuiltinMeaning
            (verifySignature @EvaluationResult)
            (runCostingFunThreeArguments . paramVerifySignature)
    toBuiltinMeaning EqualsByteString =
        makeBuiltinMeaning
            ((==) @BS.ByteString)
            (runCostingFunTwoArguments . paramEqByteString)
    toBuiltinMeaning LessThanByteString =
        makeBuiltinMeaning
            ((<) @BS.ByteString)
            (runCostingFunTwoArguments . paramLtByteString)
    toBuiltinMeaning GreaterThanByteString =
        makeBuiltinMeaning
            ((>) @BS.ByteString)
            (runCostingFunTwoArguments . paramGtByteString)
    toBuiltinMeaning IfThenElse =
       makeBuiltinMeaning
            (\b x y -> if b then x else y)
            (runCostingFunThreeArguments . paramIfThenElse)
    toBuiltinMeaning CharToString =
        makeBuiltinMeaning
            (pure :: Char -> String)
            mempty  -- TODO: budget.
    toBuiltinMeaning Append =
        makeBuiltinMeaning
            ((++) :: String -> String -> String)
            mempty  -- TODO: budget.
    toBuiltinMeaning EqualsString =
        makeBuiltinMeaning
            ((==) @String)
            mempty  -- TODO: budget.
    toBuiltinMeaning EncodeUtf8 =
        makeBuiltinMeaning
            (BSC.pack :: String -> BS.ByteString)
            mempty  -- TODO: budget.
    toBuiltinMeaning DecodeUtf8 =
        makeBuiltinMeaning
            (BSC.unpack :: BS.ByteString -> String)
            mempty  -- TODO: budget.
    toBuiltinMeaning Trace =
        makeBuiltinMeaning
            (emit :: String -> Emitter ())
            mempty  -- TODO: budget.
    toBuiltinMeaning Nop1 =
        makeBuiltinMeaning
            @(Integer -> ())
            mempty
            mempty
    toBuiltinMeaning Nop2 =
        makeBuiltinMeaning
            @(Integer -> Integer -> ())
            mempty
            mempty
    toBuiltinMeaning Nop3 =
        makeBuiltinMeaning
            @(Integer -> Integer -> Integer -> ())
            mempty
            mempty
    toBuiltinMeaning FstPair = makeBuiltinMeaning fstPlc mempty where
        fstPlc :: SomeConstantOf uni (,) '[a, b] -> Opaque term a
        fstPlc (SomeConstantOfArg uniA (SomeConstantOfArg _ (SomeConstantOfRes _ (x, _)))) =
            Opaque . fromConstant . Some $ ValueOf uniA x
    toBuiltinMeaning SndPair = makeBuiltinMeaning sndPlc mempty where
        sndPlc :: SomeConstantOf uni (,) '[a, b] -> Opaque term b
        sndPlc (SomeConstantOfArg _ (SomeConstantOfArg uniB (SomeConstantOfRes _ (_, y)))) =
            Opaque . fromConstant . Some $ ValueOf uniB y
    toBuiltinMeaning NullList = makeBuiltinMeaning nullPlc mempty where
        nullPlc :: SomeConstantOf uni [] '[a] -> Bool
        nullPlc (SomeConstantOfArg _ (SomeConstantOfRes _ xs)) = null xs
    toBuiltinMeaning HeadList = makeBuiltinMeaning headPlc mempty where
        headPlc :: SomeConstantOf uni [] '[a] -> EvaluationResult (Opaque term a)
        headPlc (SomeConstantOfArg uniA (SomeConstantOfRes _ xs)) = case xs of
            x : _ -> EvaluationSuccess . Opaque . fromConstant $ someValueOf uniA x
            _     -> EvaluationFailure
    toBuiltinMeaning TailList = makeBuiltinMeaning tailPlc mempty where
        tailPlc :: SomeConstantOf uni [] '[a] -> EvaluationResult (SomeConstantOf uni [] '[a])
        tailPlc (SomeConstantOfArg uniA (SomeConstantOfRes uniListA xs)) = case xs of
            _ : xs' -> EvaluationSuccess . SomeConstantOfArg uniA $ SomeConstantOfRes uniListA xs'
            _       -> EvaluationFailure
    toBuiltinMeaning ChooseList = makeBuiltinMeaning choosePlc mempty where
        choosePlc :: Opaque term b -> Opaque term b -> SomeConstantOf uni [] '[a] -> EvaluationResult (Opaque term b)
        choosePlc a b (SomeConstantOfArg _ (SomeConstantOfRes _ xs)) = case xs of
            []    -> EvaluationSuccess a
            _ : _ -> EvaluationSuccess b
    toBuiltinMeaning ConstrData =
        makeBuiltinMeaning
            Constr
            mempty
    toBuiltinMeaning MapData =
        makeBuiltinMeaning
            Map
            mempty
    toBuiltinMeaning ListData =
        makeBuiltinMeaning
            List
            mempty
    toBuiltinMeaning IData =
        makeBuiltinMeaning
            I
            mempty
    toBuiltinMeaning BData =
        makeBuiltinMeaning
            B
            mempty
    toBuiltinMeaning UnConstrData =
        makeBuiltinMeaning
            (\case
                Constr i ds -> EvaluationSuccess (i, ds)
                _           -> EvaluationFailure)
            mempty
    toBuiltinMeaning UnMapData =
        makeBuiltinMeaning
            (\case
                Map es -> EvaluationSuccess es
                _      -> EvaluationFailure)
            mempty
    toBuiltinMeaning UnListData =
        makeBuiltinMeaning
            (\case
                List ds -> EvaluationSuccess ds
                _       -> EvaluationFailure)
            mempty
    toBuiltinMeaning UnIData =
        makeBuiltinMeaning
            (\case
                I i -> EvaluationSuccess i
                _   -> EvaluationFailure)
            mempty
    toBuiltinMeaning UnBData =
        makeBuiltinMeaning
            (\case
                B b -> EvaluationSuccess b
                _   -> EvaluationFailure)
            mempty
    toBuiltinMeaning EqualsData =
        makeBuiltinMeaning
            ((==) @Data)
            mempty
    toBuiltinMeaning ChooseData =
        makeBuiltinMeaning
            (\xConstr xMap xList xI xB -> \case
                Constr {} -> xConstr
                Map    {} -> xMap
                List   {} -> xList
                I      {} -> xI
                B      {} -> xB)
            mempty
    toBuiltinMeaning ChooseUnit =
        makeBuiltinMeaning
            (\() a -> a)
            mempty
    toBuiltinMeaning MkPairData =
        makeBuiltinMeaning
            ((,) :: Data -> Data -> (Data, Data))
            mempty
    toBuiltinMeaning MkNilData =
        -- Nullary builtins don't work, so we need a unit argument
        makeBuiltinMeaning
            @(() -> [Data])
            (\() -> [])
            mempty
    toBuiltinMeaning MkNilPairData =
        -- Nullary builtins don't work, so we need a unit argument
        makeBuiltinMeaning
            @(() -> [(Data,Data)])
            (\() -> [])
            mempty
    toBuiltinMeaning MkCons = makeBuiltinMeaning consPlc mempty where
        consPlc
            :: SomeConstant uni a
            -> SomeConstantOf uni [] '[a]
            -> EvaluationResult (SomeConstantOf uni [] '[a])
        consPlc
            (SomeConstant (Some (ValueOf uniA x)))
            (SomeConstantOfArg uniA' (SomeConstantOfRes uniListA xs)) =
                -- Checking that the type of the constant is the same as the type of the elements
                -- of the unlifted list. Note that there's no way we could enforce this statically
                -- since in UPLC one can create an ill-typed program that attempts to prepend
                -- a value of the wrong type to a list.
                case uniA `geq` uniA' of
                    -- Should this rather be an 'UnliftingError'? For that we need
                    -- https://github.com/input-output-hk/plutus/pull/3035
                    Nothing   -> EvaluationFailure
                    Just Refl ->
                        EvaluationSuccess . SomeConstantOfArg uniA $
                            SomeConstantOfRes uniListA $ x : xs

-- It's set deliberately to give us "extra room" in the binary format to add things without running
-- out of space for tags (expanding the space would change the binary format for people who're
-- implementing it manually). So we have to set it manually.
-- | Using 7 bits to encode builtin tags.
builtinTagWidth :: NumBits
builtinTagWidth = 7

encodeBuiltin :: Word8 -> Flat.Encoding
encodeBuiltin = eBits builtinTagWidth

decodeBuiltin :: Get Word8
decodeBuiltin = dBEBits8 builtinTagWidth

-- See Note [Stable encoding of PLC]
instance Flat DefaultFun where
    encode = encodeBuiltin . \case
              AddInteger               -> 0
              SubtractInteger          -> 1
              MultiplyInteger          -> 2
              DivideInteger            -> 3
              RemainderInteger         -> 4
              LessThanInteger          -> 5
              LessThanEqualsInteger    -> 6
              GreaterThanInteger       -> 7
              GreaterThanEqualsInteger -> 8
              EqualsInteger            -> 9
              Concatenate              -> 10
              TakeByteString           -> 11
              DropByteString           -> 12
              Sha2_256                 -> 13
              Sha3_256                 -> 14
              VerifySignature          -> 15
              EqualsByteString         -> 16
              QuotientInteger          -> 17
              ModInteger               -> 18
              LessThanByteString       -> 19
              GreaterThanByteString    -> 20
              IfThenElse               -> 21
              CharToString             -> 22
              Append                   -> 23
              EqualsString             -> 28
              EncodeUtf8               -> 29
              DecodeUtf8               -> 30
              Trace                    -> 24
              Nop1                     -> 25
              Nop2                     -> 26
              Nop3                     -> 27
              FstPair                  -> 31
              SndPair                  -> 32
              NullList                 -> 33
              HeadList                 -> 34
              TailList                 -> 35
              ConstrData               -> 36
              MapData                  -> 37
              ListData                 -> 38
              IData                    -> 39
              BData                    -> 40
              UnConstrData             -> 41
              UnMapData                -> 42
              UnListData               -> 43
              UnIData                  -> 44
              UnBData                  -> 45
              EqualsData               -> 46
              ChooseData               -> 47
              ChooseUnit               -> 48
              MkPairData               -> 49
              MkNilData                -> 50
              MkNilPairData            -> 51
              MkCons                   -> 52
              ChooseList               -> 53
              PowModInteger            -> 54
              InvertInteger            -> 55
              ProbablyPrimeInteger     -> 56

    decode = go =<< decodeBuiltin
        where go 0  = pure AddInteger
              go 1  = pure SubtractInteger
              go 2  = pure MultiplyInteger
              go 3  = pure DivideInteger
              go 4  = pure RemainderInteger
              go 5  = pure LessThanInteger
              go 6  = pure LessThanEqualsInteger
              go 7  = pure GreaterThanInteger
              go 8  = pure GreaterThanEqualsInteger
              go 9  = pure EqualsInteger
              go 10 = pure Concatenate
              go 11 = pure TakeByteString
              go 12 = pure DropByteString
              go 13 = pure Sha2_256
              go 14 = pure Sha3_256
              go 15 = pure VerifySignature
              go 16 = pure EqualsByteString
              go 17 = pure QuotientInteger
              go 18 = pure ModInteger
              go 19 = pure LessThanByteString
              go 20 = pure GreaterThanByteString
              go 21 = pure IfThenElse
              go 22 = pure CharToString
              go 23 = pure Append
              go 28 = pure EqualsString
              go 29 = pure EncodeUtf8
              go 30 = pure DecodeUtf8
              go 24 = pure Trace
              go 25 = pure Nop1
              go 26 = pure Nop2
              go 27 = pure Nop3
              go 31 = pure FstPair
              go 32 = pure SndPair
              go 33 = pure NullList
              go 34 = pure HeadList
              go 35 = pure TailList
              go 36 = pure ConstrData
              go 37 = pure MapData
              go 38 = pure ListData
              go 39 = pure IData
              go 40 = pure BData
              go 41 = pure UnConstrData
              go 42 = pure UnMapData
              go 43 = pure UnListData
              go 44 = pure UnIData
              go 45 = pure UnBData
              go 46 = pure EqualsData
              go 47 = pure ChooseData
              go 48 = pure ChooseUnit
              go 49 = pure MkPairData
              go 50 = pure MkNilData
              go 51 = pure MkNilPairData
              go 52 = pure MkCons
              go 53 = pure ChooseList
              go 54 = pure PowModInteger
              go 55 = pure InvertInteger
              go 56 = pure ProbablyPrimeInteger
              go _  = fail "Failed to decode BuiltinName"

    size _ n = n + builtinTagWidth
