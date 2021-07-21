{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

-- | Primitive names and functions for working with Plutus Core builtins.
module PlutusTx.Builtins (
                                -- * Bytestring builtins
                                ByteString
                                , concatenate
                                , takeByteString
                                , dropByteString
                                , emptyByteString
                                , equalsByteString
                                , lessThanByteString
                                , greaterThanByteString
                                , sha2_256
                                , sha3_256
                                , blake2b_256
                                , verifySignature
                                , decodeUtf8
                                -- * Integer builtins
                                , addInteger
                                , subtractInteger
                                , multiplyInteger
                                , divideInteger
                                , modInteger
                                , quotientInteger
                                , remainderInteger
                                , greaterThanInteger
                                , greaterThanEqInteger
                                , lessThanInteger
                                , lessThanEqInteger
                                , equalsInteger
                                -- * Error
                                , error
                                -- * Data
                                , BuiltinData
                                , chooseData
                                , matchData
                                , matchData'
                                , equalsData
                                , mkConstr
                                , mkMap
                                , mkList
                                , mkI
                                , mkB
                                , unsafeDataAsConstr
                                , unsafeDataAsMap
                                , unsafeDataAsList
                                , unsafeDataAsI
                                , unsafeDataAsB
                                , BI.builtinDataToData
                                , BI.dataToBuiltinData
                                -- * Strings
                                , BuiltinString
                                , appendString
                                , emptyString
                                , charToString
                                , equalsString
                                , encodeUtf8
                                -- * Lists
                                , matchList
                                -- * Tracing
                                , trace
                                -- * Data
                                ) where

import           Data.ByteString            as BS
import           Prelude                    hiding (String, error)

import           PlutusTx.Builtins.Class
import           PlutusTx.Builtins.Internal (BuiltinData, BuiltinString)
import qualified PlutusTx.Builtins.Internal as BI

{-# INLINABLE concatenate #-}
-- | Concatenates two 'ByteString's.
concatenate :: ByteString -> ByteString -> ByteString
concatenate x y = fromBuiltin (BI.concatenate (toBuiltin x) (toBuiltin y))

{-# INLINABLE takeByteString #-}
-- | Returns the n length prefix of a 'ByteString'.
takeByteString :: Integer -> ByteString -> ByteString
takeByteString n bs = fromBuiltin (BI.takeByteString (toBuiltin n) (toBuiltin bs))

{-# INLINABLE dropByteString #-}
-- | Returns the suffix of a 'ByteString' after n elements.
dropByteString :: Integer -> ByteString -> ByteString
dropByteString n bs = fromBuiltin (BI.dropByteString (toBuiltin n) (toBuiltin bs))

{-# INLINABLE emptyByteString #-}
-- | An empty 'ByteString'.
emptyByteString :: ByteString
emptyByteString = fromBuiltin BI.emptyByteString

{-# INLINABLE sha2_256 #-}
-- | The SHA2-256 hash of a 'ByteString'
sha2_256 :: ByteString -> ByteString
sha2_256 b = fromBuiltin (BI.sha2_256 (toBuiltin b))

{-# INLINABLE sha3_256 #-}
-- | The SHA3-256 hash of a 'ByteString'
sha3_256 :: ByteString -> ByteString
sha3_256 b = fromBuiltin (BI.sha3_256 (toBuiltin b))

{-# INLINABLE blake2b_256 #-}
-- | The BLAKE2B-256 hash of a 'ByteString'
blake2b_256 :: ByteString -> ByteString
blake2b_256 b = fromBuiltin (BI.blake2b_256 (toBuiltin b))

{-# INLINABLE verifySignature #-}
-- | Verify that the signature is a signature of the message by the public key.
verifySignature :: ByteString -> ByteString -> ByteString -> Bool
verifySignature pubKey message signature = fromBuiltin (BI.verifySignature (toBuiltin pubKey) (toBuiltin message) (toBuiltin signature))

{-# INLINABLE equalsByteString #-}
-- | Check if two 'ByteString's are equal.
equalsByteString :: ByteString -> ByteString -> Bool
equalsByteString x y = fromBuiltin (BI.equalsByteString (toBuiltin x) (toBuiltin y))

{-# INLINABLE lessThanByteString #-}
-- | Check if one 'ByteString' is less than another.
lessThanByteString :: ByteString -> ByteString -> Bool
lessThanByteString x y = fromBuiltin (BI.lessThanByteString (toBuiltin x) (toBuiltin y))

{-# INLINABLE greaterThanByteString #-}
-- | Check if one 'ByteString' is greater than another.
greaterThanByteString :: ByteString -> ByteString -> Bool
greaterThanByteString x y = fromBuiltin (BI.greaterThanByteString (toBuiltin x) (toBuiltin y))

{-# INLINABLE decodeUtf8 #-}
-- | Converts a ByteString to a String.
decodeUtf8 :: ByteString -> BuiltinString
decodeUtf8 x = BI.decodeUtf8 (toBuiltin x)

{-# INLINABLE addInteger #-}
-- | Add two 'Integer's.
addInteger :: Integer -> Integer -> Integer
addInteger x y = fromBuiltin (BI.addInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE subtractInteger #-}
-- | Subtract two 'Integer's.
subtractInteger :: Integer -> Integer -> Integer
subtractInteger x y = fromBuiltin (BI.subtractInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE multiplyInteger #-}
-- | Multiply two 'Integer's.
multiplyInteger :: Integer -> Integer -> Integer
multiplyInteger x y = fromBuiltin (BI.multiplyInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE divideInteger #-}
-- | Divide two integers.
divideInteger :: Integer -> Integer -> Integer
divideInteger x y = fromBuiltin (BI.divideInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE modInteger #-}
-- | Integer modulo operation.
modInteger :: Integer -> Integer -> Integer
modInteger x y = fromBuiltin (BI.modInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE quotientInteger #-}
-- | Quotient of two integers.
quotientInteger :: Integer -> Integer -> Integer
quotientInteger x y = fromBuiltin (BI.quotientInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE remainderInteger #-}
-- | Take the remainder of dividing two 'Integer's.
remainderInteger :: Integer -> Integer -> Integer
remainderInteger x y = fromBuiltin (BI.remainderInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE greaterThanInteger #-}
-- | Check whether one 'Integer' is greater than another.
greaterThanInteger :: Integer -> Integer -> Bool
greaterThanInteger x y = fromBuiltin (BI.greaterThanInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE greaterThanEqInteger #-}
-- | Check whether one 'Integer' is greater than or equal to another.
greaterThanEqInteger :: Integer -> Integer -> Bool
greaterThanEqInteger x y = fromBuiltin (BI.greaterThanEqInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE lessThanInteger #-}
-- | Check whether one 'Integer' is less than another.
lessThanInteger :: Integer -> Integer -> Bool
lessThanInteger x y = fromBuiltin (BI.lessThanInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE lessThanEqInteger #-}
-- | Check whether one 'Integer' is less than or equal to another.
lessThanEqInteger :: Integer -> Integer -> Bool
lessThanEqInteger x y = fromBuiltin (BI.lessThanEqInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE equalsInteger #-}
-- | Check if two 'Integer's are equal.
equalsInteger :: Integer -> Integer -> Bool
equalsInteger x y = fromBuiltin (BI.equalsInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE error #-}
-- | Aborts evaluation with an error.
error :: () -> a
error x = BI.error (toBuiltin x)

{-# INLINABLE appendString #-}
-- | Append two 'String's.
appendString :: BuiltinString -> BuiltinString -> BuiltinString
appendString = BI.appendString

{-# INLINABLE emptyString #-}
-- | An empty 'String'.
emptyString :: BuiltinString
emptyString = BI.emptyString

{-# INLINABLE charToString #-}
-- | Turn a 'Char' into a 'String'.
charToString :: Char -> BuiltinString
charToString c = BI.charToString (toBuiltin c)

{-# INLINABLE equalsString #-}
-- | Check if two strings are equal
equalsString :: BuiltinString -> BuiltinString -> Bool
equalsString x y = fromBuiltin (BI.equalsString x y)
{-# INLINABLE trace #-}
-- | Emit the given string as a trace message before evaluating the argument.
trace :: BuiltinString -> a -> a
trace s = BI.chooseUnit (BI.trace s)

{-# INLINABLE encodeUtf8 #-}
-- | Convert a String into a ByteString.
encodeUtf8 :: BuiltinString -> ByteString
encodeUtf8 s = fromBuiltin (BI.encodeUtf8 s)

matchList :: forall a r . BI.BuiltinList a -> r -> (a -> BI.BuiltinList a -> r) -> r
matchList l nilCase consCase = BI.chooseList (\_ -> nilCase) (\_ -> consCase (BI.head l) (BI.tail l)) l ()

{-# INLINABLE chooseData #-}
-- | Given five values for the five different constructors of 'BuiltinData', selects
-- one depending on which corresponds to the actual constructor of the given value.
chooseData :: forall a . a -> a -> a -> a -> a -> BuiltinData -> a
chooseData = BI.chooseData

{-# INLINABLE mkConstr #-}
-- | Constructs a 'BuiltinData' value with the @Constr@ constructor.
mkConstr :: Integer -> [BuiltinData] -> BuiltinData
mkConstr i args = BI.mkConstr (toBuiltin i) (toBuiltin args)

{-# INLINABLE mkMap #-}
-- | Constructs a 'BuiltinData' value with the @Map@ constructor.
mkMap :: [(BuiltinData, BuiltinData)] -> BuiltinData
mkMap es = BI.mkMap (toBuiltin es)

{-# INLINABLE mkList #-}
-- | Constructs a 'BuiltinData' value with the @List@ constructor.
mkList :: [BuiltinData] -> BuiltinData
mkList l = BI.mkList (toBuiltin l)

{-# INLINABLE mkI #-}
-- | Constructs a 'BuiltinData' value with the @I@ constructor.
mkI :: Integer -> BuiltinData
mkI = BI.mkI

{-# INLINABLE mkB #-}
-- | Constructs a 'BuiltinData' value with the @B@ constructor.
mkB :: ByteString -> BuiltinData
mkB = BI.mkB

{-# INLINABLE unsafeDataAsConstr #-}
-- | Deconstructs a 'BuiltinData' as a @Constr@, or fails if it is not one.
unsafeDataAsConstr :: BuiltinData -> (Integer, [BuiltinData])
unsafeDataAsConstr d = fromBuiltin (BI.unsafeDataAsConstr d)

{-# INLINABLE unsafeDataAsMap #-}
-- | Deconstructs a 'BuiltinData' as a @Map@, or fails if it is not one.
unsafeDataAsMap :: BuiltinData -> [(BuiltinData, BuiltinData)]
unsafeDataAsMap d = fromBuiltin (BI.unsafeDataAsMap d)

{-# INLINABLE unsafeDataAsList #-}
-- | Deconstructs a 'BuiltinData' as a @List@, or fails if it is not one.
unsafeDataAsList :: BuiltinData -> [BuiltinData]
unsafeDataAsList d = fromBuiltin (BI.unsafeDataAsList d)

{-# INLINABLE unsafeDataAsI #-}
-- | Deconstructs a 'BuiltinData' as an @I@, or fails if it is not one.
unsafeDataAsI :: BuiltinData -> Integer
unsafeDataAsI d = fromBuiltin (BI.unsafeDataAsI d)

{-# INLINABLE unsafeDataAsB #-}
-- | Deconstructs a 'BuiltinData' as a @B@, or fails if it is not one.
unsafeDataAsB :: BuiltinData -> ByteString
unsafeDataAsB d = fromBuiltin (BI.unsafeDataAsB d)

{-# INLINABLE equalsData #-}
-- | Check if two 'BuiltinData's are equal.
equalsData :: BuiltinData -> BuiltinData -> Bool
equalsData d1 d2 = fromBuiltin (BI.equalsData d1 d2)

{-# INLINABLE matchData #-}
-- | Given a 'BuiltinData' value and matching functions for the five constructors,
-- applies the appropriate matcher to the arguments of the constructor and returns the result.
matchData
    :: BuiltinData
    -> (Integer -> [BuiltinData] -> r)
    -> ([(BuiltinData, BuiltinData)] -> r)
    -> ([BuiltinData] -> r)
    -> (Integer -> r)
    -> (BS.ByteString -> r)
    -> r
matchData d constrCase mapCase listCase iCase bCase =
   chooseData
   (\_ -> uncurry constrCase (unsafeDataAsConstr d))
   (\_ -> mapCase (unsafeDataAsMap d))
   (\_ -> listCase (unsafeDataAsList d))
   (\_ -> iCase (unsafeDataAsI d))
   (\_ -> bCase (unsafeDataAsB d))
   d
   ()

{-# INLINABLE matchData' #-}
-- | Given a 'BuiltinData' value and matching functions for the five constructors,
-- applies the appropriate matcher to the arguments of the constructor and returns the result.
matchData'
    :: BuiltinData
    -> (Integer -> BI.BuiltinList BuiltinData -> r)
    -> (BI.BuiltinList (BI.BuiltinPair BuiltinData BuiltinData) -> r)
    -> (BI.BuiltinList BuiltinData -> r)
    -> (Integer -> r)
    -> (BS.ByteString -> r)
    -> r
matchData' d constrCase mapCase listCase iCase bCase =
   chooseData
   (\_ -> let tup = BI.unsafeDataAsConstr d in constrCase (BI.fst tup) (BI.snd tup))
   (\_ -> mapCase (BI.unsafeDataAsMap d))
   (\_ -> listCase (BI.unsafeDataAsList d))
   (\_ -> iCase (unsafeDataAsI d))
   (\_ -> bCase (unsafeDataAsB d))
   d
   ()
