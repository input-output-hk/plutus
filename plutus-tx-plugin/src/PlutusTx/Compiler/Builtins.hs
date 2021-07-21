{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Functions for compiling Plutus Core builtins.
module PlutusTx.Compiler.Builtins (
    builtinNames
    , defineBuiltinTypes
    , defineBuiltinTerms
    , lookupBuiltinTerm
    , lookupBuiltinType
    , errorTy
    , errorFunc) where

import qualified PlutusTx.Builtins.Class       as Builtins
import qualified PlutusTx.Builtins.Internal    as Builtins

import           PlutusTx.Compiler.Error
import           PlutusTx.Compiler.Names
import           PlutusTx.Compiler.Types
import           PlutusTx.Compiler.Utils
import           PlutusTx.PIRTypes

import qualified PlutusIR                      as PIR
import qualified PlutusIR.Compiler.Definitions as PIR
import           PlutusIR.Compiler.Names
import qualified PlutusIR.MkPir                as PIR
import qualified PlutusIR.Purity               as PIR

import qualified PlutusCore                    as PLC
import qualified PlutusCore.Constant           as PLC
import qualified PlutusCore.Data               as PLC
import           PlutusCore.Quote

import qualified GhcPlugins                    as GHC

import qualified Language.Haskell.TH.Syntax    as TH

import           Control.Monad.Reader

import qualified Data.ByteString               as BS
import qualified Data.Map                      as Map
import           Data.Proxy

{- Note [Mapping builtins]
We want the user to be able to call the Plutus builtins as normal Haskell functions.

To do this, we provide a library of such functions in Haskell, and we define corresponding
functions and types in PIR so that we can translate references to the Haskell functions and
types into references to the PIR ones.

We can then do whatever we need to inside the definitions to line things up with the real builtins.
(See Note [Builtin types and Haskell types])

To do this, we first need a map from the Haskell TH names to the corresponding GHC names
(in fact the TyThings, so we have the types too). Annoyingly, this has to be done in the
GHC Core monad and then passed to us.

This map lets us write code that defines all the builtins (by their TH names), and also to look
up things by their TH names in the few other cases where we need to (mostly where we use specific
known builtins to implement primitives).

This is a bit fragile, since we rely on having all the names that we need, and having them
mapped to the right things (GHC will panic on us if we e.g. get the wrong kind of TyThing for
a name). We should probably revisit this later.
-}

{- Note [Builtin types and Haskell types]
Several of the PLC builtins use types that should (morally) line up with types that we compile from
Haskell (see also Note [Which types map to builtin types?]).
But there is a problem: they use either primitive or Scott-encoded versions of these types,
whereas when we compile them from Haskell they will end up as abstract types, and so the types
won't line up at the call site.

That is, we generate something like this:
(/\ (Integer :: *) .
  (\ addInteger : Integer -> Integer -> Integer .
      <use addInteger>
  )
  (\ x,y : Integer . <builtin addInteger> x y) -- Uh oh, type error, can't show that Integer = <builtin int>!
)
{<builtin int>}

We handle this in two different ways:
- For the types like Bool and Unit which are really algebraic types, and which have constructors etc.
that we care about elsewhere, we insert adaptor code into the definition of the builtin (see note [Mapping builtins]).
- For types like Integer and Bytestring which don't have visible constructors, we can treat them as completely opaque,
and we use a transparent type alias. (Actually we fake the alias by actually just substituting the definition in
everywhere until we have aliases in PIR. Bear this in mind for the examples below.)

Here's how that looks for a primitive that takes Ints and returns a Boolean, assuming we have bound Integer and Bool
already as an alias and an abstract type respectively:
(\ equalsInteger : Integer -> Integer -> Bool .
  <use equalsInteger>
)
(\ x, y : Integer . -- No need to do anything to the arguments, since we're using a transparent alias for Int
  (<builtin equalsInteger> x y) {Bool} True False -- Immediately match the builtin bool into an abstract Bool
)

We *could* do something like the interleaved definitions that we do for datatypes in PIR. Morally this is perhaps the
right thing to do: we should think of Integer and its builtins as a "module" that goes together and where all the definitions
have access to the internals of the other definitions. A datatype definition is then a special case of a module definition.
However, for the moment this would be quite a bit more design work and so we leave it for future work.

For an example of how the "abstract module" approach would look:
(/\ (Integer :: *) .
  (\ addInteger : Integer -> Integer -> Integer . -- Type signature is fine inside the abstraction
      <use addInteger>
  )
)
{<builtin int>}
(\ x,y : <builtin int> . <builtin addInteger> x y) -- No type error any more, abstraction is gone
-}

{- Note [Which types map to builtin types?]
We have (will have) Bool in the default builtin universe. Why do we not map the Haskell Bool type to the
builtin Bool, but rather compile it as a normal ADT?

The advantage of mapping a type to a builtin type is mainly performance:
- We can directly use (potentially optimized) implementations of operations on that type.
- We do not need adaptors to interoperate with builtin functions that use the builtin version of the type.

On the other hand, the advantages of *not* doing this are:
- User-written code that operates on the type as normal (e.g. pattern matching) will work.
    - We could make this work by compiling pattern matching specially for the builtin type, but this means
      more special cases in the compiler (boo). Maybe we can do this generically in future.
- Code that uses these types will also be compilable/runnable if those builtins are not present.

Overall, this means that we only map performance-critical types like Integer and ByteString directly to
builtin types, and the others we compile normally.
-}

{- Note [Builtin terms and values]
When generating let-bindings, we would like to generate strict bindings only for things that are obviously
pure, and non-strict bindings otherwise. This ensures that we won't evaluate the RHS of the binding prematurely,
which matters if it could trigger an error, or some other effect.

Additionally, strict bindings are a bit more efficient than non-strict ones (non-strict ones get turned into
lambdas from unit and forcing in the body). So we would like to use strict bindings where possible.

Now, we generate bindings for all our builtin functions... but they are not *obviously* pure!
Fortunately, we have a more sophisticated purity check that also detects unsaturated builtin applications,
which handles these cases too.
-}

mkBuiltin :: fun -> PIR.Term tyname name uni fun ()
mkBuiltin = PIR.Builtin ()

-- | The 'TH.Name's for which 'BuiltinNameInfo' needs to be provided.
builtinNames :: [TH.Name]
builtinNames = [
      ''BS.ByteString
    , 'Builtins.concatenate
    , 'Builtins.takeByteString
    , 'Builtins.dropByteString
    , 'Builtins.sha2_256
    , 'Builtins.sha3_256
    , 'Builtins.blake2b_256
    , 'Builtins.equalsByteString
    , 'Builtins.lessThanByteString
    , 'Builtins.greaterThanByteString
    , 'Builtins.emptyByteString
    , 'Builtins.decodeUtf8

    , 'Builtins.verifySignature

    , ''Integer
    , 'Builtins.addInteger
    , 'Builtins.subtractInteger
    , 'Builtins.multiplyInteger
    , 'Builtins.divideInteger
    , 'Builtins.modInteger
    , 'Builtins.quotientInteger
    , 'Builtins.remainderInteger
    , 'Builtins.greaterThanInteger
    , 'Builtins.greaterThanEqInteger
    , 'Builtins.lessThanInteger
    , 'Builtins.lessThanEqInteger
    , 'Builtins.equalsInteger

    , 'Builtins.error

    , ''Builtins.BuiltinString
    , ''Char
    , 'Builtins.appendString
    , 'Builtins.emptyString
    , 'Builtins.charToString
    , 'Builtins.equalsString
    , 'Builtins.encodeUtf8
    -- This one is special
    , 'Builtins.stringToBuiltinString

    , 'Builtins.trace

    , ''Builtins.BuiltinBool
    , 'Builtins.ifThenElse
    , 'Builtins.true
    , 'Builtins.false

    , ''Builtins.BuiltinUnit
    , 'Builtins.unitval
    , 'Builtins.chooseUnit

    , ''Builtins.BuiltinPair
    , 'Builtins.fst
    , 'Builtins.snd
    , 'Builtins.mkPairData

    , ''Builtins.BuiltinList
    , 'Builtins.null
    , 'Builtins.head
    , 'Builtins.tail
    , 'Builtins.chooseList
    , 'Builtins.mkNilData
    , 'Builtins.mkNilPairData
    , 'Builtins.mkCons

    , ''Builtins.BuiltinData
    , 'Builtins.chooseData
    , 'Builtins.equalsData
    , 'Builtins.mkConstr
    , 'Builtins.mkMap
    , 'Builtins.mkList
    , 'Builtins.mkI
    , 'Builtins.mkB
    , 'Builtins.unsafeDataAsConstr
    , 'Builtins.unsafeDataAsMap
    , 'Builtins.unsafeDataAsList
    , 'Builtins.unsafeDataAsB
    , 'Builtins.unsafeDataAsI
    ]

-- | Get the 'GHC.TyThing' for a given 'TH.Name' which was stored in the builtin name info,
-- failing if it is missing.
getThing :: Compiling uni fun m => TH.Name -> m GHC.TyThing
getThing name = do
    CompileContext{ccBuiltinNameInfo=names} <- ask
    case Map.lookup name names of
        Nothing    -> throwSd CompilationError $ "Missing builtin name:" GHC.<+> (GHC.text $ show name)
        Just thing -> pure thing

defineBuiltinTerm :: Compiling uni fun m => TH.Name -> PIRTerm uni fun -> m ()
defineBuiltinTerm name term = do
    ghcId <- GHC.tyThingId <$> getThing name
    var <- compileVarFresh ghcId
    -- See Note [Builtin terms and values]
    let strictness = if PIR.isPure (const PIR.NonStrict) term then PIR.Strict else PIR.NonStrict
        def = PIR.Def var (term, strictness)
    PIR.defineTerm (LexName $ GHC.getName ghcId) def mempty

-- | Add definitions for all the builtin types to the environment.
defineBuiltinType :: forall uni fun m. Compiling uni fun m => TH.Name -> PIRType uni -> m ()
defineBuiltinType name ty = do
    tc <- GHC.tyThingTyCon <$> getThing name
    var <- compileTcTyVarFresh tc
    PIR.defineType (LexName $ GHC.getName tc) (PIR.Def var ty) mempty
    -- these are all aliases for now
    PIR.recordAlias @LexName @uni @fun @() (LexName $ GHC.getName tc)

-- | Add definitions for all the builtin terms to the environment.
defineBuiltinTerms :: CompilingDefault uni fun m => m ()
defineBuiltinTerms = do

    -- See Note [Builtin terms and values]
    -- Bool
    defineBuiltinTerm 'Builtins.ifThenElse $ mkBuiltin PLC.IfThenElse
    defineBuiltinTerm 'Builtins.true $ PIR.mkConstant () True
    defineBuiltinTerm 'Builtins.false $ PIR.mkConstant () False

    defineBuiltinTerm 'Builtins.unitval $ PIR.mkConstant () ()
    defineBuiltinTerm 'Builtins.chooseUnit $ mkBuiltin PLC.ChooseUnit

    -- Bytestring builtins
    defineBuiltinTerm 'Builtins.concatenate $ mkBuiltin PLC.Concatenate
    defineBuiltinTerm 'Builtins.takeByteString $ mkBuiltin PLC.TakeByteString
    defineBuiltinTerm 'Builtins.dropByteString $ mkBuiltin PLC.DropByteString
    defineBuiltinTerm 'Builtins.sha2_256 $ mkBuiltin PLC.Sha2_256
    defineBuiltinTerm 'Builtins.sha3_256 $ mkBuiltin PLC.Sha3_256
    defineBuiltinTerm 'Builtins.equalsByteString $ mkBuiltin PLC.EqualsByteString
    defineBuiltinTerm 'Builtins.lessThanByteString $ mkBuiltin PLC.LessThanByteString
    defineBuiltinTerm 'Builtins.greaterThanByteString $ mkBuiltin PLC.GreaterThanByteString
    defineBuiltinTerm 'Builtins.emptyByteString $ PIR.mkConstant () BS.empty
    defineBuiltinTerm 'Builtins.decodeUtf8 $ mkBuiltin PLC.DecodeUtf8

    -- Crypto
    defineBuiltinTerm 'Builtins.verifySignature $ mkBuiltin PLC.VerifySignature

    -- Integer builtins
    defineBuiltinTerm 'Builtins.addInteger $ mkBuiltin PLC.AddInteger
    defineBuiltinTerm 'Builtins.subtractInteger $ mkBuiltin PLC.SubtractInteger
    defineBuiltinTerm 'Builtins.multiplyInteger $ mkBuiltin PLC.MultiplyInteger
    defineBuiltinTerm 'Builtins.divideInteger $ mkBuiltin PLC.DivideInteger
    defineBuiltinTerm 'Builtins.modInteger $ mkBuiltin PLC.ModInteger
    defineBuiltinTerm 'Builtins.quotientInteger $ mkBuiltin PLC.QuotientInteger
    defineBuiltinTerm 'Builtins.remainderInteger $ mkBuiltin PLC.RemainderInteger
    defineBuiltinTerm 'Builtins.greaterThanInteger $ mkBuiltin PLC.GreaterThanInteger
    defineBuiltinTerm 'Builtins.greaterThanEqInteger $ mkBuiltin PLC.GreaterThanEqualsInteger
    defineBuiltinTerm 'Builtins.lessThanInteger $ mkBuiltin PLC.LessThanInteger
    defineBuiltinTerm 'Builtins.lessThanEqInteger $ mkBuiltin PLC.LessThanEqualsInteger
    defineBuiltinTerm 'Builtins.equalsInteger $ mkBuiltin PLC.EqualsInteger

    -- Error
    -- See Note [Delaying error]
    func <- delayedErrorFunc
    defineBuiltinTerm 'Builtins.error func

    -- Strings and chars
    defineBuiltinTerm 'Builtins.appendString $ mkBuiltin PLC.Append
    defineBuiltinTerm 'Builtins.emptyString $ PIR.mkConstant () ("" :: String)
    defineBuiltinTerm 'Builtins.charToString $ mkBuiltin PLC.CharToString
    defineBuiltinTerm 'Builtins.equalsString $ mkBuiltin PLC.EqualsString
    defineBuiltinTerm 'Builtins.trace $ mkBuiltin PLC.Trace
    defineBuiltinTerm 'Builtins.encodeUtf8 $ mkBuiltin PLC.EncodeUtf8

    -- Pairs
    defineBuiltinTerm 'Builtins.fst $ mkBuiltin PLC.FstPair
    defineBuiltinTerm 'Builtins.snd $ mkBuiltin PLC.SndPair
    defineBuiltinTerm 'Builtins.mkPairData $ mkBuiltin PLC.MkPairData

    -- List
    defineBuiltinTerm 'Builtins.null $ mkBuiltin PLC.NullList
    defineBuiltinTerm 'Builtins.head $ mkBuiltin PLC.HeadList
    defineBuiltinTerm 'Builtins.tail $ mkBuiltin PLC.TailList
    defineBuiltinTerm 'Builtins.chooseList $ mkBuiltin PLC.ChooseList
    defineBuiltinTerm 'Builtins.mkNilData $ mkBuiltin PLC.MkNilData
    defineBuiltinTerm 'Builtins.mkNilPairData $ mkBuiltin PLC.MkNilPairData
    defineBuiltinTerm 'Builtins.mkCons $ mkBuiltin PLC.MkCons

    -- Data
    defineBuiltinTerm 'Builtins.chooseData $ mkBuiltin PLC.ChooseData
    defineBuiltinTerm 'Builtins.equalsData $ mkBuiltin PLC.EqualsData
    defineBuiltinTerm 'Builtins.mkConstr $ mkBuiltin PLC.ConstrData
    defineBuiltinTerm 'Builtins.mkMap $ mkBuiltin PLC.MapData
    defineBuiltinTerm 'Builtins.mkList $ mkBuiltin PLC.ListData
    defineBuiltinTerm 'Builtins.mkI $ mkBuiltin PLC.IData
    defineBuiltinTerm 'Builtins.mkB $ mkBuiltin PLC.BData
    defineBuiltinTerm 'Builtins.unsafeDataAsConstr $ mkBuiltin PLC.UnConstrData
    defineBuiltinTerm 'Builtins.unsafeDataAsMap $ mkBuiltin PLC.UnMapData
    defineBuiltinTerm 'Builtins.unsafeDataAsList $ mkBuiltin PLC.UnListData
    defineBuiltinTerm 'Builtins.unsafeDataAsB $ mkBuiltin PLC.UnBData
    defineBuiltinTerm 'Builtins.unsafeDataAsI $ mkBuiltin PLC.UnIData

defineBuiltinTypes
    :: CompilingDefault uni fun m
    => m ()
defineBuiltinTypes = do
    defineBuiltinType ''BS.ByteString $ PLC.toTypeAst $ Proxy @BS.ByteString
    defineBuiltinType ''Integer $ PLC.toTypeAst $ Proxy @Integer
    defineBuiltinType ''Builtins.BuiltinBool $ PLC.toTypeAst $ Proxy @Bool
    defineBuiltinType ''Builtins.BuiltinUnit $ PLC.toTypeAst $ Proxy @()
    defineBuiltinType ''Builtins.BuiltinString $ PLC.toTypeAst $ Proxy @String
    defineBuiltinType ''Char $ PLC.toTypeAst $ Proxy @Char
    defineBuiltinType ''Builtins.BuiltinData $ PLC.toTypeAst $ Proxy @PLC.Data
    defineBuiltinType ''Builtins.BuiltinPair $ PLC.TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoPair)
    defineBuiltinType ''Builtins.BuiltinList $ PLC.TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoList)

-- | Lookup a builtin term by its TH name. These are assumed to be present, so fails if it cannot find it.
lookupBuiltinTerm :: Compiling uni fun m => TH.Name -> m (PIRTerm uni fun)
lookupBuiltinTerm name = do
    ghcName <- GHC.getName <$> getThing name
    maybeTerm <- PIR.lookupTerm () (LexName ghcName)
    case maybeTerm of
        Just t  -> pure t
        Nothing -> throwSd CompilationError $ "Missing builtin definition:" GHC.<+> (GHC.text $ show name)

-- | Lookup a builtin type by its TH name. These are assumed to be present, so fails if it is cannot find it.
lookupBuiltinType :: Compiling uni fun m => TH.Name -> m (PIRType uni)
lookupBuiltinType name = do
    ghcName <- GHC.getName <$> getThing name
    maybeType <- PIR.lookupType () (LexName ghcName)
    case maybeType of
        Just t  -> pure t
        Nothing -> throwSd CompilationError $ "Missing builtin definition:" GHC.<+> (GHC.text $ show name)

-- | The function 'error :: forall a . a'.
errorFunc :: Compiling uni fun m => m (PIRTerm uni fun)
errorFunc = do
    n <- safeFreshTyName "e"
    pure $ PIR.TyAbs () n (PIR.Type ()) (PIR.Error () (PIR.TyVar () n))

-- | The delayed error function 'error :: forall a . () -> a'.
delayedErrorFunc :: CompilingDefault uni fun m => m (PIRTerm uni fun)
delayedErrorFunc = do
    n <- safeFreshTyName "a"
    t <- liftQuote (freshName "thunk")
    let ty = PLC.toTypeAst $ Proxy @()
    pure $ PIR.TyAbs () n (PIR.Type ()) $ PIR.LamAbs () t ty $ PIR.Error () (PIR.TyVar () n)

-- | The type 'forall a. a'.
errorTy :: Compiling uni fun m => m (PIRType uni)
errorTy = do
    tyname <- safeFreshTyName "a"
    pure $ PIR.TyForall () tyname (PIR.Type ()) (PIR.TyVar () tyname)
