{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "shelley-spec-ledger-test"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Test helpers from shelley-spec-ledger exposed to other packages";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "cddl-files/shelley.cddl"
        "cddl-files/real/crypto.cddl"
        "cddl-files/mock/extras.cddl"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-byron-test" or (errorHandler.buildDepError "cardano-ledger-byron-test"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
          (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."process-extras" or (errorHandler.buildDepError "process-extras"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."shelley-spec-ledger" or (errorHandler.buildDepError "shelley-spec-ledger"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        modules = [
          "Test/Shelley/Spec/Ledger/Address/Bootstrap"
          "Test/Shelley/Spec/Ledger/Address/CompactAddr"
          "Test/Shelley/Spec/Ledger/ByronTranslation"
          "Test/Shelley/Spec/Ledger/Examples/Federation"
          "Test/Shelley/Spec/Ledger/Rules/ClassifyTraces"
          "Test/Shelley/Spec/Ledger/Rules/TestDeleg"
          "Test/Shelley/Spec/Ledger/Rules/TestPool"
          "Test/Shelley/Spec/Ledger/Rules/TestPoolreap"
          "Test/Shelley/Spec/Ledger/ShelleyTranslation"
          "Test/Cardano/Crypto/VRF/Fake"
          "Test/Shelley/Spec/Ledger/BenchmarkFunctions"
          "Test/Shelley/Spec/Ledger/ConcreteCryptoTypes"
          "Test/Shelley/Spec/Ledger/Examples/Cast"
          "Test/Shelley/Spec/Ledger/Examples/Consensus"
          "Test/Shelley/Spec/Ledger/Generator/Block"
          "Test/Shelley/Spec/Ledger/Generator/Constants"
          "Test/Shelley/Spec/Ledger/Generator/Core"
          "Test/Shelley/Spec/Ledger/Generator/Delegation"
          "Test/Shelley/Spec/Ledger/Generator/Metadata"
          "Test/Shelley/Spec/Ledger/Generator/Presets"
          "Test/Shelley/Spec/Ledger/Generator/Trace/Chain"
          "Test/Shelley/Spec/Ledger/Generator/Trace/DCert"
          "Test/Shelley/Spec/Ledger/Generator/Trace/Ledger"
          "Test/Shelley/Spec/Ledger/Generator/Update"
          "Test/Shelley/Spec/Ledger/Generator/Utxo"
          "Test/Shelley/Spec/Ledger/Generator/EraGen"
          "Test/Shelley/Spec/Ledger/Generator/ScriptClass"
          "Test/Shelley/Spec/Ledger/Generator/ShelleyEraGen"
          "Test/Shelley/Spec/Ledger/Orphans"
          "Test/Shelley/Spec/Ledger/Serialisation/CDDLUtils"
          "Test/Shelley/Spec/Ledger/Serialisation/Generators"
          "Test/Shelley/Spec/Ledger/Serialisation/EraIndepGenerators"
          "Test/Shelley/Spec/Ledger/Serialisation/Generators/Bootstrap"
          "Test/Shelley/Spec/Ledger/Serialisation/Generators/Genesis"
          "Test/Shelley/Spec/Ledger/Serialisation/GoldenUtils"
          "Test/Shelley/Spec/Ledger/Shrinkers"
          "Test/Shelley/Spec/Ledger/Utils"
          "Test/Shelley/Spec/Ledger/PropertyTests"
          "Test/Shelley/Spec/Ledger/Rules/TestChain"
          "Test/TestScenario"
          ];
        hsSourceDirs = [ "src" "test" ];
        };
      tests = {
        "shelley-spec-ledger-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
            (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-ledger-byron-test" or (errorHandler.buildDepError "cardano-ledger-byron-test"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."shelley-spec-ledger-test" or (errorHandler.buildDepError "shelley-spec-ledger-test"))
            (hsPkgs."shelley-spec-ledger" or (errorHandler.buildDepError "shelley-spec-ledger"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          modules = [
            "Test/Control/Iterate/SetAlgebra"
            "Test/Shelley/Spec/Ledger/Address/Bootstrap"
            "Test/Shelley/Spec/Ledger/Address/CompactAddr"
            "Test/Shelley/Spec/Ledger/ByronTranslation"
            "Test/Shelley/Spec/Ledger/Examples"
            "Test/Shelley/Spec/Ledger/Examples/Cast"
            "Test/Shelley/Spec/Ledger/Examples/Combinators"
            "Test/Shelley/Spec/Ledger/Examples/EmptyBlock"
            "Test/Shelley/Spec/Ledger/Examples/Federation"
            "Test/Shelley/Spec/Ledger/Examples/Init"
            "Test/Shelley/Spec/Ledger/Examples/GenesisDelegation"
            "Test/Shelley/Spec/Ledger/Examples/NetworkID"
            "Test/Shelley/Spec/Ledger/Examples/Mir"
            "Test/Shelley/Spec/Ledger/Examples/MirTransfer"
            "Test/Shelley/Spec/Ledger/Examples/PoolLifetime"
            "Test/Shelley/Spec/Ledger/Examples/PoolReReg"
            "Test/Shelley/Spec/Ledger/Examples/TwoPools"
            "Test/Shelley/Spec/Ledger/Examples/Updates"
            "Test/Shelley/Spec/Ledger/Fees"
            "Test/Shelley/Spec/Ledger/MultiSigExamples"
            "Test/Shelley/Spec/Ledger/Pretty"
            "Test/Shelley/Spec/Ledger/PropertyTests"
            "Test/Shelley/Spec/Ledger/Rewards"
            "Test/Shelley/Spec/Ledger/Rules/ClassifyTraces"
            "Test/Shelley/Spec/Ledger/Rules/TestChain"
            "Test/Shelley/Spec/Ledger/Rules/TestDeleg"
            "Test/Shelley/Spec/Ledger/Rules/TestPool"
            "Test/Shelley/Spec/Ledger/Rules/TestPoolreap"
            "Test/Shelley/Spec/Ledger/SafeHash"
            "Test/Shelley/Spec/Ledger/Serialisation"
            "Test/Shelley/Spec/Ledger/Serialisation/CDDL"
            "Test/Shelley/Spec/Ledger/Serialisation/Golden/Address"
            "Test/Shelley/Spec/Ledger/Serialisation/Golden/Encoding"
            "Test/Shelley/Spec/Ledger/Serialisation/Golden/Genesis"
            "Test/Shelley/Spec/Ledger/Serialisation/Tripping/CBOR"
            "Test/Shelley/Spec/Ledger/Serialisation/Tripping/JSON"
            "Test/Shelley/Spec/Ledger/ShelleyTranslation"
            "Test/Shelley/Spec/Ledger/STSTests"
            "Test/Shelley/Spec/Ledger/UnitTests"
            "Test/TestScenario"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Tests.hs" ];
          };
        };
      benchmarks = {
        "mainbench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."shelley-spec-ledger-test" or (errorHandler.buildDepError "shelley-spec-ledger-test"))
            (hsPkgs."shelley-spec-ledger" or (errorHandler.buildDepError "shelley-spec-ledger"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          modules = [
            "Bench/Control/Iterate/SetAlgebra/Bimap"
            "BenchUTxOAggregate"
            "BenchValidation"
            "Shelley/Spec/Ledger/Bench/Gen"
            "Shelley/Spec/Ledger/Bench/Rewards"
            "Test/Shelley/Spec/Ledger/Examples/Cast"
            ];
          hsSourceDirs = [ "bench" "test" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "8";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "8";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/shelley/chain-and-ledger/shelley-spec-ledger-test; echo source root reset to \$sourceRoot";
    }