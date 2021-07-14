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
    flags = { defer-plugin-errors = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "plutus-contract"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "jann.mueller@iohk.io";
      author = "Jann Müller";
      homepage = "https://github.com/iohk/plutus#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/input-output-hk/plutus#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."row-types" or (errorHandler.buildDepError "row-types"))
          (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."quickcheck-dynamic" or (errorHandler.buildDepError "quickcheck-dynamic"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."IntervalMap" or (errorHandler.buildDepError "IntervalMap"))
          (hsPkgs."plutus-chain-index" or (errorHandler.buildDepError "plutus-chain-index"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))) ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true || system.isGhcjs || system.isWindows)) [
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
          ];
        buildable = true;
        modules = [
          "Data/Row/Extras"
          "Data/Text/Extras"
          "Data/UUID/Extras"
          "Plutus/Contract"
          "Plutus/Contract/CardanoAPI"
          "Plutus/Contract/Effects"
          "Plutus/Contract/Request"
          "Plutus/Contract/Checkpoint"
          "Plutus/Contract/Constraints"
          "Plutus/Contract/State"
          "Plutus/Contract/Schema"
          "Plutus/Contract/Trace"
          "Plutus/Contract/Trace/RequestHandler"
          "Plutus/Contract/Servant"
          "Plutus/Contract/Resumable"
          "Plutus/Contract/StateMachine"
          "Plutus/Contract/StateMachine/OnChain"
          "Plutus/Contract/StateMachine/MintingPolarity"
          "Plutus/Contract/StateMachine/ThreadToken"
          "Plutus/Contract/Tx"
          "Plutus/Contract/Types"
          "Plutus/Contract/Util"
          "Plutus/Contract/Wallet"
          "Plutus/Contract/Typed/Tx"
          "Wallet/Emulator"
          "Wallet/Emulator/Types"
          "Wallet/Emulator/Chain"
          "Wallet/Emulator/ChainIndex"
          "Wallet/Emulator/ChainIndex/Index"
          "Wallet/Emulator/Error"
          "Wallet/Emulator/Folds"
          "Wallet/Emulator/LogMessages"
          "Wallet/Emulator/NodeClient"
          "Wallet/Emulator/Notify"
          "Wallet/Emulator/MultiAgent"
          "Wallet/Emulator/Stream"
          "Wallet/Emulator/Wallet"
          "Wallet/Rollup"
          "Wallet/Rollup/Types"
          "Wallet/Rollup/Render"
          "Wallet"
          "Wallet/API"
          "Wallet/Effects"
          "Wallet/Graph"
          "Wallet/Types"
          "Plutus/Trace"
          "Plutus/Trace/Effects/ContractInstanceId"
          "Plutus/Trace/Effects/RunContract"
          "Plutus/Trace/Effects/RunContractPlayground"
          "Plutus/Trace/Effects/EmulatedWalletAPI"
          "Plutus/Trace/Effects/EmulatorControl"
          "Plutus/Trace/Effects/Waiting"
          "Plutus/Trace/Emulator"
          "Plutus/Trace/Emulator/ContractInstance"
          "Plutus/Trace/Emulator/System"
          "Plutus/Trace/Emulator/Types"
          "Plutus/Trace/Playground"
          "Plutus/Trace/Scheduler"
          "Plutus/Trace/Tag"
          ] ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true || system.isGhcjs || system.isWindows)) [
          "Plutus/Contract/Test"
          "Plutus/Contract/Test/ContractModel"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "plutus-contract-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."extensible-effects" or (errorHandler.buildDepError "extensible-effects"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            ];
          buildable = true;
          modules = [
            "Spec/Contract"
            "Spec/Emulator"
            "Spec/Rows"
            "Spec/State"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../plutus-contract; }