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
      specVersion = "2.0";
      identifier = { name = "plutus-use-cases"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "jann.mueller@iohk.io";
      author = "Manuel M T Chakravarty, Jann Müller";
      homepage = "";
      url = "";
      synopsis = "Collection of smart contracts to develop the plutus/wallet interface";
      description = "Collection of smart contracts to develop the plutus/wallet interface.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
          (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
          (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
          ];
        buildable = true;
        modules = [
          "Plutus/Contracts"
          "Plutus/Contracts/Auction"
          "Plutus/Contracts/TokenAccount"
          "Plutus/Contracts/Crowdfunding"
          "Plutus/Contracts/Currency"
          "Plutus/Contracts/ErrorHandling"
          "Plutus/Contracts/Escrow"
          "Plutus/Contracts/SimpleEscrow"
          "Plutus/Contracts/Future"
          "Plutus/Contracts/GameStateMachine"
          "Plutus/Contracts/Governance"
          "Plutus/Contracts/MultiSig"
          "Plutus/Contracts/MultiSigStateMachine"
          "Plutus/Contracts/PingPong"
          "Plutus/Contracts/Prism"
          "Plutus/Contracts/Prism/Credential"
          "Plutus/Contracts/Prism/STO"
          "Plutus/Contracts/Prism/Mirror"
          "Plutus/Contracts/Prism/StateMachine"
          "Plutus/Contracts/Prism/Unlock"
          "Plutus/Contracts/PubKey"
          "Plutus/Contracts/Stablecoin"
          "Plutus/Contracts/Swap"
          "Plutus/Contracts/Uniswap"
          "Plutus/Contracts/Uniswap/OnChain"
          "Plutus/Contracts/Uniswap/OffChain"
          "Plutus/Contracts/Uniswap/Pool"
          "Plutus/Contracts/Uniswap/Trace"
          "Plutus/Contracts/Uniswap/Types"
          "Plutus/Contracts/Vesting"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "plutus-use-cases-scripts" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."row-types" or (errorHandler.buildDepError "row-types"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            ];
          buildable = true;
          modules = [
            "Spec/Auction"
            "Spec/Crowdfunding"
            "Spec/Currency"
            "Spec/ErrorHandling"
            "Spec/Escrow"
            "Spec/SimpleEscrow"
            "Spec/Future"
            "Spec/GameStateMachine"
            "Spec/Governance"
            "Spec/MultiSig"
            "Spec/MultiSigStateMachine"
            "Spec/PingPong"
            "Spec/PubKey"
            "Spec/Prism"
            "Spec/Rollup"
            "Spec/Stablecoin"
            "Spec/TokenAccount"
            "Spec/Vesting"
            ];
          hsSourceDirs = [ "scripts" "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "plutus-use-cases-test" = {
          depends = [
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."row-types" or (errorHandler.buildDepError "row-types"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            ];
          buildable = true;
          modules = [
            "Spec/Auction"
            "Spec/Crowdfunding"
            "Spec/Currency"
            "Spec/ErrorHandling"
            "Spec/Escrow"
            "Spec/SimpleEscrow"
            "Spec/Future"
            "Spec/GameStateMachine"
            "Spec/Governance"
            "Spec/MultiSig"
            "Spec/MultiSigStateMachine"
            "Spec/PingPong"
            "Spec/PubKey"
            "Spec/Prism"
            "Spec/Rollup"
            "Spec/Stablecoin"
            "Spec/Uniswap"
            "Spec/TokenAccount"
            "Spec/Vesting"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      benchmarks = {
        "plutus-use-cases-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            ];
          buildable = true;
          modules = [ "Scott" "Recursion" "IFix" "Opt" ];
          hsSourceDirs = [ "bench" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../plutus-use-cases; }