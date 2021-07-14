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
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "plutus-benchmark"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "radu.ometita@iohk.io";
      author = "Radu Ometita";
      homepage = "https://github.com/iohk/plutus#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/input-output-hk/plutus#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [
        "templates/*.tpl"
        "validation/data/crowdfunding/*.flat"
        "validation/data/currency/*.flat"
        "validation/data/escrow/*.flat"
        "validation/data/future/*.flat"
        "validation/data/game-sm/*.flat"
        "validation/data/multisig-sm/*.flat"
        "validation/data/ping-pong/*.flat"
        "validation/data/prism/*.flat"
        "validation/data/pubkey/*.flat"
        "validation/data/stablecoin/*.flat"
        "validation/data/token-account/*.flat"
        "validation/data/uniswap/*.flat"
        "validation/data/vesting/*.flat"
        "validation/data/marlowe/trustfund/*.flat"
        "validation/data/marlowe/zerocoupon/*.flat"
        ];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        modules = [
          "Plutus/Benchmark/Clausify"
          "Plutus/Benchmark/Queens"
          "Plutus/Benchmark/Knights"
          "Plutus/Benchmark/Knights/ChessSetList"
          "Plutus/Benchmark/Knights/KnightHeuristic"
          "Plutus/Benchmark/Knights/Queue"
          "Plutus/Benchmark/Knights/Sort"
          "Plutus/Benchmark/Knights/Utils"
          "Plutus/Benchmark/LastPiece"
          "Plutus/Benchmark/Prime"
          ];
        hsSourceDirs = [ "nofib/src" ];
        };
      exes = {
        "nofib-exe" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark" or (errorHandler.buildDepError "plutus-benchmark"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          hsSourceDirs = [ "nofib/exe" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "plutus-benchmark-nofib-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark" or (errorHandler.buildDepError "plutus-benchmark"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ];
          buildable = true;
          hsSourceDirs = [ "nofib/test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      benchmarks = {
        "nofib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark" or (errorHandler.buildDepError "plutus-benchmark"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ];
          buildable = true;
          modules = [ "Common" "Paths_plutus_benchmark" ];
          hsSourceDirs = [ "nofib/bench" ];
          };
        "nofib-hs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark" or (errorHandler.buildDepError "plutus-benchmark"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          modules = [ "Common" "Paths_plutus_benchmark" ];
          hsSourceDirs = [ "nofib/bench" ];
          };
        "validation" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          modules = [ "Paths_plutus_benchmark" ];
          hsSourceDirs = [ "validation" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../plutus-benchmark; }