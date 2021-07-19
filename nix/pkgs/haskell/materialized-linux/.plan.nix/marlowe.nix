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
      identifier = { name = "marlowe"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "alexander.nemish@iohk.io";
      author = "Alexander Nemish";
      homepage = "";
      url = "";
      synopsis = "Marlowe: financial contracts on Cardano Computation Layer";
      description = "A reference implementation of Marlowe, domain-specific language targeted at\nthe execution of financial contracts in the style of Peyton Jones et al\non Cardano Computation Layer.";
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
          (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."wl-pprint" or (errorHandler.buildDepError "wl-pprint"))
          (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"));
        buildable = true;
        modules = [
          "Language/Marlowe"
          "Language/Marlowe/Extended"
          "Language/Marlowe/Semantics"
          "Language/Marlowe/Client"
          "Language/Marlowe/Util"
          "Language/Marlowe/Pretty"
          "Language/Marlowe/Analysis/FSSemantics"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "marlowe-pab" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."purescript-bridge" or (errorHandler.buildDepError "purescript-bridge"))
            (hsPkgs."marlowe" or (errorHandler.buildDepError "marlowe"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"));
          buildable = true;
          modules = [ "MarloweContracts" ];
          hsSourceDirs = [ "pab" ];
          mainPath = ([
            "Main.hs"
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) "") ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        };
      tests = {
        "marlowe-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."hint" or (errorHandler.buildDepError "hint"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."marlowe" or (errorHandler.buildDepError "marlowe"))
            (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"));
          buildable = true;
          modules = [
            "Spec/Marlowe/Common"
            "Spec/Marlowe/Marlowe"
            "Spec/Marlowe/AutoExecute"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe; }