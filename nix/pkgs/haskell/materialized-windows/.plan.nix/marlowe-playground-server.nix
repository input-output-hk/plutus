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
      specVersion = "1.10";
      identifier = { name = "marlowe-playground-server"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "";
      author = "Pablo Lamela";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."http-conduit" or (errorHandler.buildDepError "http-conduit"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
          (hsPkgs."jwt" or (errorHandler.buildDepError "jwt"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."marlowe" or (errorHandler.buildDepError "marlowe"))
          (hsPkgs."marlowe-symbolic" or (errorHandler.buildDepError "marlowe-symbolic"))
          (hsPkgs."marlowe-actus" or (errorHandler.buildDepError "marlowe-actus"))
          (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
          (hsPkgs."servant-purescript" or (errorHandler.buildDepError "servant-purescript"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
          ];
        buildable = true;
        modules = [
          "API"
          "Marlowe/Config"
          "Marlowe/Contracts"
          "Marlowe/Mermaid"
          "Server"
          "ContractForDifferences"
          "ContractForDifferencesWithOracle"
          "CouponBondGuaranteed"
          "Escrow"
          "EscrowWithCollateral"
          "Example"
          "Swap"
          "ZeroCouponBond"
          ];
        hsSourceDirs = [ "src" "contracts" ];
        };
      exes = {
        "marlowe-playground-server" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."marlowe" or (errorHandler.buildDepError "marlowe"))
            (hsPkgs."marlowe-playground-server" or (errorHandler.buildDepError "marlowe-playground-server"))
            (hsPkgs."marlowe-symbolic" or (errorHandler.buildDepError "marlowe-symbolic"))
            (hsPkgs."marlowe-actus" or (errorHandler.buildDepError "marlowe-actus"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."prometheus" or (errorHandler.buildDepError "prometheus"))
            (hsPkgs."purescript-bridge" or (errorHandler.buildDepError "purescript-bridge"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-foreign" or (errorHandler.buildDepError "servant-foreign"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."servant-purescript" or (errorHandler.buildDepError "servant-purescript"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."web-ghc" or (errorHandler.buildDepError "web-ghc"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          modules = [
            "Webserver"
            "PSGenerator"
            "Types"
            "Escrow"
            "EscrowWithCollateral"
            "Example"
            "CouponBondGuaranteed"
            "ZeroCouponBond"
            "Swap"
            "Option"
            "ContractForDifferences"
            "ContractForDifferencesWithOracle"
            ];
          hsSourceDirs = [ "app" "contracts" ];
          mainPath = [ "Main.hs" ];
          };
        "flow-chart-generator" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
            (hsPkgs."marlowe" or (errorHandler.buildDepError "marlowe"))
            (hsPkgs."marlowe-playground-server" or (errorHandler.buildDepError "marlowe-playground-server"))
            ];
          buildable = true;
          modules = [
            "Types"
            "Escrow"
            "EscrowWithCollateral"
            "Example"
            "CouponBondGuaranteed"
            "ZeroCouponBond"
            "Swap"
            "Option"
            "ContractForDifferences"
            "ContractForDifferencesWithOracle"
            ];
          hsSourceDirs = [ "app" "contracts" ];
          mainPath = [ "FlowChartGenerator.hs" ];
          };
        };
      tests = {
        "marlowe-playground-server-test" = {
          depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-playground-server; }