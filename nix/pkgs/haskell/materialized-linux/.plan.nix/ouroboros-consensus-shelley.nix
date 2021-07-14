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
    flags = { asserts = false; };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "ouroboros-consensus-shelley";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Shelley ledger integration in the Ouroboros consensus layer";
      description = "";
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
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."shelley-spec-ledger" or (errorHandler.buildDepError "shelley-spec-ledger"))
          (hsPkgs."shelley-spec-non-integral" or (errorHandler.buildDepError "shelley-spec-non-integral"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          ];
        buildable = true;
        modules = [
          "Ouroboros/Consensus/Shelley/Eras"
          "Ouroboros/Consensus/Shelley/Ledger"
          "Ouroboros/Consensus/Shelley/Ledger/Block"
          "Ouroboros/Consensus/Shelley/Ledger/Config"
          "Ouroboros/Consensus/Shelley/Ledger/Forge"
          "Ouroboros/Consensus/Shelley/Ledger/Inspect"
          "Ouroboros/Consensus/Shelley/Ledger/Integrity"
          "Ouroboros/Consensus/Shelley/Ledger/Ledger"
          "Ouroboros/Consensus/Shelley/Ledger/Mempool"
          "Ouroboros/Consensus/Shelley/Ledger/NetworkProtocolVersion"
          "Ouroboros/Consensus/Shelley/Ledger/Query"
          "Ouroboros/Consensus/Shelley/Ledger/PeerSelection"
          "Ouroboros/Consensus/Shelley/Ledger/TPraos"
          "Ouroboros/Consensus/Shelley/Node"
          "Ouroboros/Consensus/Shelley/Node/Serialisation"
          "Ouroboros/Consensus/Shelley/Protocol"
          "Ouroboros/Consensus/Shelley/Protocol/HotKey"
          "Ouroboros/Consensus/Shelley/Protocol/Util"
          "Ouroboros/Consensus/Shelley/ShelleyBased"
          "Ouroboros/Consensus/Shelley/ShelleyHFC"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "6";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "6";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/ouroboros-consensus-shelley; echo source root reset to \$sourceRoot";
    }