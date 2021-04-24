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
      specVersion = "1.8";
      identifier = { name = "shelley-spec-ledger"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Shelley Ledger Executable Model";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger" or (errorHandler.buildDepError "cardano-ledger"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."cborg-json" or (errorHandler.buildDepError "cborg-json"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."partial-order" or (errorHandler.buildDepError "partial-order"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."shelley-spec-non-integral" or (errorHandler.buildDepError "shelley-spec-non-integral"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        modules = [
          "Shelley/Spec/Ledger/API/Mempool"
          "Shelley/Spec/Ledger/API/Types"
          "Cardano/Ledger/AuxiliaryData"
          "Cardano/Ledger/Compactible"
          "Cardano/Ledger/Core"
          "Cardano/Ledger/Crypto"
          "Cardano/Ledger/Era"
          "Cardano/Ledger/Pretty"
          "Cardano/Ledger/Shelley"
          "Cardano/Ledger/Shelley/Constraints"
          "Cardano/Ledger/Torsor"
          "Cardano/Ledger/Val"
          "Shelley/Spec/Ledger/Address"
          "Shelley/Spec/Ledger/Address/Bootstrap"
          "Shelley/Spec/Ledger/API"
          "Shelley/Spec/Ledger/API/ByronTranslation"
          "Shelley/Spec/Ledger/API/Protocol"
          "Shelley/Spec/Ledger/API/Validation"
          "Shelley/Spec/Ledger/API/Wallet"
          "Shelley/Spec/Ledger/BaseTypes"
          "Shelley/Spec/Ledger/BlockChain"
          "Shelley/Spec/Ledger/Coin"
          "Shelley/Spec/Ledger/CompactAddr"
          "Shelley/Spec/Ledger/Credential"
          "Shelley/Spec/Ledger/Delegation/Certificates"
          "Shelley/Spec/Ledger/Delegation/PoolParams"
          "Shelley/Spec/Ledger/EpochBoundary"
          "Shelley/Spec/Ledger/Genesis"
          "Shelley/Spec/Ledger/HardForks"
          "Shelley/Spec/Ledger/Hashing"
          "Shelley/Spec/Ledger/Keys"
          "Shelley/Spec/Ledger/LedgerState"
          "Shelley/Spec/Ledger/Metadata"
          "Shelley/Spec/Ledger/OCert"
          "Shelley/Spec/Ledger/Orphans"
          "Shelley/Spec/Ledger/OverlaySchedule"
          "Shelley/Spec/Ledger/PParams"
          "Shelley/Spec/Ledger/Rewards"
          "Shelley/Spec/Ledger/RewardProvenance"
          "Shelley/Spec/Ledger/Scripts"
          "Shelley/Spec/Ledger/Serialization"
          "Shelley/Spec/Ledger/Slot"
          "Shelley/Spec/Ledger/SoftForks"
          "Shelley/Spec/Ledger/StabilityWindow"
          "Shelley/Spec/Ledger/STS/Bbody"
          "Shelley/Spec/Ledger/STS/Chain"
          "Shelley/Spec/Ledger/STS/Deleg"
          "Shelley/Spec/Ledger/STS/Delegs"
          "Shelley/Spec/Ledger/STS/Delpl"
          "Shelley/Spec/Ledger/STS/Epoch"
          "Shelley/Spec/Ledger/STS/EraMapping"
          "Shelley/Spec/Ledger/STS/Ledger"
          "Shelley/Spec/Ledger/STS/Ledgers"
          "Shelley/Spec/Ledger/STS/Mir"
          "Shelley/Spec/Ledger/STS/NewEpoch"
          "Shelley/Spec/Ledger/STS/Newpp"
          "Shelley/Spec/Ledger/STS/Ocert"
          "Shelley/Spec/Ledger/STS/Overlay"
          "Shelley/Spec/Ledger/STS/Pool"
          "Shelley/Spec/Ledger/STS/PoolReap"
          "Shelley/Spec/Ledger/STS/Ppup"
          "Shelley/Spec/Ledger/STS/Prtcl"
          "Shelley/Spec/Ledger/STS/Rupd"
          "Shelley/Spec/Ledger/STS/Snap"
          "Shelley/Spec/Ledger/STS/Tick"
          "Shelley/Spec/Ledger/STS/Tickn"
          "Shelley/Spec/Ledger/STS/Updn"
          "Shelley/Spec/Ledger/STS/Upec"
          "Shelley/Spec/Ledger/STS/Utxo"
          "Shelley/Spec/Ledger/STS/Utxow"
          "Shelley/Spec/Ledger/Tx"
          "Shelley/Spec/Ledger/TxBody"
          "Shelley/Spec/Ledger/UTxO"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/34; }