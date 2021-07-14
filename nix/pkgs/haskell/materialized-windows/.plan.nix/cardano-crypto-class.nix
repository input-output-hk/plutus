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
      identifier = { name = "cardano-crypto-class"; version = "2.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Type classes abstracting over cryptography primitives for Cardano";
      description = "Type classes abstracting over cryptography primitives for Cardano";
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
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        pkgconfig = [
          (pkgconfPkgs."libsodium" or (errorHandler.pkgConfDepError "libsodium"))
          ];
        buildable = true;
        modules = [
          "Cardano/Crypto/DSIGN"
          "Cardano/Crypto/Hash"
          "Cardano/Crypto/KES"
          "Cardano/Crypto/VRF"
          "Cardano/Crypto/DSIGN/Class"
          "Cardano/Crypto/DSIGN/Ed25519"
          "Cardano/Crypto/DSIGN/Ed448"
          "Cardano/Crypto/DSIGN/Mock"
          "Cardano/Crypto/DSIGN/NeverUsed"
          "Cardano/Crypto/Hash/Blake2b"
          "Cardano/Crypto/Hash/Class"
          "Cardano/Crypto/Hash/MD5"
          "Cardano/Crypto/Hash/NeverUsed"
          "Cardano/Crypto/Hash/SHA256"
          "Cardano/Crypto/Hash/SHA3_256"
          "Cardano/Crypto/Hash/Short"
          "Cardano/Crypto/KES/Class"
          "Cardano/Crypto/KES/Mock"
          "Cardano/Crypto/KES/NeverUsed"
          "Cardano/Crypto/KES/Simple"
          "Cardano/Crypto/KES/Single"
          "Cardano/Crypto/KES/Sum"
          "Cardano/Crypto/PinnedSizedBytes"
          "Cardano/Crypto/Seed"
          "Cardano/Crypto/Util"
          "Cardano/Crypto/VRF/Class"
          "Cardano/Crypto/VRF/Mock"
          "Cardano/Crypto/VRF/NeverUsed"
          "Cardano/Crypto/VRF/Simple"
          "Cardano/Crypto/Libsodium"
          "Cardano/Crypto/Libsodium/C"
          "Cardano/Crypto/Libsodium/Constants"
          "Cardano/Crypto/Libsodium/DSIGN"
          "Cardano/Crypto/Libsodium/Hash"
          "Cardano/Crypto/Libsodium/Init"
          "Cardano/Crypto/Libsodium/Memory"
          "Cardano/Crypto/Libsodium/Memory/Internal"
          "Cardano/Crypto/Libsodium/MLockedBytes"
          "Cardano/Crypto/Libsodium/MLockedBytes/Internal"
          "Cardano/Crypto/Libsodium/UnsafeC"
          "Cardano/Foreign"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "test-memory-example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            ] ++ (pkgs.lib).optional (system.isLinux || system.isOsx) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          hsSourceDirs = [ "memory-example" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "4";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "4";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/cardano-crypto-class; echo source root reset to \$sourceRoot";
    }