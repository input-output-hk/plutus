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
      identifier = { name = "plutus-pab"; version = "0.1.0.0"; };
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
        depends = [
          (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
          (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
          (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
          (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."errors" or (errorHandler.buildDepError "errors"))
          (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
          (hsPkgs."generic-arbitrary" or (errorHandler.buildDepError "generic-arbitrary"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."row-types" or (errorHandler.buildDepError "row-types"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."swagger2" or (errorHandler.buildDepError "swagger2"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
          (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
          (hsPkgs."lobemo-backend-ekg" or (errorHandler.buildDepError "lobemo-backend-ekg"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
          (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
          (hsPkgs."beam-sqlite" or (errorHandler.buildDepError "beam-sqlite"))
          (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
          (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.cardano-node.components.exes.cardano-node or (pkgs.buildPackages.cardano-node or (errorHandler.buildToolDepError "cardano-node:cardano-node")))
          (hsPkgs.buildPackages.cardano-cli.components.exes.cardano-cli or (pkgs.buildPackages.cardano-cli or (errorHandler.buildToolDepError "cardano-cli:cardano-cli")))
          ];
        buildable = true;
        modules = [
          "Servant/Extra"
          "Cardano/BM/Data/Tracer/Extras"
          "Cardano/Chain"
          "Cardano/ChainIndex/API"
          "Cardano/ChainIndex/ChainIndex"
          "Cardano/ChainIndex/Client"
          "Cardano/ChainIndex/Server"
          "Cardano/ChainIndex/Types"
          "Cardano/Metadata/API"
          "Cardano/Metadata/Client"
          "Cardano/Metadata/Mock"
          "Cardano/Metadata/Server"
          "Cardano/Metadata/Types"
          "Cardano/Node/API"
          "Cardano/Node/Client"
          "Cardano/Node/Mock"
          "Cardano/Node/RandomTx"
          "Cardano/Node/Server"
          "Cardano/Node/Types"
          "Cardano/Protocol/Socket/Type"
          "Cardano/Protocol/Socket/Server"
          "Cardano/Protocol/Socket/Client"
          "Cardano/Wallet/API"
          "Cardano/Wallet/Client"
          "Cardano/Wallet/Mock"
          "Cardano/Wallet/Server"
          "Cardano/Wallet/Types"
          "Control/Monad/Freer/Delay"
          "Control/Concurrent/Availability"
          "Plutus/PAB/App"
          "Plutus/PAB/Arbitrary"
          "Plutus/PAB/ContractCLI"
          "Plutus/PAB/Core"
          "Plutus/PAB/Core/ContractInstance"
          "Plutus/PAB/Core/ContractInstance/BlockchainEnv"
          "Plutus/PAB/Core/ContractInstance/RequestHandlers"
          "Plutus/PAB/Core/ContractInstance/STM"
          "Plutus/PAB/Db/Beam"
          "Plutus/PAB/Db/Beam/ContractStore"
          "Plutus/PAB/Db/Beam/ContractDefinitionStore"
          "Plutus/PAB/Db/Memory/ContractStore"
          "Plutus/PAB/Effects/Contract"
          "Plutus/PAB/Effects/Contract/Builtin"
          "Plutus/PAB/Effects/Contract/ContractExe"
          "Plutus/PAB/Effects/ContractRuntime"
          "Plutus/PAB/Effects/DbStore"
          "Plutus/PAB/Effects/UUID"
          "Plutus/PAB/Effects/TimeEffect"
          "Plutus/PAB/Instances"
          "Plutus/PAB/Monitoring/Monitoring"
          "Plutus/PAB/Monitoring/PABLogMsg"
          "Plutus/PAB/Monitoring/Config"
          "Plutus/PAB/Monitoring/Util"
          "Plutus/PAB/Webserver/API"
          "Plutus/PAB/Webserver/Handler"
          "Plutus/PAB/Webserver/Server"
          "Plutus/PAB/Webserver/Types"
          "Plutus/PAB/Webserver/WebSocket"
          "Plutus/PAB/Events"
          "Plutus/PAB/Events/Contract"
          "Plutus/PAB/Events/ContractInstanceState"
          "Plutus/PAB/ParseStringifiedJSON"
          "Plutus/PAB/Simulator"
          "Plutus/PAB/Timeout"
          "Plutus/PAB/Types"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "plutus-pab" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."purescript-bridge" or (errorHandler.buildDepError "purescript-bridge"))
            (hsPkgs."row-types" or (errorHandler.buildDepError "row-types"))
            (hsPkgs."servant-purescript" or (errorHandler.buildDepError "servant-purescript"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."lobemo-backend-ekg" or (errorHandler.buildDepError "lobemo-backend-ekg"))
            ];
          buildable = true;
          modules = [ "PSGenerator" "Cli" "Command" "CommandParser" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
          };
        "plutus-uniswap" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          hsSourceDirs = [ "examples/uniswap" ];
          mainPath = [ "Main.hs" ];
          };
        "plutus-game" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          hsSourceDirs = [ "examples/game-contract" ];
          mainPath = [ "Main.hs" ];
          };
        "plutus-currency" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
            ];
          buildable = true;
          hsSourceDirs = [ "examples/currency-contract" ];
          mainPath = [ "Main.hs" ];
          };
        "plutus-atomic-swap" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
            ];
          buildable = true;
          modules = [ "AtomicSwap" ];
          hsSourceDirs = [ "examples/atomic-swap-contract" ];
          mainPath = [ "Main.hs" ];
          };
        "plutus-wait-for-tx" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            ];
          buildable = true;
          hsSourceDirs = [ "examples/wait-for-tx" ];
          mainPath = [ "Main.hs" ];
          };
        "plutus-pay-to-wallet" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            ];
          buildable = true;
          modules = [ "PayToWallet" ];
          hsSourceDirs = [ "examples/pay-to-wallet-contract" ];
          mainPath = [ "Main.hs" ];
          };
        "plutus-pab-test-psgenerator" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."smallcheck" or (errorHandler.buildDepError "smallcheck"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."row-types" or (errorHandler.buildDepError "row-types"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
            (hsPkgs."servant-purescript" or (errorHandler.buildDepError "servant-purescript"))
            (hsPkgs."purescript-bridge" or (errorHandler.buildDepError "purescript-bridge"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          modules = [
            "AtomicSwap"
            "PayToWallet"
            "Plutus/PAB/Effects/Contract/ContractTest"
            "Plutus/PAB/Simulator/Test"
            ];
          hsSourceDirs = [
            "test-psgenerator"
            "test/full"
            "examples/pay-to-wallet-contract"
            "examples/atomic-swap-contract"
            ];
          mainPath = [ "TestPSGenerator.hs" ];
          };
        "prism-mirror" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            ];
          buildable = true;
          hsSourceDirs = [ "examples/prism/mirror" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        "prism-unlock-sto" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            ];
          buildable = true;
          hsSourceDirs = [ "examples/prism/unlock-sto" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        "prism-unlock-exchange" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            ];
          buildable = true;
          hsSourceDirs = [ "examples/prism/unlock-exchange" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        "tx-inject" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-conduit" or (errorHandler.buildDepError "http-conduit"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."rate-limit" or (errorHandler.buildDepError "rate-limit"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."signal" or (errorHandler.buildDepError "signal"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            ];
          buildable = true;
          hsSourceDirs = [ "tx-inject" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        };
      tests = {
        "plutus-pab-test-light" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."smallcheck" or (errorHandler.buildDepError "smallcheck"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            ];
          buildable = true;
          modules = [
            "Cardano/Metadata/ServerSpec"
            "Cardano/Metadata/TypesSpec"
            "Cardano/Wallet/ServerSpec"
            ];
          hsSourceDirs = [ "test/light" ];
          mainPath = [ "Spec.hs" ];
          };
        "plutus-pab-test-full" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
            (hsPkgs."plutus-use-cases" or (errorHandler.buildDepError "plutus-use-cases"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."smallcheck" or (errorHandler.buildDepError "smallcheck"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."row-types" or (errorHandler.buildDepError "row-types"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
            ];
          buildable = true;
          modules = [
            "Plutus/PAB/CoreSpec"
            "Plutus/PAB/Events/ContractSpec"
            "Plutus/PAB/Effects/Contract/ContractTest"
            "Plutus/PAB/Simulator/Test"
            "AtomicSwap"
            "PayToWallet"
            ];
          hsSourceDirs = [
            "test/full"
            "examples/pay-to-wallet-contract"
            "examples/atomic-swap-contract"
            ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../plutus-pab; }