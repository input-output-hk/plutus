{ config, lib, pkgs, ... }:
let
  inherit (lib) types mkOption mkIf;
  pabExec = pkgs.writeShellScriptBin "pab-exec" ''
    ${cfg.pab-package}/bin/plutus-pab-examples --config=${pabYaml} $*
  '';
  cfg = config.services.pab;

  pabConfig = {
    dbConfig = {
      dbConfigFile = cfg.dbFile;
      dbConfigPoolSize = 20;
    };

    pabWebserverConfig = {
      baseUrl = "http://localhost:${builtins.toString cfg.webserverPort}";
      staticDir = "${cfg.staticContent}";
      permissiveCorsPolicy = false;
    };

    walletServerConfig = {
      baseUrl = "http://localhost:${builtins.toString cfg.walletPort}";
      wallet = {
        getWallet = cfg.defaultWallet;
      };
    };

    nodeServerConfig = {
      mscBaseUrl = "http://localhost:${builtins.toString cfg.nodePort}";
      mscSocketPath = "/tmp/node-server.sock";
      mscRandomTxInterval = 20000000;
      mscSlotConfig = {
        scZeroSlotTime = cfg.zeroSlotTime;
        scSlotLength = cfg.slotLength;
      };
      mscFeeConfig = {
        fcConstantFee = {
          getLovelace = cfg.constantFee;
        };
        fcScriptsFeeFactor = cfg.scriptsFeeFactor;
      };
      mscNetworkId = ""; # Empty string for Mainnet. Put a network magic number in the string to use the Testnet.
      mscKeptBlocks = 100000;
      mscBlockReaper = {
        brcInterval = 6000000;
        brcBlocksToKeep = 100000;
      };
      mscInitialTxWallets = [
        { getWallet = 1; }
        { getWallet = 2; }
        { getWallet = 3; }
      ];
    };

    chainIndexConfig = {
      ciBaseUrl = "http://localhost:${builtins.toString cfg.chainIndexPort}";
      ciWatchedAddresses = [ ];
    };

    requestProcessingConfig = {
      requestProcessingInterval = 1;
    };

    signingProcessConfig = {
      spBaseUrl = "http://localhost:${builtins.toString cfg.signingProcessPort}";
      spWallet = {
        getWallet = "${builtins.toString cfg.defaultWallet}";
      };
    };

    metadataServerConfig = {
      mdBaseUrl = "http://localhost:${builtins.toString cfg.metadataPort}";
    };
  };

  pabYaml = pkgs.writeText "pab.yaml" (builtins.toJSON pabConfig);

in
{
  options.services.pab = {

    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        If enabled the pab service will be started.
      '';
    };

    pab-setup = mkOption {
      type = types.package;
      description = ''
        The pab setup script to execute.
      '';
    };

    pab-package = mkOption {
      type = types.package;
      description = ''
        The pab package to execute.
      '';
    };

    dbFile = mkOption {
      type = types.path;
      default = "/var/lib/pab/pab-core.db";
      description = ''
        Path to the pab database file.
      '';
    };

    staticContent = mkOption {
      type = types.package;
      description = ''
        The static content the webserver should serve.
      '';
    };

    defaultWallet = mkOption {
      type = types.int;
      default = 1;
      description = ''
        The default wallet to operate on.
      '';
    };

    webserverPort = mkOption {
      type = types.port;
      default = 9080;
      description = ''
        Port of the pab 'webserver' component.
      '';
    };

    walletPort = mkOption {
      type = types.port;
      default = 8081;
      description = ''
        Port of the pab 'wallet' component.
      '';
    };

    nodePort = mkOption {
      type = types.port;
      default = 8082;
      description = ''
        Port of the pab 'node' component.
      '';
    };

    chainIndexPort = mkOption {
      type = types.port;
      default = 8083;
      description = ''
        Port of the pab 'chain index' component.
      '';
    };

    signingProcessPort = mkOption {
      type = types.port;
      default = 8084;
      description = ''
        Port of the pab 'signing process' component.
      '';
    };

    metadataPort = mkOption {
      type = types.port;
      default = 8085;
      description = ''
        Port of the pab 'metadata' component.
      '';
    };

    zeroSlotTime = mkOption {
      type = types.int;
      default = 1596059091000; # POSIX time of 2020-07-29T21:44:51Z (Wednesday, July 29, 2020 21:44:51) - Shelley launch time
      description = ''
        POSIX time of slot 0 in milliseconds. Setting this (together with the slot length) enables pure datetime-to-slot mappings.
      '';
    };

    slotLength = mkOption {
      type = types.int;
      default = 1000;
      description = ''
        Length of a slot (in milliseconds).
      '';
    };

    constantFee = mkOption {
      type = types.int;
      default = 10;
      description = ''
        Constant fee per transaction in lovelace.
      '';
    };

    scriptsFeeFactor = mkOption {
      type = types.float;
      default = 1.0;
      description = ''
        Factor by which to multiply the size-dependent scripts fee in lovelace.
      '';
    };

  };

  config = mkIf cfg.enable {

    environment.systemPackages = [ pabExec ];

    systemd.services.pab-init =
      let
        # Note: The db is dropped as a workaround for a problem with
        # eventful which crashes PAB. Currently data persistence is not
        # relevant, but the problem *will* occur again when the DB removal
        # is removed unless the underlying problem is identified/fixed.
        pab-init-cmd = pkgs.writeShellScript "pab-init-cmd" ''
          set -eEuo pipefail

          echo "[pab-init-cmd]: Dropping PAB database file '${cfg.dbFile}'"
          rm -rf ${cfg.dbFile}

          echo "[pab-init-cmd]: Creating new DB '${cfg.dbFile}'"
          ${cfg.pab-setup}/bin/plutus-pab-setup migrate ${cfg.dbFile}
        '';
      in
      {
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          Restart = "no";
          DynamicUser = true;
          StateDirectory = [ "pab" ];
          ExecStart = pab-init-cmd;
        };
      };

    systemd.services.pab = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "pab-init.service" ];
      after = [ "pab-init.service" ];
      serviceConfig = {
        # Runtime behavior
        TimeoutStartSec = "5";
        Restart = "always";
        DynamicUser = true;
        StateDirectory = [ "pab" ];
        ExecStart = "${cfg.pab-package}/bin/plutus-pab-examples --config=${pabYaml} all-servers";

        # Sane defaults for security
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        ProtectKernelModules = true;
        PrivateDevices = true;
        SystemCallArchitectures = "native";
      };
      postStart = ''
        mkdir -p /var/lib/pab
      '';
    };
  };

}
