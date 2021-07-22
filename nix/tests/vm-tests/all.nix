{ makeTest
, lib
, docs
, plutus-pab
, plutus-playground
, marlowe-playground
, marlowe-dashboard
, web-ghc
, vmCompileTests # when enabled the test tries to compile plutus/marlowe code on webghc
}:
let
  plutusApiRequest = builtins.toFile "plutus-request.json" (builtins.readFile ./contract-api-request.json);
  marloweApiRequest = builtins.toFile "marlowe-request.json" (builtins.readFile ./runghc-api-request.json);
in
makeTest {
  skipLint = true;
  name = "all";
  nodes = {

    # ---------------------------------------------------------------------------------------------------------------
    # pab : 192.168.1.1 - running plutus pab
    # --------------------------------------------------------------------------------------------------------------

    pab = { pkgs, ... }: {
      imports = [ ../../modules/pab.nix ];
      environment.systemPackages = with pkgs; [ curl ];

      networking = {
        firewall.allowedTCPPorts = [ 8080 8081 8082 8083 8084 8085 ];
        dhcpcd.enable = false;
        interfaces.eth1.ipv6.addresses = lib.mkOverride 0 [{ address = "fd00::1"; prefixLength = 64; }];
        interfaces.eth1.ipv4.addresses = lib.mkOverride 0 [{ address = "192.168.1.1"; prefixLength = 24; }];
      };

      services.pab = {
        enable = true;
        pab-setup = plutus-pab.pab-exes.plutus-pab-setup;
        pab-package = plutus-pab.pab-exes.plutus-pab-examples;
        staticContent = marlowe-dashboard.client;
        dbFile = "/var/lib/pab/pab-core.db";
        defaultWallet = 1;
        webserverPort = 8080;
        walletPort = 8081;
        nodePort = 8082;
        chainIndexPort = 8083;
        signingProcessPort = 8084;
        metadataPort = 8085;
      };
    };

    # ---------------------------------------------------------------------------------------------------------------
    # playgrounds : 192.168.1.2 - running plutus/marlowe playgrounds and nginx
    # --------------------------------------------------------------------------------------------------------------

    playgrounds = { pkgs, ... }: {
      imports = [
        ../../modules/plutus-playground.nix
        ../../modules/marlowe-playground.nix
      ];

      networking = {
        firewall.allowedTCPPorts = [ 7070 8080 9090 ];
        extraHosts = ''
          127.0.0.1 plutus-playground
          127.0.0.1 marlowe-playground
          127.0.0.1 marlowe-dashboard
          192.168.1.1 pab
          192.168.1.3 webghc
        '';
        dhcpcd.enable = false;
        interfaces.eth1.ipv6.addresses = lib.mkOverride 0 [{ address = "fd00::2"; prefixLength = 64; }];
        interfaces.eth1.ipv4.addresses = lib.mkOverride 0 [{ address = "192.168.1.2"; prefixLength = 24; }];
      };

      services = {
        marlowe-playground = {
          enable = true;
          port = 4001;
          playground-server-package = marlowe-playground.server;
        };

        plutus-playground = {
          enable = true;
          port = 4000;
          playground-server-package = plutus-playground.server;
          webghcURL = "http://webghc";
        };

        nginx = {
          enable = true;
          recommendedGzipSettings = true;
          recommendedProxySettings = true;
          recommendedOptimisation = true;

          upstreams = {
            plutus-playground.servers."127.0.0.1:4000" = { };
            marlowe-playground.servers."127.0.0.1:4001" = { };
            marlowe-dashboard.servers."192.168.1.1:8080" = { };
          };
          virtualHosts = {
            "marlowe-dashboard" = {
              listen = [{ addr = "0.0.0.0"; port = 7070; }];
              locations = {
                "/" = {
                  proxyPass = "http://192.168.1.1:8080";
                };
              };
            };
            "plutus-playground" = {
              listen = [{ addr = "0.0.0.0"; port = 8080; }];
              locations = {
                "/api" = {
                  proxyPass = "http://plutus-playground";
                  proxyWebsockets = true;
                };
                "^~ /doc/" = {
                  alias = "${docs.site}/";
                  extraConfig = ''
                    error_page 404 = @fallback;
                  '';
                };
                "/" = {
                  root = "${plutus-playground.client}";
                  extraConfig = ''
                    error_page 404 = @fallback;
                  '';
                };
                "@fallback" = {
                  proxyPass = "http://plutus-playground";
                  proxyWebsockets = true;
                  extraConfig = ''
                    error_page 404 = @fallback;
                  '';
                };
              };
            };
            "marlowe-playground" = {
              listen = [{ addr = "0.0.0.0"; port = 9090; }];
              locations = {
                "/" = {
                  root = "${marlowe-playground.client}";
                  extraConfig = ''
                    error_page 404 = @fallback;
                  '';
                };
                "^~ /doc/" = {
                  alias = "${docs.site}/";
                  extraConfig = ''
                    error_page 404 = @fallback;
                  '';
                };
                "/runghc" = {
                  proxyPass = "http://webghc";
                };
                "@fallback" = {
                  proxyPass = "http://marlowe-playground";
                  proxyWebsockets = true;
                };
              };
            };
          };
        };
      };

      environment.systemPackages = with pkgs; [ curl ];
    };

    # ---------------------------------------------------------------------------------------------------------------
    # webghc : 192.168.1.3 - running webghc with plutus/marlowe deps
    # --------------------------------------------------------------------------------------------------------------

    webghc = { pkgs, ... }: {

      virtualisation.memorySize = "1024";

      networking = {
        firewall.allowedTCPPorts = [ 80 ];
        dhcpcd.enable = false;
        interfaces.eth1.ipv6.addresses = lib.mkOverride 0 [{ address = "fd00::3"; prefixLength = 64; }];
        interfaces.eth1.ipv4.addresses = lib.mkOverride 0 [{ address = "192.168.1.3"; prefixLength = 24; }];
      };

      imports = [
        ../../modules/web-ghc.nix
      ];
      services = {
        web-ghc = {
          enable = true;
          port = 80;
          web-ghc-package = web-ghc;
        };
      };
    };
  };
  testScript = ''
    playgrounds.start()
    webghc.start()
    pab.start()

    #
    # assert connectivity
    #
    playgrounds.wait_for_unit("network-online.target")
    pab.wait_for_unit("network-online.target")
    pab.wait_for_unit("pab.service")

    # Refer to `services.pab` configuration  above to see what
    # service each individual port relates to.
    pab.wait_for_open_port(8080)
    pab.wait_for_open_port(8081)
    pab.wait_for_open_port(8082)
    pab.wait_for_open_port(8083)
    pab.wait_for_open_port(8085)
    webghc.wait_for_unit("network-online.target")
    playgrounds.succeed("ping -c1 192.168.1.1")
    playgrounds.succeed("ping -c1 192.168.1.2")
    playgrounds.succeed("ping -c1 192.168.1.3")
    pab.succeed("ping -c1 192.168.1.1")
    pab.succeed("ping -c1 192.168.1.2")
    pab.succeed("ping -c1 192.168.1.3")
    webghc.succeed("ping -c1 192.168.1.1")
    webghc.succeed("ping -c1 192.168.1.2")
    webghc.succeed("ping -c1 192.168.1.3")


    #
    # playground / frontend asserts
    #
    playgrounds.wait_for_unit("marlowe-playground.service")
    playgrounds.wait_for_unit("plutus-playground.service")
    playgrounds.wait_for_unit("nginx.service")
    playgrounds.wait_for_open_port(7070)
    playgrounds.wait_for_open_port(8080)
    playgrounds.wait_for_open_port(9090)

    with subtest("********************************************************************************************* TEST: All content is being served on playgrounds"):
      res = playgrounds.succeed("curl --silent http://plutus-playground:8080/")
      assert "plutus" in res, "Expected string 'plutus' from 'http://plutus-playground:8080'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent http://plutus-playground:8080/doc/")
      assert "The Plutus Platform" in res, "Expected string 'The Plutus Platform' from 'http://plutus-playground:8080/doc/'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent http://plutus-playground:8080/doc/plutus/tutorials/")
      assert "The Plutus Platform" in res, "Expected string 'Tutorials' from 'http://plutus-playground:8080/doc/plutus/tutorials/'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent http://marlowe-playground:9090/")
      assert "marlowe-playground" in res, "Expected string 'marlowe-playground' from 'http://marlowe-playground:9090'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent http://marlowe-playground:9090/doc/")
      assert "marlowe-playground" in res, "Expected string 'The Plutus Platform' from 'http://marlowe-playground:9090/doc'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent http://marlowe-playground:9090/doc/marlowe/tutorials/")
      assert "Tutorials" in res, "Expected string 'Tutorials' from 'http://marlowe-playground:9090/doc/marlowe/tutorials'. Actual: {}".format(res)

      #res = playgrounds.succeed("curl --silent http://marlowe-dashboard:7070/")
      #assert "marlowe-dashboard" in res, "Expected string 'marlowe-dashboard' from 'http://marlowe-dashboard:7070/'. Actual: {}".format(res)

      res = pab.succeed("curl --silent http://localhost:8080/")
      assert "marlowe-dashboard" in res, "Expected string 'marlowe-dashboard' from 'http://marlowe-dashboard:7070/'. Actual: {}".format(res)

    #
    # webghc asserts
    #
    webghc.wait_for_unit("web-ghc.service")
    webghc.wait_for_open_port(80)

    #
    # pab asserts
    #

  '' + lib.optionalString (vmCompileTests) ''
    #
    # plutus-playground / webghc : using api/contract
    # marlowe-playground / webghc : using /runghc
    #
    with subtest("********************************************************************************************* TEST: compilation works"):
      res = playgrounds.succeed("curl --silent -H 'Content-Type: application/json' --request POST --data @${plutusApiRequest} http://plutus-playground:8080/api/contract")
      assert "Right" in res, "Expected response wrapped in 'Right'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent -H 'Content-Type: application/json' --request POST --data @${marloweApiRequest} http://marlowe-playground:9090/runghc")
      assert "Right" in res, "Expected response wrapped in 'Right'. Actual: {}".format(res)
  '';
}
