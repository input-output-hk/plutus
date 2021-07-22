{ makeTest, plutus-pab, marlowe-dashboard }:
makeTest {
  name = "pab";
  skipLint = true;
  machine = { pkgs, ... }:
    {
      imports = [ ../../modules/pab.nix ];
      environment.systemPackages = with pkgs; [ curl ];
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
  testScript = ''
    # fmt: off
    machine.start()
    machine.wait_for_unit("pab.service")

    machine.wait_for_open_port(8080)
    machine.wait_for_open_port(8083)
    machine.wait_for_open_port(8081)
    machine.wait_for_open_port(8082)

    with subtest("********************************************************************************************* TEST: Serves static files from config"):
      res = machine.succeed("curl -s localhost:8080 | grep marlowe-dashboard")
      assert "marlowe-dashboard" in res, "Expected string 'marlowe-dashboard' in served content. Actual: {}".format(res)
  '';

}
