{ pkgs, gitignore-nix, haskell, webCommon, webCommonMarlowe, buildPursPackage, buildNodeModules, filterNpm, marlowe-dashboard }:
let
  cleanSrc = gitignore-nix.gitignoreSource ../marlowe-dashboard-client/.;

  fake-pab-exe = haskell.packages.fake-pab.components.exes.fake-pab-server;

  # Output containing the purescript bridge code
  fake-pab-generated-purescript = pkgs.runCommand "marlowe-fake-pab-purescript" { } ''
    mkdir $out
    ${fake-pab-exe}/bin/fake-pab-server psgenerator $out
  '';

  nodeModules = buildNodeModules {
    projectDir = filterNpm cleanSrc;
    packageJson = ../marlowe-dashboard-client/package.json;
    packageLockJson = ../marlowe-dashboard-client/package-lock.json;
    githubSourceHashMap = { };
  };

  client = buildPursPackage {
    inherit pkgs nodeModules;
    src = cleanSrc;
    checkPhase = ''
      node -e 'require("./output/Test.Main").main()'
    '';
    name = "marlowe-dashboard-client-fake-pab";
    extraSrcs = {
      web-common = webCommon;
      web-common-marlowe = webCommonMarlowe;
      generated = marlowe-dashboard.generated-purescript;
      fake-pab-generated = fake-pab-generated-purescript;
    };
    packages = pkgs.callPackage ../marlowe-dashboard-client/packages.nix { };
    spagoPackages = pkgs.callPackage ../marlowe-dashboard-client/spago-packages.nix { };
  };
in
{
  inherit client fake-pab-exe fake-pab-generated-purescript;
}
