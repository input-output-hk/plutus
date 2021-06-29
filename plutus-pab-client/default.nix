{ pkgs, gitignore-nix, haskell, webCommon, webCommonPlutus, buildPursPackage, buildNodeModules, filterNpm }:
let
  server-setup-invoker = haskell.packages.plutus-pab.components.exes.plutus-pab-setup;
  server-examples-invoker = haskell.packages.plutus-pab.components.exes.plutus-pab-examples;
  test-generator = haskell.packages.plutus-pab.components.exes.plutus-pab-test-psgenerator;

  generated-purescript = pkgs.runCommand "plutus-pab-purescript" { } ''
    mkdir $out
    ln -s ${haskell.packages.plutus-pab.src}/plutus-pab.yaml.sample plutus-pab.yaml
    ${server-setup-invoker}/bin/plutus-pab-setup psgenerator $out
    ${server-examples-invoker}/bin/plutus-pab-examples --config plutus-pab.yaml psapigenerator $out
    ${test-generator}/bin/plutus-pab-test-psgenerator $out
  '';

  # For dev usage
  generate-purescript = pkgs.writeShellScriptBin "plutus-pab-generate-purs" ''
    generatedDir=./generated
    rm -rf $generatedDir
    # There might be local modifications so only copy when missing
    ! test -f ./plutus-pab.yaml && cp ../plutus-pab/plutus-pab.yaml.sample plutus-pab.yaml
    $(nix-build ../default.nix --quiet --no-build-output -A plutus-pab.server-setup-invoker)/bin/plutus-pab-setup psgenerator $generatedDir
    $(nix-build ../default.nix --quiet --no-build-output -A plutus-pab.test-generator)/bin/plutus-pab-test-psgenerator $generatedDir
    $(nix-build ../default.nix --quiet --no-build-output -A plutus-pab.server-examples-invoker)/bin/plutus-pab-examples --config plutus-pab.yaml psapigenerator $generatedDir
  '';

  # For dev usage
  migrate = pkgs.writeShellScriptBin "plutus-pab-migrate" ''
    # There might be local modifications so only copy when missing
    ! test -f ./plutus-pab.yaml && cp ../plutus-pab/plutus-pab.yaml.sample plutus-pab.yaml
    $(nix-build ../default.nix --quiet --no-build-output -A plutus-pab.server-setup-invoker)/bin/plutus-pab-setup migrate
  '';

  # For dev usage
  start-backend = pkgs.writeShellScriptBin "plutus-pab-server" ''
    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8080
    # There might be local modifications so only copy when missing
    ! test -f ./plutus-pab.yaml && cp ../plutus-pab/plutus-pab.yaml.sample plutus-pab.yaml
    $(nix-build ../default.nix --quiet --no-build-output -A plutus-pab.server-examples-invoker)/bin/plutus-pab-examples --config=plutus-pab.yaml webserver
  '';

  # For dev usage
  start-all-servers = pkgs.writeShellScriptBin "plutus-pab-all-servers" ''
    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8080
    # There might be local modifications so only copy when missing
    ! test -f ./plutus-pab.yaml && cp ../plutus-pab/plutus-pab.yaml.sample plutus-pab.yaml
    $(nix-build ../default.nix --quiet --no-build-output -A plutus-pab.server-examples-invoker)/bin/plutus-pab-examples --config=plutus-pab.yaml all-servers
  '';

  # For dev usage
  start-all-servers-m = pkgs.writeShellScriptBin "plutus-pab-all-servers-m" ''
    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8080
    # There might be local modifications so only copy when missing
    ! test -f ./plutus-pab.yaml && cp ../plutus-pab/plutus-pab.yaml.sample plutus-pab.yaml
    $(nix-build ../default.nix --quiet --no-build-output -A plutus-pab.server-examples-invoker)/bin/plutus-pab-examples --config=plutus-pab.yaml -m all-servers
  '';

  cleanSrc = gitignore-nix.gitignoreSource ./.;

  nodeModules = buildNodeModules {
    projectDir = filterNpm cleanSrc;
    packageJson = ./package.json;
    packageLockJson = ./package-lock.json;
  };

  client =
    buildPursPackage {
      inherit pkgs nodeModules;
      src = cleanSrc;
      name = "plutus-pab-client";
      extraSrcs = {
        web-common = webCommon;
        web-common-plutus = webCommonPlutus;
        generated = generated-purescript;
      };
      # ideally we would just use `npm run test` but
      # this executes `spago` which *always* attempts to download
      # remote files (which obviously fails in sandboxed builds)
      checkPhase = ''
        node -e 'require("./output/Test.Main").main()'
      '';
      packages = pkgs.callPackage ./packages.nix { };
      spagoPackages = pkgs.callPackage ./spago-packages.nix { };
    };

  pab-exes = haskell.packages.plutus-pab.components.exes;

  demo-scripts = pkgs.callPackage ./pab-demo-scripts.nix { inherit client pab-exes; };

  mkConf = pkgs.callPackage ./config.nix { };

in
{
  inherit client demo-scripts server-examples-invoker server-setup-invoker test-generator generated-purescript generate-purescript migrate start-backend start-all-servers start-all-servers-m mkConf pab-exes;
}
