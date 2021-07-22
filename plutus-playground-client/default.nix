{ pkgs, lib, gitignore-nix, haskell, webCommon, webCommonPlutus, webCommonPlayground, buildPursPackage, buildNodeModules, filterNpm }:
let
  playground-exe = haskell.packages.plutus-playground-server.components.exes.plutus-playground-server;

  build-playground-exe = "$(nix-build --quiet --no-build-output ../default.nix -A plutus.haskell.packages.plutus-playground-server.components.exes.plutus-playground-server)";

  build-ghc-with-plutus = "$(nix-build --quiet --no-build-output -E '(import ./.. {}).plutus.haskell.project.ghcWithPackages(ps: [ ps.plutus-core ps.plutus-tx ps.plutus-contract ps.plutus-ledger ps.playground-common ])')";

  # Output containing the purescript bridge code
  # We need to add ghc with dependecies because `psgenerator` needs to invoke ghc to
  # create test data.
  generated-purescript =
    let
      ghcWithPlutus = haskell.project.ghcWithPackages (ps: [ ps.plutus-core ps.plutus-tx ps.plutus-contract ps.plutus-ledger ps.playground-common ]);
    in
    # For some reason on darwin GHC will complain bout missing otool, I really don't know why
    pkgs.runCommand "plutus-playground-purescript" { buildInputs = lib.optional pkgs.stdenv.isDarwin [ pkgs.darwin.cctools ]; } ''
      PATH=${ghcWithPlutus}/bin:$PATH
      mkdir $out
      ${playground-exe}/bin/plutus-playground-server psgenerator $out
    '';

  # generate-purescript: script to create purescript bridge code
  #
  # * Note-1: We need to add ghc to the path because the purescript generator
  # actually invokes ghc to generate test data so we need ghc with the necessary deps
  #
  # * Note-2: This command is supposed to be available in the nix-shell but want
  # to avoid plutus-core in the shell closure so we do $(nix-build ..) instead
  generate-purescript = pkgs.writeShellScriptBin "plutus-playground-generate-purs" ''
    GHC_WITH_PKGS=${build-ghc-with-plutus}
    export PATH=$GHC_WITH_PKGS/bin:$PATH

    rm -rf ./generated
    ${build-playground-exe}/bin/plutus-playground-server psgenerator generated
  '';

  # start-backend: script to start the plutus-playground-server
  #
  # Note-1: We need to add ghc to the path because the server provides /runghc
  # which needs ghc and dependencies.
  # Note-2: We want to avoid to pull the huge closure in so we use $(nix-build) instead
  start-backend = pkgs.writeShellScriptBin "plutus-playground-server" ''
    echo "plutus-playground-server: for development use only"
    GHC_WITH_PKGS=${build-ghc-with-plutus}
    export PATH=$GHC_WITH_PKGS/bin:$PATH

    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8888
    export GITHUB_CALLBACK_PATH=https://localhost:8009/api/oauth/github/callback

    ${build-playground-exe}/bin/plutus-playground-server webserver "$@"
  '';

  cleanSrc = gitignore-nix.gitignoreSource ./.;

  nodeModules = buildNodeModules {
    projectDir = filterNpm cleanSrc;
    packageJson = ./package.json;
    packageLockJson = ./package-lock.json;
  };

  client = buildPursPackage {
    inherit pkgs nodeModules;
    src = cleanSrc;
    name = "plutus-playground-client";
    # ideally we would just use `npm run test` but
    # this executes `spago` which *always* attempts to download
    # remote files (which obviously fails in sandboxed builds)
    checkPhase = ''
      node -e 'require("./output/Test.Main").main()'
    '';
    extraSrcs = {
      web-common = webCommon;
      web-common-plutus = webCommonPlutus;
      web-common-playground = webCommonPlayground;
      generated = generated-purescript;
    };
    packages = pkgs.callPackage ./packages.nix { };
    spagoPackages = pkgs.callPackage ./spago-packages.nix { };
  };
in
{
  inherit client generate-purescript start-backend;
  server = playground-exe;
}
