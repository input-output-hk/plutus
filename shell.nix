{ system ? builtins.currentSystem
, enableHaskellProfiling ? false
, packages ? import ./. { inherit system enableHaskellProfiling; }
}:
let
  inherit (packages) pkgs plutus plutus-playground marlowe-playground plutus-pab marlowe-dashboard fake-pab deployment docs;
  inherit (pkgs) stdenv lib utillinux python3 nixpkgs-fmt;
  inherit (plutus) haskell agdaPackages stylish-haskell sphinxcontrib-haddock sphinx-markdown-tables sphinxemoji nix-pre-commit-hooks cardano-cli cardano-node;
  inherit (plutus) agdaWithStdlib;
  inherit (plutus) purty purty-pre-commit purs spargo;

  # For Sphinx, and ad-hoc usage
  sphinxTools = python3.withPackages (ps: [
    sphinxcontrib-haddock.sphinxcontrib-domaintools
    sphinx-markdown-tables
    sphinxemoji
    ps.sphinxcontrib_plantuml
    ps.sphinxcontrib-bibtex
    ps.sphinx
    ps.sphinx_rtd_theme
    ps.recommonmark
  ]);

  # Configure project pre-commit hooks
  pre-commit-check = nix-pre-commit-hooks.run {
    src = (lib.cleanSource ./.);
    tools = {
      stylish-haskell = stylish-haskell;
      nixpkgs-fmt = nixpkgs-fmt;
      shellcheck = pkgs.shellcheck;
      purty = purty-pre-commit;
    };
    hooks = {
      purty.enable = true;
      stylish-haskell.enable = true;
      nixpkgs-fmt = {
        enable = true;
        # While nixpkgs-fmt does exclude patterns specified in `.ignore` this
        # does not appear to work inside the hook. For now we have to thus
        # maintain excludes here *and* in `./.ignore` and *keep them in sync*.
        excludes = [ ".*nix/pkgs/haskell/materialized.*/.*" ".*nix/sources.nix$" ".*/spago-packages.nix$" ".*/packages.nix$" ];
      };
      shellcheck.enable = true;
      png-optimization = {
        enable = true;
        name = "png-optimization";
        description = "Ensure that PNG files are optimized";
        entry = "${pkgs.optipng}/bin/optipng";
        files = "\\.png$";
      };
    };
  };

  # build inputs from nixpkgs ( -> ./nix/default.nix )
  nixpkgsInputs = (with pkgs; [
    cacert
    ghcid
    morph
    niv
    nixpkgs-fmt
    nodejs
    shellcheck
    sqlite-interactive
    stack
    z3
    zlib
  ] ++ (lib.optionals (!stdenv.isDarwin) [ rPackages.plotly R ]));

  # local build inputs ( -> ./nix/pkgs/default.nix )
  localInputs = (with plutus; [
    aws-mfa-login
    cabal-install
    cardano-repo-tool
    fixPurty
    fixStylishHaskell
    haskell-language-server
    hie-bios
    hlint
    marlowe-playground.generate-purescript
    marlowe-playground.start-backend
    plutus-playground.generate-purescript
    plutus-playground.start-backend
    plutus-pab.generate-purescript
    plutus-pab.migrate
    plutus-pab.start-backend
    plutus-pab.start-all-servers
    plutus-pab.start-all-servers-m
    purs
    purty
    spago
    stylish-haskell
    updateMaterialized
    updateClientDeps
    updateMetadataSamples
    docs.build-and-serve-docs
  ]);

in
haskell.project.shellFor {
  nativeBuildInputs = nixpkgsInputs ++ localInputs ++ [ agdaWithStdlib sphinxTools ];
  # We don't currently use this, and it's a pain to materialize, and otherwise
  # costs a fair bit of eval time.
  withHoogle = false;

  shellHook = ''
    ${pre-commit-check.shellHook}
  ''
  # Work around https://github.com/NixOS/nix/issues/3345, which makes
  # tests etc. run single-threaded in a nix-shell.
  # Sets the affinity to cores 0-1000 for $$ (current PID in bash)
  # Only necessary for linux - darwin doesn't even expose thread
  # affinity APIs!
  + lib.optionalString stdenv.isLinux ''
    ${utillinux}/bin/taskset -pc 0-1000 $$
  ''
  # It's handy to have an environment variable for the project root (assuming people
  # normally start the shell from there.
  # We also use it in a deployment hack.
  # We have a local passwords store that we use for deployments etc.
  + ''
    export ACTUS_TEST_DATA_DIR=${packages.actus-tests}/tests/
  '';
}
