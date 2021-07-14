########################################################################
# default.nix -- The top-level nix build file for Plutus.
#
# This file defines various attributes that are used for building and
# developing Plutus.
#
########################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? { allowUnfreePredicate = (import ./nix/lib/unfree.nix).unfreePredicate; }
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { inherit system; } // sourcesOverride
, isInFlake ? false
, haskellNix ? import sources."haskell.nix" {
    sourcesOverride = {
      hackage = sources."hackage.nix";
      stackage = sources."stackage.nix";
    };
  }
, packages ? import ./nix { inherit system sources crossSystem config sourcesOverride haskellNix isInFlake checkMaterialization enableHaskellProfiling; }
  # An explicit git rev to use, passed when we are in Hydra
  # Whether to check that the pinned shas for haskell.nix are correct. We want this to be
  # false, generally, since it does more work, but we set it to true in the CI
, checkMaterialization ? false
  # Whether to build our Haskell packages (and their dependencies) with profiling enabled.
, enableHaskellProfiling ? false
}:
let
  inherit (packages) pkgs plutus sources;
  inherit (pkgs) lib haskell-nix;
  inherit (plutus) haskell agdaPackages;
  inherit (plutus) easyPS sphinxcontrib-haddock;
  noCross = x: if crossSystem == null then x else {};
in
rec {
  inherit pkgs plutus;

  inherit (plutus) web-ghc;

  inherit (haskell.packages.plutus-pab.components.exes)
    plutus-game
    plutus-currency
    plutus-atomic-swap
    plutus-pay-to-wallet;

  inherit (haskell.packages.marlowe.components.exes)
    marlowe-app
    marlowe-companion-app
    marlowe-follow-app;

  webCommon = pkgs.callPackage ./web-common { inherit (plutus.lib) gitignore-nix; };
  webCommonPlutus = pkgs.callPackage ./web-common-plutus { inherit (plutus.lib) gitignore-nix; };
  webCommonMarlowe = pkgs.callPackage ./web-common-marlowe { inherit (plutus.lib) gitignore-nix; };
  webCommonPlayground = pkgs.callPackage ./web-common-playground { inherit (plutus.lib) gitignore-nix; };

  plutus-playground = pkgs.recurseIntoAttrs rec {
    haddock = plutus.plutus-haddock-combined;

    inherit (pkgs.callPackage ./plutus-playground-client {
      inherit (plutus.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit haskell webCommon webCommonPlutus webCommonPlayground;
    }) client server generate-purescript start-backend;
  };

  marlowe-playground = pkgs.recurseIntoAttrs rec {
    inherit (pkgs.callPackage ./marlowe-playground-client {
      inherit (plutus.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit haskell webCommon webCommonMarlowe webCommonPlayground;
    }) client server generate-purescript start-backend;
  };

  marlowe-dashboard = pkgs.recurseIntoAttrs rec {
    inherit (pkgs.callPackage ./marlowe-dashboard-client {
      inherit plutus-pab marlowe-app marlowe-companion-app marlowe-follow-app;
      inherit (plutus.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit webCommon webCommonMarlowe;
    }) client server-invoker generated-purescript generate-purescript contractsJSON install-marlowe-contracts;
  };

  marlowe-dashboard-fake-pab = pkgs.recurseIntoAttrs rec {
    inherit (pkgs.callPackage ./fake-pab {
      inherit plutus-pab marlowe-app marlowe-companion-app marlowe-follow-app;
      inherit (plutus.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit haskell webCommon webCommonMarlowe;
    }) client fake-pab-exe fake-pab-generated-purescript;
  };

  marlowe-marketplace = pkgs.recurseIntoAttrs rec {
    inherit (pkgs.callPackage ./marlowe-marketplace-client {
      inherit (plutus.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit webCommon webCommonMarlowe;
    }) client;
  };

  marlowe-web = pkgs.callPackage ./marlowe-website { inherit (plutus.lib) npmlock2nix gitignore-nix; };

  plutus-pab = pkgs.recurseIntoAttrs (pkgs.callPackage ./plutus-pab-client {
    inherit (plutus.lib) buildPursPackage buildNodeModules gitignore-nix filterNpm;
    inherit haskell webCommon webCommonPlutus;
  });

  tests = import ./nix/tests/default.nix {
    inherit pkgs docs;
    inherit (plutus.lib) gitignore-nix;
    inherit (plutus) fixStylishHaskell fixPurty fixPngOptimization;
    inherit (pkgs) terraform;
    inherit plutus-playground marlowe-playground marlowe-dashboard web-ghc plutus-pab
      marlowe-app marlowe-companion-app marlowe-follow-app;
    src = ./.;
  };

  docs = noCross (import ./nix/docs.nix { inherit pkgs plutus; });

  deployment = noCross (pkgs.recurseIntoAttrs (pkgs.callPackage ./deployment/morph {
    plutus = {
      inherit plutus-pab marlowe-app marlowe-companion-app marlowe-follow-app
        marlowe-dashboard marlowe-playground plutus-playground web-ghc docs marlowe-web;
    };
  }));

  # This builds a vscode devcontainer that can be used with the plutus-starter project (or probably the plutus project itself).
  devcontainer = noCross (import ./nix/devcontainer/plutus-devcontainer.nix { inherit pkgs plutus; });
  build-and-push-devcontainer-script = noCross (import ./nix/devcontainer/deploy/default.nix { inherit pkgs plutus; });
}
