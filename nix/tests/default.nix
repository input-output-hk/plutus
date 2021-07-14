{ pkgs
, gitignore-nix
, fixStylishHaskell
, fixPurty
, fixPngOptimization
, src
, terraform
, plutus-playground
, marlowe-playground
, marlowe-dashboard
, web-ghc
, plutus-pab
, marlowe-app
, marlowe-companion-app
, marlowe-follow-app
, docs
, vmCompileTests ? false
}:
let
  inherit (pkgs) lib;
  cleanSrc = gitignore-nix.gitignoreSource src;
  noCross = x: if pkgs.stdenv.hostPlatform == pkgs.stdenv.buildPlatform then x else {};
in
pkgs.recurseIntoAttrs {
  shellcheck = pkgs.callPackage ./shellcheck.nix { src = cleanSrc; };

  stylishHaskell = noCross (pkgs.callPackage ./stylish-haskell.nix {
    src = cleanSrc;
    inherit fixStylishHaskell;
  });

  purty = pkgs.callPackage ./purty.nix {
    src = cleanSrc;
    inherit fixPurty;
  };

  nixpkgsFmt = pkgs.callPackage ./nixpkgs-fmt.nix {
    src = cleanSrc;
    inherit (pkgs) nixpkgs-fmt;
  };

  pngOptimization = pkgs.callPackage ./png-optimization.nix {
    src = cleanSrc;
    inherit fixPngOptimization;
  };

  vmTests = noCross (pkgs.callPackage ./vm.nix {
    inherit vmCompileTests plutus-playground marlowe-playground
      marlowe-dashboard web-ghc plutus-pab
      marlowe-app marlowe-companion-app marlowe-follow-app docs;
  });
}
