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
, docs
, vmCompileTests ? false
}:
let
  inherit (pkgs) lib;
  cleanSrc = gitignore-nix.gitignoreSource src;
in
pkgs.recurseIntoAttrs {
  shellcheck = pkgs.callPackage ./shellcheck.nix { src = cleanSrc; };

  stylishHaskell = pkgs.callPackage ./stylish-haskell.nix {
    src = cleanSrc;
    inherit fixStylishHaskell;
  };

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

  vmTests = pkgs.callPackage ./vm.nix {
    inherit vmCompileTests plutus-playground marlowe-playground
      marlowe-dashboard web-ghc plutus-pab docs;
  };
}
