############################################################################
# Extra Haskell packages which we build with haskell.nix, but which aren't
# part of our project's package set themselves.
#
# These are for e.g. developer usage, or for running formatting tests.
############################################################################
{ stdenv
, lib
, haskell-nix
, sources
, index-state
, compiler-nix-name
, checkMaterialization
, buildPackages
, writeShellScript
}:
let
  agdaProject = haskell-nix.hackage-project {
    name = "Agda";
    version = "2.6.2";
    plan-sha256 = lib.removeSuffix "\n" (builtins.readFile (
      if stdenv.hostPlatform.isWindows
        then ./agda-windows.sha
        else ./agda.sha));
    # Should use the index-state from the target cabal.project, but that disables plan-sha256. Fixed
    # in recent haskell.nix, delete the index-state passing when we update.
    inherit compiler-nix-name index-state checkMaterialization;
    modules = [{
      # Agda is a huge pain. They have a special custom setup that compiles the interface files for
      # the Agda that ships with the compiler. These go in the data files for the *library*, but they
      # require the *executable* to compile them, which depends on the library!
      # They get away with it by using the old-style builds and building everything together, we can't
      # do that.
      # So we work around it:
      # - turn off the custom setup
      # - manually compile the executable (fortunately it has no extra dependencies!) and do the
      # compilation at the end of the library derivation.
      packages.Agda.package.buildType = lib.mkForce "Simple";
      packages.Agda.components.library.enableSeparateDataOutput = lib.mkForce true;
      packages.Agda.components.library.postInstall = ''
        # Compile the executable using the package DB we've just made, which contains
        # the main Agda library
        ghc src/main/Main.hs -package-db=$out/package.conf.d -o agda

        # Find all the files in $data
        shopt -s globstar
        files=($data/**/*.agda)
        for f in "''${files[@]}" ; do
          echo "Compiling $f"
          # This is what the custom setup calls in the end
          ./agda --no-libraries --local-interfaces $f
        done
      '';
    }];
  };
  cabalInstallProject = haskell-nix.hackage-project {
    name = "cabal-install";
    version = "3.4.0.0";
    inherit compiler-nix-name index-state checkMaterialization;
    plan-sha256 = lib.removeSuffix "\n" (builtins.readFile ./cabal-install.sha);
  };
  cardanoRepoToolProject = haskell-nix.cabalProject' {
    src = sources.cardano-repo-tool;
    inherit compiler-nix-name index-state checkMaterialization;
    plan-sha256 = lib.removeSuffix "\n" (builtins.readFile ./cardano-repo-tool.sha);
    sha256map = {
      "https://github.com/input-output-hk/nix-archive"."7dcf21b2af54d0ab267f127b6bd8fa0b31cfa49d" = "0mhw896nfqbd2iwibzymydjlb3yivi9gm0v2g1nrjfdll4f7d8ly";
    };
  };
  stylishHaskellProject = haskell-nix.hackage-project {
    name = "stylish-haskell";
    version = "0.12.2.0";
    inherit compiler-nix-name index-state checkMaterialization;
    plan-sha256 = lib.removeSuffix "\n" (builtins.readFile ./stylish-haskell.sha);
  };
  hlintProject = haskell-nix.hackage-project {
    name = "hlint";
    version = "3.2.1";
    inherit compiler-nix-name index-state checkMaterialization;
    plan-sha256 = lib.removeSuffix "\n" (builtins.readFile ./hlint.sha);
    modules = [{ reinstallableLibGhc = false; }];
  };
  # See https://github.com/input-output-hk/nix-tools/issues/97
  hlsShaFile = if stdenv.isLinux then ./hls-linux.sha else ./hls-darwin.sha;
  hlsProject = haskell-nix.cabalProject' {
    src = sources.haskell-language-server;
    inherit compiler-nix-name index-state checkMaterialization;
    plan-sha256 = lib.removeSuffix "\n" (builtins.readFile hlsShaFile);
    modules = [{
      packages.ghcide.patches = [ ../../patches/ghcide_partial_iface.patch ];
      # Workaround for https://github.com/haskell/haskell-language-server/issues/1160
      packages.haskell-language-server.patches = lib.mkIf stdenv.isDarwin [ ../../patches/haskell-language-server-dynamic.patch ];
      # See https://github.com/haskell/haskell-language-server/pull/1382#issuecomment-780472005
      packages.ghcide.flags.ghc-patched-unboxed-bytecode = true;
    }];
  };

  updateShaFile = project: shaFile: writeShellScript "updateShaFile" ''
    ${project.plan-nix.passthru.calculateMaterializedSha} > ${toString shaFile}
  '';
  updateAllShaFiles = writeShellScript "updateShaFiles" ''
    ${updateShaFile cabalInstallProject ./cabal-install.sha}
    ${updateShaFile agdaProject ./agda.sha}
    ${updateShaFile agdaProject.projectCross.mingwW64 ./agda-windows.sha}
    ${updateShaFile stylishHaskellProject ./stylish-haskell.sha}
    ${updateShaFile hlintProject ./hlint.sha}
    ${updateShaFile hlsProject hlsShaFile}
    ${updateShaFile cardanoRepoToolProject ./cardano-repo-tool.sha}
  '';
in
{
  inherit (agdaProject.hsPkgs) Agda;
  inherit (hlsProject.hsPkgs) haskell-language-server hie-bios implicit-hie;
  inherit (cabalInstallProject.hsPkgs) cabal-install;
  inherit (stylishHaskellProject.hsPkgs) stylish-haskell;
  inherit (hlintProject.hsPkgs) hlint;
  inherit (cardanoRepoToolProject.hsPkgs) cardano-repo-tool;
  inherit updateAllShaFiles;
}
