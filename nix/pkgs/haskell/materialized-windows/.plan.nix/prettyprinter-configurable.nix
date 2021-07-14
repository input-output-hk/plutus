{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "prettyprinter-configurable";
        version = "0.1.0.0";
        };
      license = "NONE";
      copyright = "";
      maintainer = "plutus@iohk.io";
      author = "David Luposchainsky, effectfully";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Custom";
      isLocal = true;
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.cabal-doctest or (pkgs.buildPackages.cabal-doctest or (errorHandler.setupDepError "cabal-doctest")))
        ];
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          ];
        buildable = true;
        modules = [
          "Text/Fixity"
          "Text/Fixity/Internal"
          "Text/Pretty"
          "Text/PrettyBy"
          "Text/PrettyBy/Default"
          "Text/PrettyBy/Fixity"
          "Text/PrettyBy/Internal"
          "Text/PrettyBy/Internal/Utils"
          "Text/PrettyBy/Monad"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "prettyprinter-configurable-test" = {
          depends = [
            (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-text" or (errorHandler.buildDepError "quickcheck-text"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          modules = [ "Default" "NonDefault" "Universal" "Expr" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        "prettyprinter-configurable-doctest" = {
          depends = [
            (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = true;
          hsSourceDirs = [ "doctest" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../prettyprinter-configurable; }