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
      identifier = { name = "purescript-bridge"; version = "0.13.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "robert . klotzner A T gmx . at";
      author = "Robert Klotzner";
      homepage = "";
      url = "";
      synopsis = "Generate PureScript data types from Haskell data types";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
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
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."wl-pprint-text" or (errorHandler.buildDepError "wl-pprint-text"))
          (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
          ];
        buildable = true;
        modules = [
          "Language/PureScript/Bridge"
          "Language/PureScript/Bridge/CodeGenSwitches"
          "Language/PureScript/Bridge/Builder"
          "Language/PureScript/Bridge/Primitives"
          "Language/PureScript/Bridge/Printer"
          "Language/PureScript/Bridge/PSTypes"
          "Language/PureScript/Bridge/SumType"
          "Language/PureScript/Bridge/Tuple"
          "Language/PureScript/Bridge/TypeInfo"
          "Language/PureScript/Bridge/TypeParameters"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."purescript-bridge" or (errorHandler.buildDepError "purescript-bridge"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."wl-pprint-text" or (errorHandler.buildDepError "wl-pprint-text"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-expectations-pretty-diff" or (errorHandler.buildDepError "hspec-expectations-pretty-diff"))
            ];
          buildable = true;
          modules = [ "TestData" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "1";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "1";
      rev = "minimal";
      sha256 = "";
      };
    }