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
      identifier = { name = "servant-purescript"; version = "0.9.0.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright: (c) 2016 Robert Klotzner";
      maintainer = "robert Dot klotzner A T gmx Dot at";
      author = "Robert Klotzner";
      homepage = "https://github.com/eskimor/servant-purescript#readme";
      url = "";
      synopsis = "Generate PureScript accessor functions for you servant API";
      description = "Please see README.md";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "Readme.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mainland-pretty" or (errorHandler.buildDepError "mainland-pretty"))
          (hsPkgs."purescript-bridge" or (errorHandler.buildDepError "purescript-bridge"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-foreign" or (errorHandler.buildDepError "servant-foreign"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."servant-subscriber" or (errorHandler.buildDepError "servant-subscriber"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        modules = [
          "Servant/PureScript/Internal"
          "Servant/PureScript/CodeGen"
          "Servant/PureScript/Subscriber"
          "Servant/PureScript/MakeRequests"
          "Servant/PureScript"
          "Servant/API/BrowserHeader"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "servant-purescript-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mainland-pretty" or (errorHandler.buildDepError "mainland-pretty"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."purescript-bridge" or (errorHandler.buildDepError "purescript-bridge"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-foreign" or (errorHandler.buildDepError "servant-foreign"))
            (hsPkgs."servant-purescript" or (errorHandler.buildDepError "servant-purescript"))
            (hsPkgs."servant-subscriber" or (errorHandler.buildDepError "servant-subscriber"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "2";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "2";
      rev = "minimal";
      sha256 = "";
      };
    }