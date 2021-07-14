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
      specVersion = "2.4";
      identifier = { name = "plutus-metatheory"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "james.chapman@iohk.io";
      author = "James Chapman";
      homepage = "https://github.com/input-output-hk/plutus";
      url = "";
      synopsis = "Command line tool for running plutus core programs";
      description = "";
      buildType = "Custom";
      isLocal = true;
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.process or (pkgs.buildPackages.process or (errorHandler.setupDepError "process")))
        (hsPkgs.buildPackages.turtle or (pkgs.buildPackages.turtle or (errorHandler.setupDepError "turtle")))
        ];
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "README.md"
        "Plutus.agda-lib"
        "src/**/*.lagda"
        "src/**/*.lagda.md"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."ieee754" or (errorHandler.buildDepError "ieee754"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        modules = [
          "MAlonzo/Code/Main"
          "MAlonzo/Code/Agda/Builtin/Bool"
          "MAlonzo/Code/Agda/Builtin/Char"
          "MAlonzo/Code/Agda/Builtin/Equality"
          "MAlonzo/Code/Agda/Builtin/IO"
          "MAlonzo/Code/Agda/Builtin/Int"
          "MAlonzo/Code/Agda/Builtin/List"
          "MAlonzo/Code/Agda/Builtin/Nat"
          "MAlonzo/Code/Agda/Builtin/Sigma"
          "MAlonzo/Code/Agda/Builtin/String"
          "MAlonzo/Code/Agda/Builtin/Unit"
          "MAlonzo/Code/Agda/Primitive"
          "MAlonzo/Code/Algebra/Bundles"
          "MAlonzo/Code/Algebra/Consequences/Base"
          "MAlonzo/Code/Algebra/Consequences/Setoid"
          "MAlonzo/Code/Algebra/Construct/LiftedChoice"
          "MAlonzo/Code/Algebra/Construct/NaturalChoice/Min"
          "MAlonzo/Code/Algebra/Morphism"
          "MAlonzo/Code/Algebra/Properties/BooleanAlgebra"
          "MAlonzo/Code/Algebra/Properties/DistributiveLattice"
          "MAlonzo/Code/Algebra/Properties/Lattice"
          "MAlonzo/Code/Algebra/Properties/Semilattice"
          "MAlonzo/Code/Algebra/Structures"
          "MAlonzo/Code/Algebra/Structures/Biased"
          "MAlonzo/Code/Algorithmic"
          "MAlonzo/Code/Algorithmic/CEKV"
          "MAlonzo/Code/Algorithmic/ReductionEC"
          "MAlonzo/Code/Algorithmic/CC"
          "MAlonzo/Code/Algorithmic/CK"
          "MAlonzo/Code/Algorithmic/Evaluation"
          "MAlonzo/Code/Algorithmic/Reduction"
          "MAlonzo/Code/Algorithmic/RenamingSubstitution"
          "MAlonzo/Code/Builtin"
          "MAlonzo/Code/Builtin/Constant/Term"
          "MAlonzo/Code/Builtin/Constant/Type"
          "MAlonzo/Code/Builtin/Signature"
          "MAlonzo/Code/Category/Applicative/Indexed"
          "MAlonzo/Code/Category/Functor"
          "MAlonzo/Code/Category/Monad/Indexed"
          "MAlonzo/Code/Check"
          "MAlonzo/Code/Data/Bool/Base"
          "MAlonzo/Code/Data/Bool/Properties"
          "MAlonzo/Code/Data/Char/Properties"
          "MAlonzo/Code/Data/Digit"
          "MAlonzo/Code/Data/Empty"
          "MAlonzo/Code/Data/Empty/Irrelevant"
          "MAlonzo/Code/Data/Empty/Polymorphic"
          "MAlonzo/Code/Data/Fin/Base"
          "MAlonzo/Code/Data/Integer"
          "MAlonzo/Code/Data/Integer/Base"
          "MAlonzo/Code/Data/Integer/Properties"
          "MAlonzo/Code/Data/Integer/Show"
          "MAlonzo/Code/Data/List/Base"
          "MAlonzo/Code/Data/List/Categorical"
          "MAlonzo/Code/Data/List/Extrema"
          "MAlonzo/Code/Data/List/Extrema/Core"
          "MAlonzo/Code/Data/List/Membership/DecSetoid"
          "MAlonzo/Code/Data/List/Membership/Propositional"
          "MAlonzo/Code/Data/List/Membership/Propositional/Properties"
          "MAlonzo/Code/Data/List/Membership/Propositional/Properties/Core"
          "MAlonzo/Code/Data/List/Membership/Setoid"
          "MAlonzo/Code/Data/List/Membership/Setoid/Properties"
          "MAlonzo/Code/Data/List/NonEmpty"
          "MAlonzo/Code/Data/List/Properties"
          "MAlonzo/Code/Data/List/Relation/Binary/Equality/Propositional"
          "MAlonzo/Code/Data/List/Relation/Binary/Equality/Setoid"
          "MAlonzo/Code/Data/List/Relation/Binary/Lex/Core"
          "MAlonzo/Code/Data/List/Relation/Binary/Lex/Strict"
          "MAlonzo/Code/Data/List/Relation/Binary/Pointwise"
          "MAlonzo/Code/Data/List/Relation/Binary/Pointwise/Properties"
          "MAlonzo/Code/Data/List/Relation/Unary/All"
          "MAlonzo/Code/Data/List/Relation/Unary/All/Properties"
          "MAlonzo/Code/Data/List/Relation/Unary/AllPairs/Core"
          "MAlonzo/Code/Data/List/Relation/Unary/Any"
          "MAlonzo/Code/Data/List/Relation/Unary/Any/Properties"
          "MAlonzo/Code/Data/Maybe/Base"
          "MAlonzo/Code/Data/Maybe/Relation/Unary/All"
          "MAlonzo/Code/Data/Maybe/Relation/Unary/Any"
          "MAlonzo/Code/Data/Nat/Base"
          "MAlonzo/Code/Data/Nat/DivMod"
          "MAlonzo/Code/Data/Nat/DivMod/Core"
          "MAlonzo/Code/Data/Nat/Divisibility/Core"
          "MAlonzo/Code/Data/Nat/Properties"
          "MAlonzo/Code/Data/Nat/Properties/Core"
          "MAlonzo/Code/Data/Nat/Show"
          "MAlonzo/Code/Data/Product"
          "MAlonzo/Code/Data/Product/Function/Dependent/Propositional"
          "MAlonzo/Code/Data/Product/Function/NonDependent/Propositional"
          "MAlonzo/Code/Data/Product/Function/NonDependent/Setoid"
          "MAlonzo/Code/Data/Product/Properties"
          "MAlonzo/Code/Data/Product/Relation/Binary/Pointwise/NonDependent"
          "MAlonzo/Code/Data/Sign/Base"
          "MAlonzo/Code/Data/String/Base"
          "MAlonzo/Code/Data/String/Properties"
          "MAlonzo/Code/Data/Sum/Base"
          "MAlonzo/Code/Data/Sum/Function/Propositional"
          "MAlonzo/Code/Data/Sum/Function/Setoid"
          "MAlonzo/Code/Data/Sum/Relation/Binary/Pointwise"
          "MAlonzo/Code/Data/These/Base"
          "MAlonzo/Code/Data/Vec/Base"
          "MAlonzo/Code/Data/Vec/Bounded/Base"
          "MAlonzo/Code/Debug/Trace"
          "MAlonzo/Code/Function/Bijection"
          "MAlonzo/Code/Function/Bundles"
          "MAlonzo/Code/Function/Equality"
          "MAlonzo/Code/Function/Equivalence"
          "MAlonzo/Code/Function/HalfAdjointEquivalence"
          "MAlonzo/Code/Function/Injection"
          "MAlonzo/Code/Function/Inverse"
          "MAlonzo/Code/Function/LeftInverse"
          "MAlonzo/Code/Function/Metric/Nat/Bundles"
          "MAlonzo/Code/Function/Metric/Structures"
          "MAlonzo/Code/Function/Related"
          "MAlonzo/Code/Function/Related/TypeIsomorphisms"
          "MAlonzo/Code/Function/Structures"
          "MAlonzo/Code/Function/Surjection"
          "MAlonzo/Code/IO/Primitive"
          "MAlonzo/Code/Induction"
          "MAlonzo/Code/Induction/WellFounded"
          "MAlonzo/Code/Level"
          "MAlonzo/Code/Raw"
          "MAlonzo/Code/Relation/Binary/Bundles"
          "MAlonzo/Code/Relation/Binary/Consequences"
          "MAlonzo/Code/Relation/Binary/Construct/Converse"
          "MAlonzo/Code/Relation/Binary/Construct/NaturalOrder/Left"
          "MAlonzo/Code/Relation/Binary/Construct/NonStrictToStrict"
          "MAlonzo/Code/Relation/Binary/Construct/On"
          "MAlonzo/Code/Relation/Binary/Definitions"
          "MAlonzo/Code/Relation/Binary/Indexed/Heterogeneous/Bundles"
          "MAlonzo/Code/Relation/Binary/Indexed/Heterogeneous/Construct/Trivial"
          "MAlonzo/Code/Relation/Binary/Indexed/Heterogeneous/Structures"
          "MAlonzo/Code/Relation/Binary/Lattice"
          "MAlonzo/Code/Relation/Binary/Properties/Poset"
          "MAlonzo/Code/Relation/Binary/Properties/Preorder"
          "MAlonzo/Code/Relation/Binary/PropositionalEquality"
          "MAlonzo/Code/Relation/Binary/PropositionalEquality/Algebra"
          "MAlonzo/Code/Relation/Binary/PropositionalEquality/Core"
          "MAlonzo/Code/Relation/Binary/PropositionalEquality/Properties"
          "MAlonzo/Code/Relation/Binary/HeterogeneousEquality/Core"
          "MAlonzo/Code/Relation/Binary/Reasoning/Base/Double"
          "MAlonzo/Code/Relation/Binary/Reasoning/Base/Triple"
          "MAlonzo/Code/Relation/Binary/Structures"
          "MAlonzo/Code/Relation/Nullary"
          "MAlonzo/Code/Relation/Nullary/Decidable"
          "MAlonzo/Code/Relation/Nullary/Decidable/Core"
          "MAlonzo/Code/Relation/Nullary/Negation"
          "MAlonzo/Code/Relation/Nullary/Product"
          "MAlonzo/Code/Relation/Nullary/Reflects"
          "MAlonzo/Code/Relation/Nullary/Sum"
          "MAlonzo/Code/Relation/Unary/Properties"
          "MAlonzo/Code/Scoped"
          "MAlonzo/Code/Scoped/CK"
          "MAlonzo/Code/Scoped/Extrication"
          "MAlonzo/Code/Scoped/Reduction"
          "MAlonzo/Code/Scoped/RenamingSubstitution"
          "MAlonzo/Code/Type"
          "MAlonzo/Code/Type/BetaNBE"
          "MAlonzo/Code/Type/BetaNBE/Completeness"
          "MAlonzo/Code/Type/BetaNBE/RenamingSubstitution"
          "MAlonzo/Code/Type/BetaNBE/Soundness"
          "MAlonzo/Code/Type/BetaNormal"
          "MAlonzo/Code/Type/Equality"
          "MAlonzo/Code/Type/RenamingSubstitution"
          "MAlonzo/Code/Utils"
          "MAlonzo/RTE"
          "MAlonzo/Code/Algorithmic/Erasure"
          "MAlonzo/Code/Declarative"
          "MAlonzo/Code/Untyped"
          "MAlonzo/Code/Untyped/Reduction"
          "MAlonzo/Code/Untyped/RenamingSubstitution"
          "MAlonzo/Code/Agda/Builtin/Maybe"
          "MAlonzo/Code/Algebra/Construct/NaturalChoice/Base"
          "MAlonzo/Code/Algebra/Construct/NaturalChoice/Max"
          "MAlonzo/Code/Algebra/Construct/NaturalChoice/MaxOp"
          "MAlonzo/Code/Algebra/Construct/NaturalChoice/MinMaxOp"
          "MAlonzo/Code/Algebra/Construct/NaturalChoice/MinOp"
          "MAlonzo/Code/Category/Comonad"
          "MAlonzo/Code/Data/List/NonEmpty/Base"
          "MAlonzo/Code/Data/List/Relation/Binary/Lex"
          "MAlonzo/Code/Data/List/Relation/Binary/Pointwise/Base"
          "MAlonzo/Code/Data/Maybe/Categorical"
          "MAlonzo/Code/Function/Base"
          "MAlonzo/Code/Function/Identity/Categorical"
          "MAlonzo/Code/Relation/Binary/Construct/Closure/Reflexive"
          "MAlonzo/Code/Relation/Binary/Construct/Closure/Reflexive/Properties"
          "MAlonzo/Code/Relation/Binary/Reasoning/Base/Single"
          "MAlonzo/Code/Relation/Binary/Reasoning/Setoid"
          "MAlonzo/Code/Relation/Nullary/Negation/Core"
          "Opts"
          "Raw"
          "Scoped"
          "Untyped"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "plc-agda" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-metatheory" or (errorHandler.buildDepError "plutus-metatheory"))
            ];
          buildable = true;
          hsSourceDirs = [ "exe" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "test1" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-metatheory" or (errorHandler.buildDepError "plutus-metatheory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.plutus-core.components.exes.plc or (pkgs.buildPackages.plc or (errorHandler.buildToolDepError "plutus-core:plc")))
            (hsPkgs.buildPackages.plutus-core.components.exes.uplc or (pkgs.buildPackages.uplc or (errorHandler.buildToolDepError "plutus-core:uplc")))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "TestSimple.hs" ];
          };
        "test2" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."plutus-metatheory" or (errorHandler.buildDepError "plutus-metatheory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.plutus-core.components.exes.plc or (pkgs.buildPackages.plc or (errorHandler.buildToolDepError "plutus-core:plc")))
            (hsPkgs.buildPackages.plutus-core.components.exes.uplc or (pkgs.buildPackages.uplc or (errorHandler.buildToolDepError "plutus-core:uplc")))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          };
        "test3" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."lazy-search" or (errorHandler.buildDepError "lazy-search"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-metatheory" or (errorHandler.buildDepError "plutus-metatheory"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."size-based" or (errorHandler.buildDepError "size-based"))
            (hsPkgs."Stream" or (errorHandler.buildDepError "Stream"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "TestNEAT.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../plutus-metatheory; }