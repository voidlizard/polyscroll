{
description = "offgrid";

inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";

    binary-bits.url = "github:ivanovs-4/binary-bits";
    binary-bits.inputs.nixpkgs.follows = "nixpkgs";
    binary-bits.inputs.haskell-flake-utils.follows = "haskell-flake-utils";

    codec.url = "github:ivanovs-4/codec";
    codec.inputs.nixpkgs.follows = "nixpkgs";
    codec.inputs.haskell-flake-utils.follows = "haskell-flake-utils";
    codec.inputs.binary-bits.follows = "binary-bits";

};

outputs = { self, nixpkgs, haskell-flake-utils, ... }@inputs:
  haskell-flake-utils.lib.simpleCabal2flake {
      inherit self nixpkgs;
      systems = [ "x86_64-linux" ];

      name = "polyscroll";

      shellWithHoogle = true;

      haskellFlakes = with inputs; [
        binary-bits
        codec
      ];

      hpPreOverrides = { pkgs }: new: old:
        with pkgs.haskell.lib;
        with haskell-flake-utils.lib;
        tunePackages pkgs old {
          mr-env            = [ (jailbreakUnbreak pkgs) dontCheck ];
          thread-supervisor = [ (jailbreakUnbreak pkgs) dontCheck ];
          higgledy          = [ (jailbreakUnbreak pkgs) dontCheck ];
        };

      packagePostOverrides = { pkgs }: with pkgs; with haskell.lib; [
        disableExecutableProfiling
        disableLibraryProfiling
        dontBenchmark
        dontCoverage
        dontDistribute
        dontHaddock
        dontHyperlinkSource
        doStrip
        enableDeadCodeElimination
        justStaticExecutables

        dontCheck
      ];

      shellExtBuildInputs = {pkgs}: with pkgs; [
        haskellPackages.haskell-language-server
        jq
      ];

  };
}


