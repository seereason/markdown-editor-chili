{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, stdenv, base, text, patches-vector }:
      mkDerivation {
        pname = "patches-vector";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base text patches-vector ];
        buildTools = [ pkgs.haskellPackages.cabal-install pkgs.haskellPackages.ghc ];
        description = "An WYSIWYG editor for markdown in HTML5";
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
