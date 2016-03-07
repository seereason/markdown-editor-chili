{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, hsx2hs, lens, patches-vector
      , servant, stdenv, text
      }:
      mkDerivation {
        pname = "common";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base containers hsx2hs lens patches-vector servant text
        ];
        homepage = "https://www.github.com/stepcut/markdown-editor";
        description = "A collaborative WYSIWYG editor";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
