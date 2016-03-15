{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, acid-state, base, bytestring, containers
      , filepath, happstack-server, hsp, lens, mtl
      , safecopy, servant, servant-happstack, stdenv, text, time
      , web-routes, cabal-install, cabal2nix, patches-vector, hsx2hs
      }:
      mkDerivation {
        pname = "editor-server";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          acid-state base bytestring containers filepath
          happstack-server hsp lens mtl safecopy servant servant-happstack
          text time web-routes patches-vector hsx2hs
        ];
        buildTools = [ cabal-install cabal2nix ];
        description = "Server backend for the wysiwyg editor";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
