{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, ghcjs-base, hsx2hs, http-types, lens, stdenv,
        text, time, isomaniac, servant, servant-isomaniac, patches-vector
      }:
      mkDerivation {
        pname = "markdown-editor";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ aeson base bytestring ghcjs-base hsx2hs http-types lens isomaniac servant servant-isomaniac text time patches-vector ];
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
