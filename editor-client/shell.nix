{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, ghcjs-base, hsx2hs, http-types, lens, stdenv,
        text, time, isomaniac, servant,  patches-vector, http-api-data, hsp, web-routes, web-routes-th,
        userid, Cabal, servant-isomaniac
      }:
      mkDerivation {
        pname = "markdown-editor";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ aeson base bytestring ghcjs-base hsx2hs http-types lens isomaniac servant hsp text time patches-vector http-api-data web-routes web-routes-th userid Cabal servant-isomaniac ];
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
