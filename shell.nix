{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, directory, filepath
      , hashable, MissingH, process, stdenv, text, time, vty, vty-ui
      }:
      mkDerivation {
        pname = "univOpWrap";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          aeson base bytestring directory filepath hashable MissingH process
          text time vty vty-ui
        ];
        description = "A universal wrapper, which makes searching and opening of files nicer";
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
