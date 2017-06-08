{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, alex, fgl, array, base
      , containers, happy, mtl, stdenv, transformers
      , graphviz
      }:
      mkDerivation {
        pname = "absint";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          array base containers mtl transformers fgl graphviz
        ];
        executableToolDepends = [ alex happy ];
        buildDepends = with pkgs; [ cabal-install ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
