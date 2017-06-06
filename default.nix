{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, alex, algebraic-graphs, array, base
      , containers, happy, mtl, stdenv, transformers
      }:
      mkDerivation {
        pname = "absint";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          algebraic-graphs array base containers mtl transformers
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
