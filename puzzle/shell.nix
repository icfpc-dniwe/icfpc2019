let pkg =
  { python }:

  python.withPackages (pkgs: with pkgs; [
    # scikitimage
    # matplotlib
    # scipy
    # cython
    # jupyter
    numpy
    pillow
    networkx
  ]);
    
in let pkgs = import <nixpkgs> { };
in (pkgs.callPackage pkg {
  python = pkgs.python3;
}).env
