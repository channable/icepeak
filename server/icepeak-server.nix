{ # We expect getPkgs to be a lambda that gives us a package set. Example:
  # the lambda in `nixpkgs/default.nix` without any arguments passed to it.
  # That allows us to pass in config later, while allowing users to pass in
  # their own version of Nixpkgs.
  getPkgs ? import ../nix/nixpkgs.nix
}:

let
  pkgs = getPkgs { };
  python = pkgs.python310;
  pythonEnv = python.withPackages (p: [
    p.websockets
    p.requests
  ]);
  libs = with pkgs; [ ];
  haskell = pkgs.haskellPackages;
in
  rec {
    icepeak-server = haskell.callPackage ./server.nix { };
    stack = pkgs.stack;
    icepeak-server-env = pkgs.buildEnv {
      name = "icepeak-server-env";
      paths = [
        haskell.stack
        icepeak-server
        pythonEnv
        haskell.haskell-language-server
        haskell.ghcide
        haskell.implicit-hie
        haskell.ghc
      ] ++ libs;
      passthru = { icepeak-server = icepeak-server; stack = stack; };
    };
  }
