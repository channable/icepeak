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
in
  rec {
    icepeak-server = pkgs.haskellPackages.callPackage ./server.nix { };
    stack = pkgs.haskellPackages.stack;
    icepeak-server-env = pkgs.buildEnv {
      name = "icepeak-server-env";
      paths = [
        stack
        icepeak-server
        pythonEnv
      ] ++ libs;
    };
  }
