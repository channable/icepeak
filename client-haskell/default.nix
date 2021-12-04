{ # We expect getPkgs to be a lambda that gives us a package set. Example:
  # the lambda in `nixpkgs/default.nix` without any arguments passed to it.
  # That allows us to pass in config later, while allowing users to pass in
  # their own version of Nixpkgs.
  getPkgs ? import ../nix/nixpkgs.nix
}:

let
  pkgs = getPkgs { };
  icepeak-client = pkgs.haskellPackages.callPackage ./client.nix { };
in
  {
    icepeak-client = icepeak-client;
    stack = pkgs.haskellPackages.stack;
  }
