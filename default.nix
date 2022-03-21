# Helper file to make `niv` available for Nixpkgs snapshot updates:
let
  pkgs = import ./nix/nixpkgs.nix {};
in {
  inherit (pkgs) niv;
}
