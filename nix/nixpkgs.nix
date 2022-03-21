# This file is a helper file needed to make Stack's Nix integration use the
# Nixpkgs revision pinned by `niv`.
let
  sources = import ./sources.nix {};
in
  import sources.nixpkgs
