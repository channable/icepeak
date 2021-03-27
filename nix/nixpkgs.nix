let
  rev = "bed08131cd29a85f19716d9351940bdc34834492";
  extractedTarball = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "sha256:19gxrzk9y4g2f09x2a4g5699ccw35h5frznn9n0pbsyv45n9vxix";
  };
in
  # extractedTarball will be a directory here, and 'import' will automatically append /default.nix here
  import extractedTarball
