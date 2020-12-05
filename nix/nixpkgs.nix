let
  rev = "a52850e30442aa0b058a7afa328679da4d38407f";
  extractedTarball = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "sha256:19frnv2pxcfi60gkal587xs9lpj4mjxxkcc26yvia9526yqg9k6l";
  };
in
  # extractedTarball will be a directory here, and 'import' will automatically append /default.nix here
  import extractedTarball
