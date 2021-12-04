let
  rev = "d6a4500f88725c24b82f6f86fb3129ed0561800c";
  extractedTarball = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "sha256:1xvgir3jr0mff9zk3ca2m0mzk6blyhjwmd5flyp3jp83bphr7301";
  };
in
  # extractedTarball will be a directory here, and 'import' will automatically append /default.nix here
  import extractedTarball
