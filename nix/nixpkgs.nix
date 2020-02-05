let
  rev = "5e329ff83c864cd0204118fc15c1ce5aed247c53";
  extractedTarball = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "sha256:1jiyjs7c10d0gfgrgzlmhhsszp6sjr14l3rivc1j99hia8cnh8s4";
  };
in
  # extractedTarball will be a directory here, and 'import' will automatically append /default.nix here 
  import extractedTarball
