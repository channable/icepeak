let
  rev = "b937c4c734afffe5cf7bf83d1f85e861b7a8c68c";
  tarball = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "sha256:1w7qi0kan6afr6iz1rkwmij3isdvvc3cmx1jj5hqj6p7d9lcdb3n";
  };
in
  import tarball
