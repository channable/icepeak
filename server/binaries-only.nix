# This is a helper file that strips unneeded dependencies from the derivation
# produced by `default.nix`. Doing this reduces the closure size of the
# resulting derivation from around 3 GiB to around 50 MiB.
{
  # As in default.nix.
  getPkgs ? import ../nix/nixpkgs.nix,
  getInput ? import ./default.nix,
}:

let
  pkgs = getPkgs { };
  icepeak-server = (getInput { inherit getPkgs; }).icepeak-server;

in pkgs.stdenvNoCC.mkDerivation {
  inherit (icepeak-server) pname;
  version = icepeak-server.version + "-binaries-only";

  phases = [ "installPhase" ];
  installPhase = ''
    # Copy the binaries in the package to the build result.
    mkdir -p $out
    cp --recursive ${icepeak-server}/bin $out/bin
  '';
}
