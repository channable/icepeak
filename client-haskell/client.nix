{ aeson
, base
, binary
, bytestring
, http-client
, http-types
, text
, retry
, exceptions
, mkDerivation
, stdenv
, hpack
}:
mkDerivation {
  pname = "icepeak-client";
  version = "0.1.1";

  # Opt in to hpack. We don't commit the cabal file in our repo currently.
  buildTools = [ hpack ];
  preConfigure = "hpack .";

  src =
    let
      srcPaths = builtins.map builtins.toString [
          ./package.yaml
          ./src
          ./src/Client.hs
      ];
    in
      builtins.filterSource
        (path: (type: builtins.elem path srcPaths))
        ./.;

  isLibrary = true;
  isExecutable = false;

  libraryHaskellDepends = [
      aeson base binary bytestring http-client http-types text retry exceptions
  ];

  license = stdenv.lib.licenses.bsd3;
}
