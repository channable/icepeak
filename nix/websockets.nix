{ HUnit
, QuickCheck
, SHA
, async
, attoparsec
, base
, base64-bytestring
, binary
, bytestring
, bytestring-builder
, case-insensitive
, clock
, containers
, criterion
, entropy
, mkDerivation
, network
, random
, stdenv
, streaming-commons
, test-framework
, test-framework-hunit
, test-framework-quickcheck2
, text
}:

mkDerivation {
  pname = "websockets";
  version = "0.12.7.0";
  sha256 = "11jz0d7hgbl449dvz789gyf85gdwm6h0klq05vilmplpdx61h4az";

  isLibrary = true;
  isExecutable = true;

  libraryHaskellDepends = [
    SHA
    async
    attoparsec
    base
    base64-bytestring
    binary
    bytestring
    bytestring-builder
    case-insensitive
    clock
    containers
    entropy
    network
    random
    streaming-commons
    text
  ];

  doCheck = false;
  description = "A sensible and clean way to write WebSocket-capable servers in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
