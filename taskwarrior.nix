{ mkDerivation, aeson, base, bytestring, hspec, hspec-discover
, process, QuickCheck, quickcheck-instances, random, stdenv
, string-interpolate, text, time, unordered-containers, uuid
}:
mkDerivation {
  pname = "taskwarrior";
  version = "0.1.2.0";
  sha256 = "972b4f4d070fd2174935a88a58f6d8d5b371fb1f1b6dbae921ccadd4c6a2b5ce";
  libraryHaskellDepends = [
    aeson base bytestring process random string-interpolate text time
    unordered-containers uuid
  ];
  testHaskellDepends = [
    aeson base hspec QuickCheck quickcheck-instances text time
    unordered-containers uuid
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/maralorn/haskell-taskwarrior";
  description = "Types and aeson instances for taskwarrior tasks";
  license = stdenv.lib.licenses.agpl3Plus;
}
