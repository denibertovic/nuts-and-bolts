{ mkDerivation, aeson, base, bytestring, fast-logger, fetchgit
, hspec, HUnit, monad-logger, mtl, persistent
, persistent-postgresql, persistent-template, resourcet, scientific
, stdenv, text, time, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "persistent-postgresql-json";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/creichert/persistent-postgresql-json";
    sha256 = "1qx6qcl85qwdsg6kgirisfb4kxl1chrv3gyaap76d68wy9kwwi4d";
    rev = "d12f92ff954b8041fe2a0116c4ae3a5eda991222";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base bytestring mtl persistent persistent-template scientific
    text time transformers unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base fast-logger hspec HUnit monad-logger persistent
    persistent-postgresql persistent-template resourcet text
    transformers
  ];
  description = "JSON/JSONB helpers and types for persistent-postgresql";
  license = stdenv.lib.licenses.bsd3;
}
