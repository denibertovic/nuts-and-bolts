{ mkDerivation, fetchFromGitHub, aeson, base, bytestring, cases, either, extra
, hpack, http-media, http-types, jose, lens, lens-aeson, lucid
, monad-logger, mtl, persistent, persistent-postgresql
, persistent-postgresql-json, persistent-template
, purescript-bridge, pwstore-fast, retry, servant, servant-auth
, servant-auth-server, servant-docs, servant-lucid, servant-server
, stdenv, text, time, transformers, unordered-containers, uuid, wai
, wai-extra, warp
}:
let
  gitignoreSrc = fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    # put the latest commit sha of gitignore Nix library here:
    rev = "f9e996052b5af4032fe6150bba4a6fe4f7b9d698";
    # use what nix suggests in the mismatch message here:
    sha256 = "sha256:0jrh5ghisaqdd0vldbywags20m2cxpkbbk5jjjmwaw0gr8nhsafv";
  };
  inherit (import gitignoreSrc {}) gitignoreSource;
in
mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = gitignoreSource ../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cases either http-media http-types jose lens
    lens-aeson lucid monad-logger mtl persistent persistent-postgresql
    persistent-postgresql-json persistent-template purescript-bridge
    pwstore-fast servant servant-auth servant-auth-server servant-docs
    servant-lucid servant-server text time transformers
    unordered-containers uuid wai wai-extra warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring cases either extra http-media http-types jose
    lens lens-aeson lucid monad-logger mtl persistent
    persistent-postgresql persistent-postgresql-json
    persistent-template purescript-bridge pwstore-fast retry servant
    servant-auth servant-auth-server servant-docs servant-lucid
    servant-server text time transformers unordered-containers uuid wai
    wai-extra warp
  ];
  testHaskellDepends = [
    aeson base bytestring cases either http-media http-types jose lens
    lens-aeson lucid monad-logger mtl persistent persistent-postgresql
    persistent-postgresql-json persistent-template purescript-bridge
    pwstore-fast servant servant-auth servant-auth-server servant-docs
    servant-lucid servant-server text time transformers
    unordered-containers uuid wai wai-extra warp
  ];
  doHaddock = false;
  enableSharedExecutables = false;
  enableLibraryProfiling = false;
  prePatch = "hpack";
  homepage = "https://github.com/denibertovic/nuts-and-bolts#readme";
  license = stdenv.lib.licenses.gpl3;
}
