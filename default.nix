{ mkDerivation, base, binary, bytestring, haskell-src-meta, mtl
, network, parsec, regex-compat, regex-posix, stdenv
, template-haskell, time, utf8-string
}:
mkDerivation {
  pname = "templatepg";
  version = "0.2.8";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring haskell-src-meta mtl network parsec
    regex-compat regex-posix template-haskell time utf8-string
  ];
  homepage = "https://github.com/jekor/templatepg";
  description = "A PostgreSQL access library with compile-time SQL type inference";
  license = stdenv.lib.licenses.mit;
}
