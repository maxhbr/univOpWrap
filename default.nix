{ mkDerivation, aeson, base, bytestring, directory, filepath
, hashable, MissingH, process, stdenv, text, time, vty, vty-ui
}:
mkDerivation {
  pname = "univOpWrap";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring directory filepath hashable MissingH process
    text time vty vty-ui
  ];
  description = "A universal wrapper, which makes searching and opening of files nicer";
  license = stdenv.lib.licenses.bsd3;
}
