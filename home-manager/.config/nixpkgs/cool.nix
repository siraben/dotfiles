{ lib, stdenv, fetchurl }:

stdenv.mkDerivation rec {
  pname = "cool";
  version = "unstable-2022";

  src = fetchurl {
    url = "https://kjl.name/cs3276/cool/osx-x86/cool-osx";
    sha256 = "sha256-mQ9hGTRw3lT+m/uAGANhf3ky2jMQog4BoRRejG1kpPk=";
  };

  dontBuild = true;
  dontUnpack = true;

  installPhase= ''
    runHook preInstall
    mkdir -p $out/bin
    cp $src $out/bin/cool-osx
    chmod +x $out/bin/cool-osx
    runHook postInstall
  '';

  meta = with lib; {
    description = "Reference compiler for the Cool programming language";
    license = licenses.free;
    maintainers = with maintainers; [ siraben ];
    platforms = platforms.darwin;
  };
}
