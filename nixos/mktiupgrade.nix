{ config, pkgs, ... }:

with pkgs;

let
  mktiupgrade = stdenv.mkDerivation {
    name = "mktiupgrade";
    src = fetchFromGitHub {
      owner = "KnightOS";
      repo = "mktiupgrade";
      rev = "2fee529";
      sha256 = "15wvhszsvac3di89rnpk1bray719rhfnaadjmwsl7b6xb9fi3y3w";
    };
    buildInputs = [ cmake asciidoc-full libxslt ];
    cmakeFlags = [ "-DCAMKE_BUILD_TYPE=Release" ];
    hardeningDisable = [ "format" ];
  };
in {
  environment.systemPackages = [ mktiupgrade ];
}
