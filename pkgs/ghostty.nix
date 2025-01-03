# A workaround until ghostty for macos lands in nixpkgs
# ...why do I brew install in 30+ LOC

{ stdenv, lib, fetchurl }:

stdenv.mkDerivation rec {
  pname = "ghostty";
  version = "1.0.1";

  src = fetchurl {
    url = "https://release.files.ghostty.org/${version}/Ghostty.dmg";
    hash = "sha256-QA9oy9EXLSFbzcRybKM8CxmBnUYhML82w48C+0gnRmM=";
  };

  sourceRoot = ".";
  preferLocalBuild = true;
  allowSubstitutes = false;


  # No build or install phases—just fetch the .dmg
  dontUnpack = true;

  installPhase = ''
    mkdir -p $out/Applications
    cp $src $out/Applications/Ghostty.dmg
  '';

  meta = with lib; {
    description = "Ghostty is a fast, feature-rich, and cross-platform terminal emulator";
    homepage = "https://ghostty.org";
    license = licenses.mit;
    platforms = platforms.darwin;
  };
}

# {
#   stdenv,
#   lib,
#   fetchurl,
# }:
# stdenv.mkDerivation rec {
#   pname = "ghostty";
#   version = "1.0.1";

#   src = fetchurl {
#     name = "${pname}-${version}.dmg";
#     url = "https://release.files.ghostty.org/${version}/Ghostty.dmg";
#     hash = "sha256-QA9oy9EXLSFbzcRybKM8CxmBnUYhML82w48C+0gnRmM=";
#   };

#   sourceRoot = ".";
#   preferLocalBuild = true;
#   allowSubstitutes = false;

#   buildCommand = ''
#     mkdir -p $out/Applications
#     MOUNT_POINT=$(mktemp -d)
#     /usr/bin/hdiutil attach $src -nobrowse -readonly -mountpoint $MOUNT_POINT
#     cp -pR $MOUNT_POINT/*.app $out/Applications/
#     /usr/bin/hdiutil detach $MOUNT_POINT -force
#     rm -rf $MOUNT_POINT
#   '';

#   meta = with lib; {
#     description = "Ghostty is a fast, feature-rich, and cross-platform terminal emulator that uses platform-native UI and GPU acceleration";
#     homepage = "https://ghostty.org";
#     license = licenses.mit;
#     mainProgram = "Ghostty.app";
#     platforms = platforms.darwin;
#   };
# }
