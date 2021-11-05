{ stdenv, lib, fetchFromGitHub, pkg-config, ... }:

stdenv.mkDerivation rec {
  pname = "k380-function-keys-conf";
  version = "1.1";
  src = fetchFromGitHub {
    owner = "jergusg";
    repo = "k380-function-keys-conf";
    rev = "v${version}";
    sha256 = "I7Umq1ynKX91s6TFQ1+8o+tJw4Ql+WNvgafLLnsM0E0=";
  };

  nativeBuildInputs = [ pkg-config ];
  installFlags = [ "DESTDIR=$(out)" "PREFIX=" ];

  meta = with lib; {
    description =
      "Make function keys default on Logitech k380 bluetooth keyboard";
    homepage = "https://github.com/jergusg/k380-function-keys-conf";
    license = licenses.gpl3;
    maintainers = with maintainers; [ alternateved ];
    platforms = platforms.linux;
  };
}
