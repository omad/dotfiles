{ lib
, stdenv
, fetchFromGitHub
, cmake
, curl
}:

stdenv.mkDerivation rec {
  pname = "fastgron";
  version = "0.4.5";

  src = fetchFromGitHub {
    owner = "adamritter";
    repo = "fastgron";
    rev = "v${version}";
    hash = "sha256-dhw5PdnYgsEQUKt2w7Jdo80azHzTNsAvC9IOtL95PxA=";
  };

  nativeBuildInputs = [
    cmake
    curl
  ];

  meta = with lib; {
    description = "High-performance JSON to GRON (greppable, flattened JSON) converter";
    homepage = "https://github.com/adamritter/fastgron";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
