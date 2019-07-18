#!/bin/bash

set -xe


pushd $TMPDIR
http_code="$(curl -z ~/bin/bat -O https://github.com/sharkdp/bat/releases/download/v0.11.0/bat-v0.11.0-x86_64-unknown-linux-gnu.tar.gz --silent --location --write-out %{http_code})"

if [[ "$http_code" == "200" ]]; then
  # code here to process index.html because 200 means it gets updated
  tar xzf bat-v*
  cp bat*/bat ~/bin/bat
  cp bat*/bat.1 ~/share/man/man1/
fi
popd
