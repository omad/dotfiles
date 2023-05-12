#!/usr/bin/env bash
#
set -euo pipefail

pushd ~/.config/home-manager/

nix build .#homeManagerConfigurations.omad.activationPackage
./result/activate

popd
