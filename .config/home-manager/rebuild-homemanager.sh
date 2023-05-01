#!/usr/bin/env bash
#
set -euo pipefail

nix build .#homeManagerConfigurations.omad.activationPackage
./result/activate
