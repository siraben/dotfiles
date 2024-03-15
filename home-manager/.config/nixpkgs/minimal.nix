args@{ config, lib, currentSystem, ... }:

import ./base.nix (args // { minimal = true; })
