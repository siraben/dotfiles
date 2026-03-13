args@{ config, lib, currentSystem, ... }:

import ./base.nix (args // { profile = "minimal"; })
