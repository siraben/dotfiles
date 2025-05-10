let
  sources = import ./nix/sources.nix;
  nixpkgs = sources.nixpkgs;
  pkgs = import nixpkgs {};
in

pkgs.mkShell rec {
  name = "home-manager-shell";

  buildInputs = with pkgs; [
    # niv
    # pkgs.home-manager
    (import sources.home-manager { inherit pkgs; }).home-manager
  ];

  shellHook = ''
    export NIX_PATH="nixpkgs=${nixpkgs}:home-manager=${sources."home-manager"}"
    export HOME_MANAGER_CONFIG="./home-manager/.config/nixpkgs/home.nix"
  '';
}
