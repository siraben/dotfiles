{ pkgs }:

pkgs.haskellPackages.ghcWithHoogle (h: with h; [
  QuickCheck # property based testing
  vector # efficient arrays
  criterion # benchmarking
  aeson # JSON
  recursion-schemes # recursion schemes
  yaml
  effectful
  effectful-plugin
  linear-base
  heftia
])
