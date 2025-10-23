{ inputs }:

final: prev:

{
  kitty = prev.kitty.overrideAttrs (old: {
    src = inputs.kitty-src;
  });
}
