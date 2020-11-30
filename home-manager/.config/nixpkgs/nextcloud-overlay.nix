# Overlay to fix nextcloud-client on wayland
self: super:
{
  nextcloud-client = super.nextcloud-client.overrideAttrs (old: {
    version = "3.0.2";
    src = super.fetchFromGitHub {
      owner = "nextcloud";
      repo = "desktop";
      rev = "v3.0.2";
      sha256 = "0qzriiaj6sjdkgh61iihvpsvhi0kvfwjz1vvjnwdlfdx2s4xmv24";
    };
  });
}
