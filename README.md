# 1 - nix.conf
Check etc/nix/nix.conf file contains Just:

```
build-users-group = nixbld
experimental-features = nix-command flakes ca-derivations
substituters = https://cache.nixos.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```
# 2 - nix develop
Run nix develop inside the cloned repo.
# 3 - Setup the right ghc and hls versions
run the followiong command inside the nix shell:
```
SETUP_ALL
```