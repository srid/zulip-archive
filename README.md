# zulip-archive

...

## Prerequisites

First, install the [Nix package manager](https://nixos.org/nix/):

``` bash
bash <(curl https://nixos.org/nix/install)
```

Optionally, enable the [Nix cache](https://srid.cachix.org/) if you would like to speed up local builds:

``` bash
# If you do not already have cachix, install it:
nix-env -iA cachix -f https://cachix.org/api/v1/install
# Enable nix cache for rib
cachix use srid
```

## Running

To build and run the site:

```bash
nix-shell --run 'ghcid -T ":main YOURAPIKEY"'
```

This launches a web server at http://localhost:8080 serving the statically
generated content. Changing either `Main.hs` or the content in `./a` reloads everything.

