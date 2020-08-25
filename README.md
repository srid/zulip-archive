# zulip-archive

Zulip archive viewer written in Haskell, using the [rib](https://github.com/srid/rib) static site generator.

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

### Zulip API Key

Get your API key for zulip ([instructions here](https://zulipchat.com/api/api-keys)). Note: you are looking for *your* API key, and not a bot's API key.

## Running

First, create a configuration file, adding your Zulip site settings (including the API key):

```
cp config/config.example.dhall config/config.dhall
vim config/config.dhall
```

To build and run the site:

```bash
# Or, use `bin/run` (useful for production)
nix-shell --run 'ghcid -T ":main serve" --restart=config'
```

Go to http://localhost:7004 to view your generated site.

## Manual update

User uploaded files are unavailable via the API. They must be manually exported (needs admin rights), and copied to `./static/user_uploads`.
