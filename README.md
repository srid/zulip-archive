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

You may store it an environment variable for later access:

```bash
export ZULIPAPIKEY="<your api key here>"
```

## Running

To build and run the site:

```bash
rm b  # b is a symlink, remove if you are not the author 
nix-shell --run 'ghcid -T ":main $ZULIPAPIKEY"'
```

HTML files will have been generated under `./b`.

## Manual update

User uploaded files are unavailable via the API. They must be manually exported (needs admin rights), and copied to `./a/user_uploads`.
