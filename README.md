
# Table of Contents

1.  [Highlights](#org3ff3224)
    1.  [`/systems`](#org4c17c9f)
        1.  [`/hosts`](#org64dcde7)
        2.  [`/modules`](#orgc9efe73)
    2.  [`/users`](#orgf386674)
        1.  [`/modules`](#org454203f)
        2.  [`/{users}/default.nix`](#orgb8cd752)
        3.  [`/{users}/home.nix`](#org80163eb)
    3.  [`/nix`](#org6d3a3e6)
    4.  [`/tools`](#org07d0006)
    5.  [`/www`](#orgf60030d)
2.  [References](#orgaa3bf31)
3.  [Licensing](#org4c1cafd)

`home` is the monorepo containing my personal tools and infrastructure. Everything in here
should be built using [Nix](https://nixos.org/nix).


<a id="org3ff3224"></a>

# Highlights

It is meant to be fully reproducible (using [niv](https://github.com/nmattia/niv) for now) and position-independent, meaning
there is no moving around of `configuration.nix`. For the configurations' entry points see
the individual [systems](systems), as well as [default.nix](default.nix).

This will be a all-time work-in-progress, so please beware that things might change
dramatically or even not working anymore 😛.


<a id="org4c17c9f"></a>

## `/systems`

This holds the configuration of my different systems. It is meant to be
position-independent, meaning there is no moving around of `configuration.nix`. For the
configurations' entry points see the individual [systems](systems), as well as [default.nix](default.nix).

*Note: to test `flakes` use `nix build '.#nixosConfigurations.foo.config.system.build.toplevel'`, or even better, to test in a VM use `nix build '.#nixosConfigurations.foo.config.system.build.vm'`.*


<a id="org64dcde7"></a>

### `/hosts`

This is the configuration for each hosts. It should be as simple as enabling some profiles
and some ad-hoc very specific configuration.


<a id="orgc9efe73"></a>

### `/modules`

This is where all configuration that can be used across different system would lend. These
are not system dependent and should be configurable. This contains the following
"modules", a bit like how nixpkgs is organized

-   hardware
-   profiles
-   programs
-   services
-   virtualisation


<a id="orgf386674"></a>

## `/users`

Users configuration, for [NixOS](https://nixos.org) or not and using home-manager.


<a id="org454203f"></a>

### `/modules`

This is a list of modules that can be used by all users.


<a id="orgb8cd752"></a>

### `/{users}/default.nix`

Contains NixOS user specific configuration. It gets imported if the user is listed in a
given configuration. It will contains the user creation information (groups, …) and can
refer to a given home-manager configuration.


<a id="org80163eb"></a>

### `/{users}/home.nix`

Contains home-manager configuration.
*Note*: In order to get information from Nixos, use `nixosConfig`. This should be useful to
configure some modules depending on the host configuration.


<a id="org6d3a3e6"></a>

## `/nix`

Nix configurations, packages definitions, overlays.


<a id="org07d0006"></a>

## `/tools`

Those are tools I have written for my personal usage.


<a id="orgf60030d"></a>

## `/www`

Website sources and builders/deployers.


<a id="orgaa3bf31"></a>

# References

Repositories

-   [https://github.com/lovesegfault/nix-config](https://github.com/lovesegfault/nix-config)
-   <https://github.com/utdemir/dotfiles>
-   <https://github.com/davidtwco/veritas>
-   [https://github.com/bqv/nixrc](https://github.com/bqv/nixrc)
-   [https://github.com/berbiche/dotfiles](https://github.com/berbiche/dotfiles)
-   <https://github.com/hlissner/dotfiles/>
-   <https://github.com/leotaku/nixos-config>
-   <https://github.com/rasendubi/dotfiles>
-   [https://github.com/akirak/nix-desktop](https://github.com/akirak/nix-desktop)
-   <https://git.tazj.in/about/>
-   <https://github.com/danieldk/nix-home>

Old

-   <https://gitlab.com/samueldr/nixos-configuration>
-   [https://github.com/yurrriq/dotfiles](https://github.com/yurrriq/dotfiles)
-   <https://github.com/akirak/nixos-config>
-   <https://github.com/akirak/home.nix>
-   <https://github.com/cstrahan/nixos-config>
-   <https://github.com/jwiegley/nix-config>
-   <https://github.com/arianvp/nixos-stuff>
-   <https://github.com/romatthe/ronix>
-   <https://github.com/rummik/nixos-config>
-   <https://github.com/a-schaefers/nix-config.old>
-   <https://github.com/auntieNeo/nixrc>
    -   <https://github.com/glines/nixrc>
-   <https://github.com/therealpxc/pxc.nix.d>
-   <https://github.com/tycho01/nix-config>
-   <https://github.com/ghuntley/dotfiles-nixos>
-   <https://github.com/budevg/nix-conf>
-   <https://github.com/cleverca22/nixos-configs>
-   <https://github.com/coreyoconnor/nix_configs>
-   <https://github.com/dejanr/dotfiles>
-   <https://github.com/Ericson2314/nixos-configuration>
-   <https://gitlab.com/garry-cairns/nixos-config>
-   <https://github.com/grahamc/nixos-config>
-   <https://github.com/HugoReeves/nix-home>
-   <https://github.com/kampfschlaefer/nixconfig>
-   <https://github.com/lambdael/nixosconf>
-   <https://github.com/puffnfresh/nix-files>
-   <https://github.com/talyz/nixos-config>
-   <https://github.com/uwap/nixos-configs>
-   <https://github.com/yacinehmito/yarn-nix>
-   <https://github.com/yrashk/nix-home>
-   <https://github.com/pSub/configs>
-   <https://github.com/periklis/nix-config>
-   <https://github.com/peel/dotfiles>
-   <https://github.com/bennofs/etc-nixos>
-   <https://github.com/Baughn/machine-config>
-   <https://github.com/gvolpe/nix-config>


<a id="org4c1cafd"></a>

# Licensing

Unless otherwise stated in a subdirectory, all code is licensed under the GNU GPL v3. See
[COPYING](COPYING) for details.
