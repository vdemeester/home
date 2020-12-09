
# Table of Contents

1.  [Highlights](#orge5f20f1)
    1.  [`/systems`](#org25a7672)
        1.  [`/hosts`](#orgbb234d3)
        2.  [`/modules`](#org8f64aa6)
        3.  [`/profiles`](#org46c952b)
    2.  [`/home`](#orgf0bb072)
        1.  [`/hosts`](#org65fb6a1)
        2.  [`/modules`](#org2316502)
        3.  [`/profiles`](#org48814c0)
    3.  [`/nix`](#orgaf0439d)
    4.  [`/tools`](#org6af22d2)
    5.  [`/www`](#org1942cc5)
2.  [References](#orgc09a499)
3.  [Licensing](#orgfbd9526)

`home` is the monorepo containing my personal tools and infrastructure. Everything in here
should be built using [Nix](https://nixos.org/nix).


<a id="orge5f20f1"></a>

# Highlights

It is meant to be fully reproducible (using [niv](https://github.com/nmattia/niv) for now) and position-independent, meaning
there is no moving around of `configuration.nix`. For the configurations' entry points see
the individual [systems](systems), as well as [default.nix](default.nix).

This will be a all-time work-in-progress, so please beware that things might change
dramatically or even not working anymore 😛.


<a id="org25a7672"></a>

## `/systems`

This holds the configuration of my different systems. It is meant to be
position-independent, meaning there is no moving around of `configuration.nix`. For the
configurations' entry points see the individual [systems](systems), as well as [default.nix](default.nix).


<a id="orgbb234d3"></a>

### `/hosts`

This is the configuration for each hosts. It should be as simple as enabling some profiles
and some ad-hoc very specific configuration.


<a id="org8f64aa6"></a>

### `/modules`

This is where all configuration that can be used across different system would lend. These
are not system dependent and should be configurable.


<a id="org46c952b"></a>

### `/profiles`

This is where profiles enable/disable and configure modules (defined in NixOS or in
here). Examples are \`developement\`, …


<a id="orgf0bb072"></a>

## `/home`

Users configuration, for [NixOS](https://nixos.org) or not and using home-manager.


<a id="org65fb6a1"></a>

### `/hosts`

This is the configuration for each hosts. It should be as simple as enabling some profiles
and some ad-hoc very specific configuration.


<a id="org2316502"></a>

### `/modules`

This is where all configuration that can be used across different system would lend. These
are not system dependent and should be configurable.


<a id="org48814c0"></a>

### `/profiles`

This is where profiles enable/disable and configure modules (defined in NixOS or in
here). Examples are \`developement\`, …


<a id="orgaf0439d"></a>

## `/nix`

Nix configurations, packages definitions, overlays.


<a id="org6af22d2"></a>

## `/tools`

Those are tools I have written for my personal usage.


<a id="org1942cc5"></a>

## `/www`

Website sources and builders/deployers.


<a id="orgc09a499"></a>

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


<a id="orgfbd9526"></a>

# Licensing

Unless otherwise stated in a subdirectory, all code is licensed under the GNU GPL v3. See
[COPYING](COPYING) for details.
