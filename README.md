
# Table of Contents

1.  [Highlights](#orgdb11bda)
    1.  [`/systems`](#org44cd846)
        1.  [`/hosts`](#org5b75493)
        2.  [`/modules`](#org144fd2d)
        3.  [`/profiles`](#orge369937)
    2.  [`/home`](#org867ced9)
        1.  [`/hosts`](#org267bc9d)
        2.  [`/modules`](#orgdcff411)
        3.  [`/profiles`](#orga356201)
    3.  [`/nix`](#org702161f)
    4.  [`/tools`](#org7d9ca66)
    5.  [`/www`](#org241b931)
2.  [References](#org48c6cb4)
3.  [Licensing](#org98e56a9)

`home` is the monorepo containing my personal tools and infrastructure. Everything in here
should be built using [Nix](https://nixos.org/nix).


<a id="orgdb11bda"></a>

# Highlights

It is meant to be fully reproducible (using [niv](https://github.com/nmattia/niv) for now) and position-independent, meaning
there is no moving around of `configuration.nix`. For the configurations' entry points see
the individual [systems](systems), as well as [default.nix](default.nix).

This will be a all-time work-in-progress, so please beware that things might change
dramatically or even not working anymore 😛.


<a id="org44cd846"></a>

## `/systems`

This holds the configuration of my different systems. It is meant to be
position-independent, meaning there is no moving around of `configuration.nix`. For the
configurations' entry points see the individual [systems](systems), as well as [default.nix](default.nix).

/Note: to test `flakes` use `nix build
'.#nixosConfigurations.foo.config.system.build.toplevel'`, or even better, to test in a VM
use `nix build '.#nixosConfigurations.foo.config.system.build.vm'`./


<a id="org5b75493"></a>

### `/hosts`

This is the configuration for each hosts. It should be as simple as enabling some profiles
and some ad-hoc very specific configuration.


<a id="org144fd2d"></a>

### `/modules`

This is where all configuration that can be used across different system would lend. These
are not system dependent and should be configurable.


<a id="orge369937"></a>

### `/profiles`

This is where profiles enable/disable and configure modules (defined in NixOS or in
here). Examples are \`developement\`, …


<a id="org867ced9"></a>

## `/home`

Users configuration, for [NixOS](https://nixos.org) or not and using home-manager.


<a id="org267bc9d"></a>

### `/hosts`

This is the configuration for each hosts. It should be as simple as enabling some profiles
and some ad-hoc very specific configuration.


<a id="orgdcff411"></a>

### `/modules`

This is where all configuration that can be used across different system would lend. These
are not system dependent and should be configurable.


<a id="orga356201"></a>

### `/profiles`

This is where profiles enable/disable and configure modules (defined in NixOS or in
here). Examples are \`developement\`, …


<a id="org702161f"></a>

## `/nix`

Nix configurations, packages definitions, overlays.


<a id="org7d9ca66"></a>

## `/tools`

Those are tools I have written for my personal usage.


<a id="org241b931"></a>

## `/www`

Website sources and builders/deployers.


<a id="org48c6cb4"></a>

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


<a id="org98e56a9"></a>

# Licensing

Unless otherwise stated in a subdirectory, all code is licensed under the GNU GPL v3. See
[COPYING](COPYING) for details.
