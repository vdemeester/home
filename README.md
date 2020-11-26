
# Table of Contents

1.  [Highlights](#org955d5f4)
    1.  [Systems](#orgb1330e3)
    2.  [Tools](#orgb798790)
    3.  [User(s)](#org075b62a)
2.  [References](#orgdf5348e)
3.  [Licensing](#orge77bd17)

`home` is the monorepo containing my personal tools and infrastructure. Everything in here
should be built using [Nix](https://nixos.org/nix).


<a id="org955d5f4"></a>

# Highlights

It is meant to be fully reproducible (using [niv](https://github.com/nmattia/niv) for now) and position-independent, meaning
there is no moving around of `configuration.nix`. For the configurations' entry points see
the individual [systems](systems), as well as [default.nix](default.nix).

This will be a all-time work-in-progress, so please beware that things might change
dramatically or even not working anymore 😛.


<a id="orgb1330e3"></a>

## Systems

This holds the configuration of my different systems. It is meant to be
position-independent, meaning there is no moving around of `configuration.nix`. For the
configurations' entry points see the individual [systems](systems), as well as [default.nix](default.nix).


<a id="orgb798790"></a>

## Tools

Those are tools I have written for my personal usage.


<a id="org075b62a"></a>

## User(s)

Users configuration, for [NixOS](https://nixos.org) and using home-manager.


<a id="orgdf5348e"></a>

# References

Repositories

-   [https://github.com/lovesegfault/nix-config](https://github.com/lovesegfault/nix-config)
-   <https://github.com/utdemir/dotfiles>
-   <https://github.com/davidtwco/veritas>
-   <https://gitlab.com/samueldr/nixos-configuration>
-   <https://github.com/rasendubi/dotfiles>
-   [https://github.com/yurrriq/dotfiles](https://github.com/yurrriq/dotfiles)
-   <https://github.com/akirak/nixos-config>
-   <https://github.com/akirak/home.nix>
-   <https://github.com/cstrahan/nixos-config>
-   <https://github.com/jwiegley/nix-config>
-   <https://github.com/arianvp/nixos-stuff>
-   <https://github.com/leotaku/nixos-config>
-   <https://github.com/romatthe/ronix>
-   <https://github.com/rummik/nixos-config>
-   <https://git.tazj.in/about/>
-   <https://github.com/a-schaefers/nix-config.old>
-   <https://github.com/auntieNeo/nixrc>
    -   <https://github.com/glines/nixrc>
-   <https://github.com/therealpxc/pxc.nix.d>
-   <https://github.com/tycho01/nix-config>
-   <https://github.com/ghuntley/dotfiles-nixos>
-   <https://github.com/budevg/nix-conf>
-   <https://github.com/cleverca22/nixos-configs>
-   <https://github.com/coreyoconnor/nix_configs>
-   <https://github.com/danieldk/nix-home>
-   <https://github.com/dejanr/dotfiles>
-   <https://github.com/Ericson2314/nixos-configuration>
-   <https://gitlab.com/garry-cairns/nixos-config>
-   <https://github.com/grahamc/nixos-config>
-   <https://github.com/HugoReeves/nix-home>
-   <https://github.com/jwiegley/nix-config>
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

-   <https://github.com/hlissner/dotfiles/>


<a id="orge77bd17"></a>

# Licensing

Unless otherwise stated in a subdirectory, all code is licensed under the GNU GPL v3. See
[COPYING](COPYING) for details.
