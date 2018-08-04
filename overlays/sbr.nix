self: super:
{
	prm = import ../pkgs/prm {
	    inherit (super) stdenv lib buildGoPackage fetchgit;
	};
}