function _nix_run_package
    set -l s $argv[1]
    set -l package (string split ":" $s)
    switch (count $package)
	case 1
	    _nix_run $s $s $argv[2] $argv[3]
	case 2
	    _nix_run $package[1] $package[2] $argv[2] $argv[3]
    end
end

function _nix_run
    set -l c $argv[1]
    set -l p $argv[2]
    set -l channel $argv[3]
    set -l channelsfile $argv[4]
    function $c --inherit-variable c --inherit-variable p --inherit-variable channel --inherit-variable channelsfile
	set -l cmd nix run
	if test -n "$channelsfile"
	    set cmd $cmd -f $channelsfile
	end
	eval $cmd $channel.$p -c $c $argv
    end
end
