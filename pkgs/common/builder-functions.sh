#!/usr/bin/env bash
# Common builder functions for Nix packages

# Standard setup for Nix builders
nixpkgs_builder_setup() {
    source $stdenv/setup
}

# Copy specific binaries from source to output bin directory
copy_binaries_to_bin() {
    local source_dir="$1"
    shift
    local binaries=("$@")
    
    mkdir -p $out/bin
    for binary in "${binaries[@]}"; do
        cp "$source_dir/$binary" "$out/bin/"
    done
}

# Copy all binaries from a source bin directory to output bin directory
copy_all_binaries() {
    local source_bin_dir="$1"
    
    mkdir -p $out/bin
    cp "$source_bin_dir"/* "$out/bin/"
}

# Install shell completions for multiple commands
install_shell_completions() {
    local base_dir="$1"
    shift
    local -A completions=()
    
    # Parse arguments in pairs: command completion_file
    while [[ $# -gt 1 ]]; do
        local cmd="$1"
        local comp_file="$2"
        completions["$cmd"]="$comp_file"
        shift 2
    done
    
    for cmd in "${!completions[@]}"; do
        local comp_file="${completions[$cmd]}"
        installShellCompletion --cmd "$cmd" --zsh "$base_dir/$comp_file"
    done
}