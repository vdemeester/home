{ lib }:
let
  /**
    Check if the given name matches the current hostname.

    @param hostname The current hostname to compare against
    @param n The name to check
    @return true if n equals hostname, false otherwise
  */
  isCurrentHost = hostname: n: n == hostname;

  /**
    Check if a host has a VPN public key configured.

    @param host The host configuration to check
    @return true if host has a non-empty VPN public key, false otherwise
  */
  hasVPNPublicKey = host: (lib.attrsets.attrByPath [ "net" "vpn" "pubkey" ] "" host) != "";

  /**
    Check if a host has VPN IP addresses configured.

    @param host The host configuration to check
    @return true if host has at least one VPN IP address, false otherwise
  */
  hasVPNips = host: (builtins.length (lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ ] host)) > 0;

  /**
    Check if a host has network IP addresses configured.

    @param host The host configuration to check
    @return true if host has at least one VPN IP address, false otherwise
  */
  hasIps = host: (builtins.length (lib.attrsets.attrByPath [ "net" "ips" ] [ ] host)) > 0;

  /**
    Return true if the given host has a list of Syncthing folder configured.

    @param host The host configuration to check
    @return true if host has syncthing folders configured, false otherwise
  */
  hasSyncthingFolders =
    host:
    builtins.hasAttr "syncthing" host
    && builtins.hasAttr "folders" host.syncthing
    && (builtins.length (lib.attrsets.attrValues host.syncthing.folders)) > 0;

  /**
    Check if a host has SSH host keys configured.

    @param host The host configuration to check
    @return true if host has SSH host keys, false otherwise
  */
  hasSSHHostKeys = host: builtins.hasAttr "ssh" host && builtins.hasAttr "hostKey" host.ssh;

  /**
    Get the path for the given folder, either using the host specified path or the default one.

    @param name The folder name
    @param folder The folder configuration
    @param folders The complete folders configuration
    @return The path for the folder
  */
  syncthingFolderPath =
    name: folder: folders:
    lib.attrsets.attrByPath [ "path" ] folders."${name}".path folder;

  /**
    Filter machines with the given syncthing folder.

    @param hostname The current hostname to exclude from results
    @param folderName The folder name to filter by
    @param machines The set of all machines
    @return Filtered set of machines that have the specified folder and are not the current host
  */
  syncthingMachinesWithFolder =
    hostname: folderName: machines:
    lib.attrsets.filterAttrs (
      name: value:
      hasSyncthingFolders value
      && !(isCurrentHost hostname name)
      && (builtins.hasAttr folderName value.syncthing.folders)
    ) machines;

  /**
    Generate Syncthing addresses for a machine from its network configuration.

    @param machine The machine configuration
    @return List of TCP addresses (ips, vpn ips, and names) prefixed with "tcp://"
  */
  generateSyncthingAdresses =
    machine:
    builtins.map (x: "tcp://${x}") (
      lib.attrsets.attrByPath [ "net" "ips" ] [ ] machine
      ++ lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ ] machine
      ++ lib.attrsets.attrByPath [ "net" "names" ] [ ] machine
    );

  /**
    Get SSH host identifiers for a machine (names, IPs, and VPN IPs).

    @param machine The machine configuration
    @return List of all network identifiers for the machine
  */
  sshHostIdentifier =
    machine:
    lib.attrsets.attrByPath [ "net" "names" ] [ ] machine
    ++ lib.attrsets.attrByPath [ "net" "ips" ] [ ] machine
    ++ lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ ] machine;

  /**
    Generate host configuration mapping IPs to appropriate hostnames.

    @param machine The machine configuration
    @return Attribute set mapping IP addresses to corresponding hostnames
  */
  hostConfig =
    machine:
    builtins.listToAttrs (
      map
        (x: {
          name = x;
          value =
            if (lib.strings.hasPrefix "10.100" x) then
              builtins.filter (n: lib.strings.hasSuffix ".vpn" n) machine.net.names
            else if (lib.strings.hasPrefix "192.168" x) then
              builtins.filter (n: lib.strings.hasSuffix ".home" n) machine.net.names
            else
              [ ];
        })
        (
          lib.attrsets.attrByPath [ "net" "ips" ] [ ] machine
          ++ lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ ] machine
        )
    );

  /**
    Generate SSH configuration for a machine.

    @param machine The machine configuration
    @return Attribute set of SSH host configurations with hostnames, identity settings, etc.
  */
  sshConfig =
    machine:
    builtins.listToAttrs (
      map
        (x: {
          name = x;
          value = {
            hostname =
              if (lib.strings.hasSuffix ".vpn" x) then
                builtins.head machine.net.vpn.ips
              else if (lib.strings.hasSuffix ".home" x) then
                builtins.head machine.net.ips
              else
                x;
            forwardAgent = true;
            identityFile = "~/.ssh/kyushu";
            identityAgent = "empty";
          };
        })
        (
          builtins.filter (x: (lib.strings.hasSuffix ".home" x) || (lib.strings.hasSuffix ".vpn" x)) (
            sshHostIdentifier machine
          )
        )
    );

  /**
    Return a list of wireguard ips from a list of ips.

    Essentially, it will append /32 to each element of the list.

    @param ips List of IP addresses
    @return List of IP addresses with /32 suffix for wireguard configuration
  */
  wg-ips = ips: builtins.map (x: "${x}/32") ips;

  /**
    Generate Wireguard peer configurations from a set of machines.

    @param machines The set of all machines
    @return List of wireguard peer configurations with allowedIPs and publicKey
  */
  generateWireguardPeers =
    machines:
    lib.attrsets.attrValues (
      lib.attrsets.mapAttrs
        (_name: value: {
          allowedIPs = value.net.vpn.ips;
          publicKey = value.net.vpn.pubkey;
        })
        (
          lib.attrsets.filterAttrs (
            name: value: name != "kerkouane" && (hasVPNPublicKey value) && (hasVPNips value)
          ) machines
        )
    );

  /**
    Generate Syncthing folder configurations for the current machine.

    @param hostname The current hostname
    @param machine The current machine configuration
    @param machines The set of all machines
    @param folders The folder definitions
    @return Attribute set of syncthing folder configurations
  */
  generateSyncthingFolders =
    hostname: machine: machines: folders:
    lib.attrsets.mapAttrs' (
      name: value:
      lib.attrsets.nameValuePair (syncthingFolderPath name value folders) {
        inherit (folders."${name}") id;
        label = name;
        devices = lib.attrsets.mapAttrsToList (n: _v: n) (
          syncthingMachinesWithFolder hostname name machines
        );
        rescanIntervalS = 3600 * 6; # TODO: make it configurable
      }
    ) (lib.attrsets.attrByPath [ "syncthing" "folders" ] { } machine);

  /**
    Generate Syncthing device configurations for all machines except the current one.

    @param hostname The current hostname to exclude
    @param machines The set of all machines
    @return Attribute set of syncthing device configurations with IDs and addresses
  */
  generateSyncthingDevices =
    hostname: machines:
    lib.attrsets.mapAttrs
      (_name: value: {
        inherit (value.syncthing) id;
        addresses = generateSyncthingAdresses value;
      })
      (
        lib.attrsets.filterAttrs (
          name: value: hasSyncthingFolders value && !(isCurrentHost hostname name)
        ) machines
      );

  /**
    Generate Syncthing GUI address for a machine.

    @param machine The machine configuration
    @return String in format "IP:8384" for accessing Syncthing GUI
  */
  syncthingGuiAddress =
    machine:
    (builtins.head (lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ "127.0.0.1" ] machine)) + ":8384";

  /**
    Generate SSH known_hosts entries for all machines with SSH host keys.

    @param machines The set of all machines
    @return String containing SSH known_hosts entries
  */
  sshKnownHosts =
    machines:
    lib.strings.concatStringsSep "\n" (
      lib.attrsets.mapAttrsToList (
        _name: value: "${lib.strings.concatStringsSep "," (sshHostIdentifier value)} ${value.ssh.hostKey}"
      ) (lib.attrsets.filterAttrs (_name: hasSSHHostKeys) machines)
    );

  /**
    Merge host configurations from all machines.

    @param machines The set of all machines
    @return Merged attribute set of all host configurations
  */
  hostConfigs =
    machines: lib.attrsets.mergeAttrsList (lib.attrsets.mapAttrsToList (_name: hostConfig) machines);

  /**
    Generate and merge SSH configurations from all machines.

    @param machines The set of all machines
    @return Merged attribute set of all SSH configurations
  */
  sshConfigs =
    machines:
    lib.attrsets.mergeAttrsList (
      lib.attrsets.mapAttrsToList (_name: sshConfig) (
        lib.attrsets.filterAttrs (_name: _value: true) machines
      )
    );

  /**
    Create service defaults for media/homelab services.

    Common pattern for services that run as a specific user/group with firewall access.

    @param user The user to run the service as (default: "vincent")
    @param group The group to run the service as (default: "users")
    @param openFirewall Whether to open firewall for the service (default: true)
    @return Attribute set with user, group, and openFirewall settings
  */
  mkServiceDefaults =
    {
      user ? "vincent",
      group ? "users",
      openFirewall ? true,
    }:
    {
      inherit user group openFirewall;
    };

  /**
    Create a Samba share configuration with common defaults.

    Standard configuration for public, writable shares with guest access.

    @param name The name of the share
    @param path The filesystem path to share
    @param user The user for force user/group (default: "vincent")
    @param group The group for force user/group (default: "users")
    @param readOnly Make the share read-only (default: false)
    @return Attribute set with complete Samba share configuration
  */
  mkSambaShare =
    {
      name,
      path,
      user ? "vincent",
      group ? "users",
      readOnly ? false,
    }:
    {
      inherit path;
      public = "yes";
      browseable = "yes";
      "read only" = if readOnly then "yes" else "no";
      "guest ok" = "yes";
      writable = if readOnly then "no" else "yes";
      comment = if readOnly then "${name} (read-only)" else name;
      "create mask" = "0644";
      "directory mask" = "0755";
      "force user" = user;
      "force group" = group;
    };
in
{
  inherit
    syncthingFolderPath
    hasSyncthingFolders
    syncthingMachinesWithFolder
    generateSyncthingAdresses
    isCurrentHost
    hasVPNPublicKey
    hasVPNips
    hasIps
    hasSSHHostKeys
    sshHostIdentifier
    sshConfig
    hostConfig
    wg-ips
    generateWireguardPeers
    generateSyncthingFolders
    generateSyncthingDevices
    syncthingGuiAddress
    sshKnownHosts
    hostConfigs
    sshConfigs
    mkServiceDefaults
    mkSambaShare
    ;
}
