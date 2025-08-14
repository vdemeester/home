{ lib }:
let
  isCurrentHost = hostname: n: n == hostname;
  hasVPNPublicKey = host: (lib.attrsets.attrByPath [ "net" "vpn" "pubkey" ] "" host) != "";
  hasVPNips = host: (builtins.length (lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ ] host)) > 0;
  
  /**
      Return true if the given host has a list of Syncthing folder configured.
    *
  */
  hasSyncthingFolders =
    host:
    builtins.hasAttr "syncthing" host
    && builtins.hasAttr "folders" host.syncthing
    && (builtins.length (lib.attrsets.attrValues host.syncthing.folders)) > 0;

  hasSSHHostKeys = host: builtins.hasAttr "ssh" host && builtins.hasAttr "hostKey" host.ssh;

  # Get the path for the given folder, either using the host specified path or the default one
  syncthingFolderPath =
    name: folder: folders:
    lib.attrsets.attrByPath [ "path" ] folders."${name}".path folder;

  # Filter machine with the given syncthing folder
  syncthingMachinesWithFolder =
    hostname: folderName: machines:
    lib.attrsets.filterAttrs (
      name: value:
      hasSyncthingFolders value
      && !(isCurrentHost hostname name)
      && (builtins.hasAttr folderName value.syncthing.folders)
    ) machines;

  generateSyncthingAdresses =
    machine:
    builtins.map (x: "tcp://${x}") (
      lib.attrsets.attrByPath [ "net" "ips" ] [ ] machine
      ++ lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ ] machine
      ++ lib.attrsets.attrByPath [ "net" "names" ] [ ] machine
    );

  sshHostIdentifier =
    machine:
    lib.attrsets.attrByPath [ "net" "names" ] [ ] machine
    ++ lib.attrsets.attrByPath [ "net" "ips" ] [ ] machine
    ++ lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ ] machine;

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

     Essentially, it will append /32 to the each element of the list.
  *
  */
  wg-ips = ips: builtins.map (x: "${x}/32") ips;

  # WIREGUARD
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

  # SYNCTHING
  generateSyncthingFolders =
    hostname: machine: machines: folders:
    lib.attrsets.mapAttrs' (
      name: value:
      lib.attrsets.nameValuePair (syncthingFolderPath name value folders) {
        inherit (folders."${name}") id;
        label = name;
        devices = lib.attrsets.mapAttrsToList (n: _v: n) (syncthingMachinesWithFolder hostname name machines);
        rescanIntervalS = 3600 * 6; # TODO: make it configurable
      }
    ) (lib.attrsets.attrByPath [ "syncthing" "folders" ] { } machine);

  generateSyncthingDevices =
    hostname: machines:
    lib.attrsets.mapAttrs
      (_name: value: {
        inherit (value.syncthing) id;
        addresses = generateSyncthingAdresses value;
      })
      (
        lib.attrsets.filterAttrs (name: value: hasSyncthingFolders value && !(isCurrentHost hostname name)) machines
      );

  syncthingGuiAddress =
    machine:
    (builtins.head (lib.attrsets.attrByPath [ "net" "vpn" "ips" ] [ "127.0.0.1" ] machine)) + ":8384";

  # SSH

  sshKnownHosts =
    machines:
    lib.strings.concatStringsSep "\n" (
      lib.attrsets.mapAttrsToList (
        _name: value: "${lib.strings.concatStringsSep "," (sshHostIdentifier value)} ${value.ssh.hostKey}"
      ) (lib.attrsets.filterAttrs (_name: hasSSHHostKeys) machines)
    );

  hostConfigs =
    machines: lib.attrsets.mergeAttrsList (lib.attrsets.mapAttrsToList (_name: hostConfig) machines);

  sshConfigs =
    machines:
    lib.attrsets.mergeAttrsList (
      lib.attrsets.mapAttrsToList (_name: sshConfig) (
        lib.attrsets.filterAttrs (_name: _value: true) machines
      )
    );
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
    ;
}