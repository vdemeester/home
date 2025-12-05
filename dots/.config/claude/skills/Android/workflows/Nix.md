# Nix Workflow

Configure Android development on NixOS and with Nix package manager using nixpkgs androidenv.

## Why Nix for Android?

**Benefits:**
- Declarative configuration
- Reproducible builds
- Version pinning
- No manual SDK management
- Share configuration across machines

**Challenges:**
- More complex initial setup
- Not all Android SDK versions available
- May need to use imperative setup for some tools

## NixOS System Configuration

### System Packages

`systems/common/programs/android.nix`:
```nix
{ config, pkgs, ... }:

{
  programs.adb.enable = true;  # Enable adb system-wide
  users.users.youruser.extraGroups = [ "adbusers" ];  # Add user to adbusers

  environment.systemPackages = with pkgs; [
    android-tools  # adb, fastboot
  ];
}
```

### Home Manager Configuration

`home/common/dev/android.nix`:
```nix
{ config, pkgs, ... }:

let
  # Compose Android SDK with specific components
  androidSdk = pkgs.androidenv.composeAndroidPackages {
    platformVersions = [ "34" "33" ];
    buildToolsVersions = [ "34.0.0" "33.0.2" ];
    includeNDK = true;
    ndkVersion = "26.1.10909125";
    includeEmulator = true;
    includeSources = false;
    includeSystemImages = true;
    systemImageTypes = [ "google_apis" ];
    abiVersions = [ "x86_64" "arm64-v8a" ];
    cmakeVersions = [ "3.22.1" ];
  };

in
{
  home.packages = with pkgs; [
    # Android SDK (composed above)
    androidSdk.androidsdk

    # Development tools
    android-studio
    jdk17

    # Go and gomobile
    go
    # Note: gomobile needs to be installed via go install
  ];

  home.sessionVariables = {
    ANDROID_HOME = "${androidSdk.androidsdk}/libexec/android-sdk";
    ANDROID_SDK_ROOT = "${androidSdk.androidsdk}/libexec/android-sdk";
  };

  home.sessionPath = [
    "${androidSdk.androidsdk}/libexec/android-sdk/platform-tools"
    "${androidSdk.androidsdk}/libexec/android-sdk/tools"
    "${androidSdk.androidsdk}/libexec/android-sdk/emulator"
  ];
}
```

Apply configuration:
```bash
# For NixOS systems
sudo nixos-rebuild switch

# For home-manager
home-manager switch --flake .#youruser@hostname
```

## androidenv.composeAndroidPackages

### Available Options

```nix
pkgs.androidenv.composeAndroidPackages {
  # Platform versions (Android API levels)
  platformVersions = [ "34" "33" "31" ];

  # Build tools versions
  buildToolsVersions = [ "34.0.0" "33.0.2" ];

  # Include NDK
  includeNDK = true;
  ndkVersion = "26.1.10909125";  # Specific NDK version

  # Include Android Emulator
  includeEmulator = true;

  # Include system images for emulator
  includeSystemImages = true;
  systemImageTypes = [ "google_apis" "google_apis_playstore" "default" ];
  abiVersions = [ "x86_64" "arm64-v8a" "armeabi-v7a" ];

  # Include platform sources
  includeSources = false;

  # Include CMake (for C++ projects)
  cmakeVersions = [ "3.22.1" ];

  # Include extras
  includeExtras = [ "extras;google;gcm" ];
}
```

### Minimal Configuration

For basic Android development:

```nix
androidSdk = pkgs.androidenv.composeAndroidPackages {
  platformVersions = [ "34" ];
  buildToolsVersions = [ "34.0.0" ];
  includeNDK = false;
  includeEmulator = false;
};
```

### Full Configuration

For complete Android development with emulator:

```nix
androidSdk = pkgs.androidenv.composeAndroidPackages {
  platformVersions = [ "34" "33" "31" ];
  buildToolsVersions = [ "34.0.0" "33.0.2" ];
  includeNDK = true;
  ndkVersion = "26.1.10909125";
  includeEmulator = true;
  includeSystemImages = true;
  systemImageTypes = [ "google_apis" ];
  abiVersions = [ "x86_64" "arm64-v8a" ];
  includeSources = true;
  cmakeVersions = [ "3.22.1" ];
};
```

## Development Shell (nix develop)

Create a project-specific development shell.

`flake.nix`:
```nix
{
  description = "Android app with gomobile";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        androidSdk = pkgs.androidenv.composeAndroidPackages {
          platformVersions = [ "34" ];
          buildToolsVersions = [ "34.0.0" ];
          includeNDK = true;
          ndkVersion = "26.1.10909125";
          includeEmulator = true;
          includeSystemImages = true;
          systemImageTypes = [ "google_apis" ];
          abiVersions = [ "x86_64" ];
        };

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            androidSdk.androidsdk
            jdk17
            gradle
            go
          ];

          shellHook = ''
            export ANDROID_HOME="${androidSdk.androidsdk}/libexec/android-sdk"
            export ANDROID_SDK_ROOT="$ANDROID_HOME"
            export PATH="$ANDROID_HOME/platform-tools:$ANDROID_HOME/tools:$ANDROID_HOME/emulator:$PATH"

            # Install gomobile if not already installed
            if ! command -v gomobile &> /dev/null; then
              echo "Installing gomobile..."
              go install golang.org/x/mobile/cmd/gomobile@latest
              gomobile init
            fi

            echo "Android development environment ready!"
            echo "ANDROID_HOME: $ANDROID_HOME"
            echo "Java: $(java -version 2>&1 | head -n 1)"
            echo "Gradle: $(gradle --version | grep Gradle)"
            echo "Go: $(go version)"
          '';
        };
      }
    );
}
```

Enter development shell:
```bash
nix develop

# Or use direnv for automatic activation
echo "use flake" > .envrc
direnv allow
```

## Gomobile with Nix

### Install Gomobile

Gomobile isn't packaged in nixpkgs, so install via Go:

```bash
# In nix shell
go install golang.org/x/mobile/cmd/gomobile@latest
gomobile init
```

### Build Script

`build-aar.sh`:
```bash
#!/usr/bin/env nix-shell
#! nix-shell -i bash -p go android-tools

set -euo pipefail

# Install gomobile if needed
if ! command -v gomobile &> /dev/null; then
    go install golang.org/x/mobile/cmd/gomobile@latest
    gomobile init
fi

# Build AAR
cd golib
gomobile bind -target=android -o mylib.aar .
cp mylib.aar ../app/libs/

echo "AAR built and copied to app/libs/"
```

Make executable and run:
```bash
chmod +x build-aar.sh
./build-aar.sh
```

## Android Emulator with Nix

### Create AVD

```bash
# Enter nix shell with Android SDK
nix develop

# List available system images
ls $ANDROID_HOME/system-images/

# Create AVD
avdmanager create avd \
    -n NixPixel \
    -k "system-images;android-34;google_apis;x86_64" \
    -d "pixel_7"

# Start emulator
emulator -avd NixPixel
```

### Emulator Issues on NixOS

**Graphics acceleration:**

```nix
# System configuration for KVM support
virtualisation.libvirtd.enable = true;
users.users.youruser.extraGroups = [ "libvirtd" "kvm" ];

# Or use hardware.opengl
hardware.opengl.enable = true;
hardware.opengl.driSupport = true;
hardware.opengl.driSupport32Bit = true;
```

**If emulator fails to start:**

```bash
# Use software rendering
emulator -avd NixPixel -gpu swiftshader_indirect

# Or check emulator log
emulator -avd NixPixel -verbose
```

## Gradle with Nix

### Using Nix-provided Gradle

```nix
devShells.default = pkgs.mkShell {
  buildInputs = with pkgs; [
    gradle
    jdk17
  ];
};
```

### Using Gradle Wrapper (Recommended)

Gradle wrapper is more portable:

```bash
# Generate wrapper
gradle wrapper --gradle-version=8.6

# Use wrapper (works without nix shell)
./gradlew build
```

## Building Android App with Nix

### Simple Build

`default.nix`:
```nix
{ pkgs ? import <nixpkgs> {} }:

let
  androidSdk = pkgs.androidenv.composeAndroidPackages {
    platformVersions = [ "34" ];
    buildToolsVersions = [ "34.0.0" ];
    includeNDK = true;
    ndkVersion = "26.1.10909125";
  };

in
pkgs.stdenv.mkDerivation {
  pname = "myapp";
  version = "1.0";

  src = ./.;

  buildInputs = with pkgs; [
    androidSdk.androidsdk
    jdk17
    gradle
    go
  ];

  buildPhase = ''
    export ANDROID_HOME="${androidSdk.androidsdk}/libexec/android-sdk"
    export GRADLE_USER_HOME="$PWD/.gradle"

    # Build gomobile AAR
    cd golib
    ${pkgs.go}/bin/go install golang.org/x/mobile/cmd/gomobile@latest
    export PATH="$HOME/go/bin:$PATH"
    gomobile bind -target=android -o mylib.aar .
    cp mylib.aar ../app/libs/
    cd ..

    # Build APK
    gradle assembleDebug --no-daemon
  '';

  installPhase = ''
    mkdir -p $out
    cp app/build/outputs/apk/debug/app-debug.apk $out/
  '';
}
```

Build:
```bash
nix-build
# APK at: result/app-debug.apk
```

## Troubleshooting

### ANDROID_HOME not set

```bash
# Check variable
echo $ANDROID_HOME

# Should point to:
/nix/store/.../libexec/android-sdk

# If not set, add to shell:
export ANDROID_HOME="$(nix-build '<nixpkgs>' -A androidenv.composeAndroidPackages {...} --no-out-link)/libexec/android-sdk"
```

### adb permission denied

```bash
# On NixOS, enable adb
# In configuration.nix:
programs.adb.enable = true;
users.users.youruser.extraGroups = [ "adbusers" ];

# Rebuild
sudo nixos-rebuild switch

# Log out and back in
```

### Gradle can't find Android SDK

Create `local.properties`:
```bash
echo "sdk.dir=$ANDROID_HOME" > local.properties
```

### NDK not found by gomobile

```bash
# Check NDK location
ls $ANDROID_HOME/ndk/

# gomobile init should find it automatically
gomobile init
```

## Hybrid Approach (Recommended)

Use Nix for system tools, imperative for SDK:

**System (Nix):**
```nix
environment.systemPackages = with pkgs; [
  android-tools  # adb, fastboot
  jdk17
  gradle
  go
];
```

**SDK (Imperative):**
```bash
# Install SDK manually
mkdir -p ~/Android/Sdk
# Download cmdline-tools
# Use sdkmanager for packages
```

This provides flexibility while keeping tools reproducible.

## Integration with this Repository

In your home repository, add:

`home/common/dev/android.nix`:
```nix
{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    android-tools
    android-studio  # Optional
    jdk17
  ];

  # Add to shell init
  programs.zsh.initExtra = lib.mkAfter ''
    # Android SDK (if using manual installation)
    export ANDROID_HOME=$HOME/Android/Sdk
    export PATH=$ANDROID_HOME/platform-tools:$ANDROID_HOME/tools:$PATH
  '';
}
```

Import in your home configuration:
```nix
imports = [
  ./common/dev/android.nix
];
```

## Next Steps

- **Build Android app** - Use the **Build** workflow
- **Setup emulator** - Use the **Emulator** workflow
- **Integrate gomobile** - Use the **GomobileBind** workflow

## Resources

- [Nixpkgs Android Documentation](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/mobile/androidenv/README.md)
- [androidenv Reference](https://nixos.org/manual/nixpkgs/stable/#android)
- [Nix Android Examples](https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/mobile/androidenv/examples)
