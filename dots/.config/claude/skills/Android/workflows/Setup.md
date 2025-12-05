# Setup Workflow

Configure Android development environment including SDK, NDK, command-line tools, and necessary environment variables.

## Installation Options

### Option 1: Android Studio (Recommended for beginners)

**Pros**: GUI, automatic SDK management, emulator, IDE
**Cons**: Large download (~1GB), more resources

**Steps:**
1. Download [Android Studio](https://developer.android.com/studio)
2. Install and launch
3. Follow setup wizard to install SDK and tools
4. SDK installed at: `~/Android/Sdk` (Linux/Mac) or `C:\Users\<user>\AppData\Local\Android\Sdk` (Windows)

### Option 2: Command-line Tools (Recommended for this setup)

**Pros**: Lightweight, scriptable, no GUI
**Cons**: Manual configuration, no IDE

**Steps:**

#### 1. Download Command-line Tools

```bash
# Create SDK directory
mkdir -p ~/Android/Sdk
cd ~/Android/Sdk

# Download latest cmdline-tools (Linux)
wget https://dl.google.com/android/repository/commandlinetools-linux-11076708_latest.zip

# Or for Mac
# wget https://dl.google.com/android/repository/commandlinetools-mac-11076708_latest.zip

# Extract
unzip commandlinetools-linux-*_latest.zip
rm commandlinetools-linux-*_latest.zip

# Move to correct location
mkdir -p cmdline-tools/latest
mv cmdline-tools/* cmdline-tools/latest/ 2>/dev/null || true
```

#### 2. Set Environment Variables

Add to `~/.bashrc` or `~/.zshrc`:

```bash
# Android SDK
export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/cmdline-tools/latest/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools
export PATH=$PATH:$ANDROID_HOME/emulator
```

Apply changes:
```bash
source ~/.bashrc  # or ~/.zshrc
```

#### 3. Accept Licenses

```bash
sdkmanager --licenses
```

Type `y` and press Enter for each license.

#### 4. Install SDK Components

```bash
# Platform tools (adb, fastboot)
sdkmanager "platform-tools"

# Latest Android platform (API 34 as of 2025)
sdkmanager "platforms;android-34"

# Build tools
sdkmanager "build-tools;34.0.0"

# NDK (required for gomobile)
sdkmanager "ndk;26.1.10909125"

# Emulator (optional)
sdkmanager "emulator"

# System image for emulator (optional)
sdkmanager "system-images;android-34;google_apis;x86_64"
```

#### 5. Verify Installation

```bash
# Check SDK manager
sdkmanager --list

# Check adb
adb version

# Check environment
echo $ANDROID_HOME
ls $ANDROID_HOME
```

### Option 3: NixOS / Nix Package Manager

See the **Nix** workflow for NixOS-specific setup.

## Post-Installation

### Install Gomobile

```bash
go install golang.org/x/mobile/cmd/gomobile@latest
gomobile init
```

This downloads additional toolchains for cross-compilation.

### Configure Gradle (if using Android projects)

Create `~/.gradle/gradle.properties`:

```properties
# Use more memory for Gradle
org.gradle.jvmargs=-Xmx4096m

# Enable Gradle daemon
org.gradle.daemon=true

# Enable parallel execution
org.gradle.parallel=true

# Enable build cache
org.gradle.caching=true

# Use AndroidX
android.useAndroidX=true
android.enableJetifier=true
```

### Setup Device for Development

#### Enable Developer Options on Android Device

1. Go to **Settings** → **About phone**
2. Tap **Build number** 7 times
3. Go back to **Settings** → **System** → **Developer options**
4. Enable **USB debugging**

#### Connect Device

```bash
# Connect via USB
adb devices

# Should show:
# List of devices attached
# 1234567890ABCDEF    device

# If shows "unauthorized", check phone for authorization prompt
```

#### Connect via WiFi (optional)

```bash
# First connect via USB
adb tcpip 5555

# Find device IP (Settings → About → Status → IP address)
# Then connect wirelessly
adb connect 192.168.1.100:5555

# Disconnect USB cable
# Verify wireless connection
adb devices
```

## Directory Structure

After setup, your SDK should look like:

```
~/Android/Sdk/
├── build-tools/
│   └── 34.0.0/
│       ├── aapt
│       ├── aapt2
│       ├── apksigner
│       └── ...
├── cmdline-tools/
│   └── latest/
│       └── bin/
│           ├── sdkmanager
│           ├── avdmanager
│           └── ...
├── emulator/
│   ├── emulator
│   └── ...
├── ndk/
│   └── 26.1.10909125/
├── platform-tools/
│   ├── adb
│   ├── fastboot
│   └── ...
├── platforms/
│   └── android-34/
└── system-images/
    └── android-34/
```

## Troubleshooting

### sdkmanager: command not found

```bash
# Check ANDROID_HOME
echo $ANDROID_HOME

# Check PATH
echo $PATH | grep Android

# Reload shell configuration
source ~/.bashrc
```

### adb: command not found

```bash
# Install platform-tools
sdkmanager "platform-tools"

# Add to PATH
export PATH=$PATH:$ANDROID_HOME/platform-tools
```

### gomobile init fails

```bash
# Make sure NDK is installed
sdkmanager "ndk;26.1.10909125"

# Check ANDROID_HOME
echo $ANDROID_HOME

# Retry
gomobile init -v
```

### Device not detected

```bash
# Check USB debugging is enabled
adb devices

# Restart adb
adb kill-server
adb start-server

# Check USB connection/cable
# Try different USB port
```

## Updating Components

### Update SDK components

```bash
# List installed and available packages
sdkmanager --list

# Update all installed packages
sdkmanager --update

# Install specific version
sdkmanager "platforms;android-35"
sdkmanager "build-tools;35.0.0"
```

### Update Gomobile

```bash
go install golang.org/x/mobile/cmd/gomobile@latest
gomobile init
```

## Next Steps

After successful setup:

- **Build Android app** - Use the **Build** workflow
- **Create emulator** - Use the **Emulator** workflow
- **Build Go library** - Use the **GomobileBind** workflow
- **Debug app** - Use the **Debug** workflow

## Resources

- [Android Command-line Tools](https://developer.android.com/studio/command-line)
- [sdkmanager Reference](https://developer.android.com/studio/command-line/sdkmanager)
- [ADB Documentation](https://developer.android.com/studio/command-line/adb)
- [Gomobile Installation](https://pkg.go.dev/golang.org/x/mobile/cmd/gomobile)
