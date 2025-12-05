# Emulator Workflow

Create, configure, and manage Android Virtual Devices (AVDs) for testing apps without physical devices.

## Quick Start

```bash
# List available system images
sdkmanager --list | grep system-images

# Install system image (API 34, x86_64, Google APIs)
sdkmanager "system-images;android-34;google_apis;x86_64"

# Create AVD
avdmanager create avd \
    -n Pixel7 \
    -k "system-images;android-34;google_apis;x86_64" \
    -d "pixel_7"

# List AVDs
avdmanager list avd

# Start emulator
emulator -avd Pixel7
```

## System Images

### Image Types

| Type | Google Apps | Play Store | Use Case |
|------|-------------|------------|----------|
| `default` | ❌ | ❌ | AOSP, minimal |
| `google_apis` | ✅ | ❌ | Google Maps, etc. |
| `google_apis_playstore` | ✅ | ✅ | Full Play Services |

### Common System Images

```bash
# Android 14 (API 34) - x86_64 (for Intel/AMD)
sdkmanager "system-images;android-34;google_apis;x86_64"

# Android 14 (API 34) - ARM64 (for Apple Silicon with Android Emulator for M1)
sdkmanager "system-images;android-34;google_apis;arm64-v8a"

# Android 13 (API 33)
sdkmanager "system-images;android-33;google_apis;x86_64"

# Android 12 (API 31)
sdkmanager "system-images;android-31;google_apis;x86_64"
```

**Choose architecture based on your CPU:**
- Intel/AMD processors: `x86_64`
- Apple Silicon (M1/M2/M3): `arm64-v8a` (requires Android Emulator for M1+)

## Create AVD

### Interactive (using avdmanager)

```bash
avdmanager create avd \
    -n <name> \
    -k <system-image> \
    -d <device-definition>
```

**Examples:**

```bash
# Pixel 7 with Android 14
avdmanager create avd \
    -n Pixel7 \
    -k "system-images;android-34;google_apis;x86_64" \
    -d "pixel_7"

# Pixel 6 with Android 13
avdmanager create avd \
    -n Pixel6 \
    -k "system-images;android-33;google_apis;x86_64" \
    -d "pixel_6"

# Tablet (Pixel Tablet)
avdmanager create avd \
    -n PixelTablet \
    -k "system-images;android-34;google_apis;x86_64" \
    -d "pixel_tablet"
```

### Custom Configuration

```bash
# Custom resolution and RAM
avdmanager create avd \
    -n CustomPhone \
    -k "system-images;android-34;google_apis;x86_64" \
    -d "pixel_7" \
    -c 512M \  # SD card size
    --force    # Overwrite if exists
```

### List Device Definitions

```bash
avdmanager list device
```

Common devices:
- `pixel_7`, `pixel_7_pro`
- `pixel_6`, `pixel_6_pro`
- `pixel_5`
- `pixel_tablet`

## Manage AVDs

### List AVDs

```bash
# List all AVDs
avdmanager list avd

# List with paths
avdmanager list avd -c
```

### Delete AVD

```bash
avdmanager delete avd -n <name>

# Example
avdmanager delete avd -n Pixel7
```

### Move AVD

AVDs are stored in `~/.android/avd/`. To move:

```bash
# Move AVD directory
mv ~/.android/avd/Pixel7.avd /new/location/

# Update .ini file
echo "path=/new/location/Pixel7.avd" > ~/.android/avd/Pixel7.ini
```

## Start Emulator

### Basic Start

```bash
# Start AVD
emulator -avd Pixel7

# Start in background
emulator -avd Pixel7 &

# Start with GPU acceleration
emulator -avd Pixel7 -gpu host
```

### Start Options

```bash
# Wipe data (factory reset)
emulator -avd Pixel7 -wipe-data

# Read-only system
emulator -avd Pixel7 -read-only

# Specific resolution
emulator -avd Pixel7 -skin 1080x1920

# Increase RAM
emulator -avd Pixel7 -memory 4096

# Cold boot (ignore snapshot)
emulator -avd Pixel7 -no-snapshot-load

# No audio
emulator -avd Pixel7 -no-audio
```

### Performance Options

```bash
# Maximum performance
emulator -avd Pixel7 \
    -gpu host \
    -memory 4096 \
    -cores 4 \
    -no-boot-anim \
    -no-snapshot

# Quick boot (use snapshot)
emulator -avd Pixel7 -no-snapshot-save
```

### Network Options

```bash
# Use specific DNS
emulator -avd Pixel7 -dns-server 8.8.8.8

# Network latency simulation
emulator -avd Pixel7 -netdelay gprs  # or edge, umts, none

# Network speed simulation
emulator -avd Pixel7 -netspeed full  # or gsm, hscsd, gprs, edge, umts, hsdpa

# Proxy
emulator -avd Pixel7 -http-proxy http://localhost:8888
```

## Emulator Control

### From Command Line

While emulator is running:

```bash
# Connect to emulator console
telnet localhost 5554  # Port is shown when emulator starts

# In telnet session:
# auth <token>  # Token is in ~/.emulator_console_auth_token
# help
# sms send 1234567890 "Test message"
# geo fix -122.084 37.422  # Set GPS location
# quit
```

### Using ADB

```bash
# List running emulators
adb devices

# Install app
adb -e install app-debug.apk  # -e targets emulator

# Rotate screen
adb shell settings put system accelerometer_rotation 0
adb shell settings put system user_rotation 1  # 0=0°, 1=90°, 2=180°, 3=270°

# Simulate low battery
adb shell dumpsys battery set level 10

# Reset battery
adb shell dumpsys battery reset
```

## Configuration Files

### config.ini

Located at `~/.android/avd/<name>.avd/config.ini`

**Common settings:**
```ini
# RAM
hw.ramSize=4096

# Storage
disk.dataPartition.size=8G

# GPU
hw.gpu.enabled=yes
hw.gpu.mode=host

# Camera
hw.camera.back=emulated
hw.camera.front=emulated

# Keyboard
hw.keyboard=yes

# Network
hw.net.speed=full
hw.net.delay=none
```

## Snapshots

### Quick Boot (Snapshots)

Emulator saves state for faster startup.

```bash
# Start without loading snapshot (cold boot)
emulator -avd Pixel7 -no-snapshot-load

# Start without saving snapshot (testing)
emulator -avd Pixel7 -no-snapshot-save

# Disable snapshots completely
emulator -avd Pixel7 -no-snapshot
```

### Manage Snapshots

```bash
# List snapshots
emulator -avd Pixel7 -list-snapshots

# Load specific snapshot
emulator -avd Pixel7 -snapshot <name>
```

## Troubleshooting

### Emulator won't start

```bash
# Check emulator location
which emulator

# Run with verbose logging
emulator -avd Pixel7 -verbose

# Check for errors
emulator -avd Pixel7 -debug init
```

### Slow emulator

**Enable hardware acceleration:**

**Linux (KVM):**
```bash
# Check KVM support
egrep -c '(vmx|svm)' /proc/cpuinfo  # Should be > 0

# Check KVM device
ls -l /dev/kvm

# Add user to kvm group
sudo usermod -a -G kvm $USER
# Log out and back in

# Verify
emulator -accel-check
```

**macOS (Hypervisor Framework):**
Automatically used on modern macOS.

**Windows (HAXM):**
Install Intel HAXM:
```bash
sdkmanager "extras;intel;Hardware_Accelerated_Execution_Manager"
```

### "PANIC: Cannot find AVD system path"

```bash
# Check ANDROID_AVD_HOME
echo $ANDROID_AVD_HOME

# Set if needed
export ANDROID_AVD_HOME=$HOME/.android/avd
```

### Graphics issues

```bash
# Try different GPU modes
emulator -avd Pixel7 -gpu auto
emulator -avd Pixel7 -gpu host
emulator -avd Pixel7 -gpu swiftshader_indirect
emulator -avd Pixel7 -gpu angle_indirect
```

## Multiple Emulators

### Run Multiple Instances

```bash
# Start first emulator
emulator -avd Pixel7 &

# Start second emulator (different AVD)
emulator -avd Pixel6 &

# Install on specific emulator
adb -s emulator-5554 install app-debug.apk
adb -s emulator-5556 install app-debug.apk
```

### Emulator Ports

- First emulator: console port 5554, adb port 5555
- Second emulator: console port 5556, adb port 5557
- And so on...

## Automation Scripts

### Start emulator and wait for boot

```bash
#!/usr/bin/env bash
set -euo pipefail

AVD_NAME="Pixel7"

echo "Starting emulator: $AVD_NAME"
emulator -avd "$AVD_NAME" -no-snapshot-save &

# Wait for device to boot
echo "Waiting for device to boot..."
adb wait-for-device

# Wait for boot animation to finish
while [ "$(adb shell getprop init.svc.bootanim | tr -d '\r')" = "running" ]; do
    sleep 1
done

echo "Emulator ready!"

# Optional: Install app
if [ -f "app-debug.apk" ]; then
    echo "Installing app..."
    adb install -r app-debug.apk
fi

echo "Done!"
```

### Create standard test emulator

```bash
#!/usr/bin/env bash
set -euo pipefail

AVD_NAME="TestDevice"
SYSTEM_IMAGE="system-images;android-34;google_apis;x86_64"

# Install system image if not already installed
sdkmanager "$SYSTEM_IMAGE"

# Create AVD (force overwrite if exists)
avdmanager create avd \
    -n "$AVD_NAME" \
    -k "$SYSTEM_IMAGE" \
    -d "pixel_7" \
    --force

echo "AVD '$AVD_NAME' created successfully"
echo "Start with: emulator -avd $AVD_NAME"
```

## Best Practices

1. **Use hardware acceleration**: Essential for acceptable performance
2. **Limit RAM**: Don't exceed 50% of your system RAM
3. **Close when not in use**: Emulators consume significant resources
4. **Use snapshots**: Enable quick boot for faster startup
5. **Match target devices**: Create AVDs matching your target devices
6. **Test on real devices**: Emulators don't perfectly match real hardware
7. **Use x86_64 images**: Faster than ARM on x86 computers (except Apple Silicon)

## Next Steps

- **Install and test app** - Use the **Build** and **Debug** workflows
- **Run automated tests** - Use the **Test** workflow

## Resources

- [AVD Manager Documentation](https://developer.android.com/studio/command-line/avdmanager)
- [Emulator Documentation](https://developer.android.com/studio/run/emulator)
- [Configure Hardware Acceleration](https://developer.android.com/studio/run/emulator-acceleration)
