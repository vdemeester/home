# Debug Workflow

Debug Android applications using adb, logcat, and Android debugging tools.

## ADB (Android Debug Bridge)

### Basic Commands

```bash
# List connected devices
adb devices

# Connect to specific device (if multiple)
adb -s <device-id> <command>

# Check device info
adb shell getprop ro.product.model
adb shell getprop ro.build.version.release
```

### App Management

```bash
# Install APK
adb install app-debug.apk

# Reinstall (keep data)
adb install -r app-debug.apk

# Uninstall app
adb uninstall com.example.myapp

# Clear app data
adb shell pm clear com.example.myapp

# List installed packages
adb shell pm list packages
adb shell pm list packages | grep myapp

# Get app info
adb shell dumpsys package com.example.myapp
```

### Start/Stop App

```bash
# Start app activity
adb shell am start -n com.example.myapp/.MainActivity

# Start with intent data
adb shell am start -n com.example.myapp/.MainActivity -d "https://example.com"

# Stop (force close) app
adb shell am force-stop com.example.myapp

# Kill app process
adb shell am kill com.example.myapp
```

## Logcat

### View Logs

```bash
# All logs (verbose)
adb logcat

# Clear logs first, then follow
adb logcat -c && adb logcat

# Filter by app package
adb logcat | grep "com.example.myapp"

# Filter by tag
adb logcat -s MyTag

# Filter by priority (V=Verbose, D=Debug, I=Info, W=Warn, E=Error, F=Fatal)
adb logcat *:E  # Only errors

# Multiple filters
adb logcat MyTag:D *:E  # MyTag at Debug level, everything else at Error
```

### Log Priorities

```
V - Verbose (lowest priority)
D - Debug
I - Info
W - Warning
E - Error
F - Fatal
S - Silent (highest priority, nothing printed)
```

### Format Options

```bash
# Brief format (default)
adb logcat -v brief

# Time format (with timestamps)
adb logcat -v time

# Threadtime (time + PID + TID)
adb logcat -v threadtime

# Long format (all metadata)
adb logcat -v long
```

### Save Logs to File

```bash
# Save all logs
adb logcat > logs.txt

# Save with timestamp in filename
adb logcat > "logs-$(date +%Y%m%d-%H%M%S).txt"

# Save for 10 seconds then stop
timeout 10s adb logcat > logs.txt
```

### Filter Crash Logs

```bash
# Show only fatal errors
adb logcat *:F

# Show crashes and errors
adb logcat *:E

# Find specific crash
adb logcat | grep -A 50 "FATAL EXCEPTION"

# Show stack traces
adb logcat | grep -E "(at |Caused by)"
```

### Gomobile / Go-specific Logs

```bash
# Go runtime logs
adb logcat | grep GoLog

# Go panic messages
adb logcat | grep "panic:"

# Your Go log.Printf output
adb logcat | grep "GoLog"
```

## Debugging from Code

### Android (Kotlin/Java)

```kotlin
import android.util.Log

class MainActivity : AppCompatActivity() {
    companion object {
        private const val TAG = "MainActivity"
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        // Log levels
        Log.v(TAG, "Verbose message")
        Log.d(TAG, "Debug message")
        Log.i(TAG, "Info message")
        Log.w(TAG, "Warning message")
        Log.e(TAG, "Error message")

        // With exception
        try {
            // Code that might throw
        } catch (e: Exception) {
            Log.e(TAG, "Error occurred", e)
        }
    }
}
```

View in logcat:
```bash
adb logcat -s MainActivity
```

### Go Code (Gomobile)

```go
package mylib

import "log"

func FetchData(url string) (string, error) {
    log.Printf("Fetching URL: %s", url)  // Appears in logcat with GoLog tag

    // Your code
    result, err := doFetch(url)

    if err != nil {
        log.Printf("Error fetching: %v", err)
        return "", err
    }

    log.Printf("Fetch successful, got %d bytes", len(result))
    return result, nil
}
```

View in logcat:
```bash
adb logcat | grep GoLog
```

## Device/Emulator Commands

### File Operations

```bash
# Push file to device
adb push local-file.txt /sdcard/

# Pull file from device
adb pull /sdcard/file.txt ./

# List files
adb shell ls /sdcard/

# View file content
adb shell cat /sdcard/file.txt

# Remove file
adb shell rm /sdcard/file.txt
```

### Screenshots and Screen Recording

```bash
# Take screenshot
adb shell screencap /sdcard/screen.png
adb pull /sdcard/screen.png
adb shell rm /sdcard/screen.png

# Or in one command
adb exec-out screencap -p > screen.png

# Record screen (max 3 minutes)
adb shell screenrecord /sdcard/demo.mp4
# Press Ctrl+C to stop
adb pull /sdcard/demo.mp4
adb shell rm /sdcard/demo.mp4
```

### System Information

```bash
# CPU info
adb shell cat /proc/cpuinfo

# Memory info
adb shell cat /proc/meminfo

# Battery status
adb shell dumpsys battery

# Display info
adb shell dumpsys display

# Running processes
adb shell ps

# App processes
adb shell ps | grep com.example.myapp
```

### Network

```bash
# Check network connectivity
adb shell ping -c 4 google.com

# View network interfaces
adb shell ip addr

# Port forwarding (device port to local port)
adb forward tcp:8080 tcp:8080

# Reverse port forwarding (local port to device)
adb reverse tcp:9000 tcp:9000
```

## Performance Profiling

### CPU Profiling

```bash
# Start profiling
adb shell am profile start com.example.myapp /sdcard/profile.trace

# Stop profiling
adb shell am profile stop com.example.myapp

# Pull trace file
adb pull /sdcard/profile.trace

# Analyze with Android Studio or Perfetto
```

### Memory Profiling

```bash
# Get memory info for app
adb shell dumpsys meminfo com.example.myapp

# Heap dump
adb shell am dumpheap com.example.myapp /sdcard/heap.hprof
adb pull /sdcard/heap.hprof

# Analyze with Android Studio Memory Profiler
```

### Monitor App Performance

```bash
# CPU usage
adb shell top | grep com.example.myapp

# Memory usage over time
adb shell dumpsys meminfo com.example.myapp | grep TOTAL
```

## Debugging Crashes

### 1. View Crash Logs

```bash
adb logcat -c  # Clear old logs
# Reproduce crash
adb logcat *:E  # View errors
```

### 2. Find Stack Trace

Look for:
```
FATAL EXCEPTION: main
Process: com.example.myapp, PID: 12345
java.lang.NullPointerException: Attempt to invoke virtual method '...' on a null object reference
    at com.example.myapp.MainActivity.onCreate(MainActivity.kt:25)
    at android.app.Activity.performCreate(Activity.java:...)
```

### 3. Analyze Stack Trace

- **Line number**: `MainActivity.kt:25` - exact location
- **Exception type**: `NullPointerException` - what went wrong
- **Message**: Details about the error
- **Call stack**: How we got there

### 4. Common Crash Types

**NullPointerException**:
```kotlin
// Bad
val user = getUser()  // Returns null
val name = user.name  // Crash!

// Good
val user = getUser()
val name = user?.name ?: "Unknown"
```

**ClassCastException**:
```kotlin
// Bad
val text = view as TextView  // Crash if not TextView!

// Good
val text = view as? TextView
```

**ActivityNotFoundException**:
```kotlin
// Check if intent can be handled
if (intent.resolveActivity(packageManager) != null) {
    startActivity(intent)
}
```

## Interactive Debugging

### Enable Debug Mode in App

```kotlin
// app/build.gradle.kts
android {
    buildTypes {
        debug {
            isDebuggable = true
        }
    }
}
```

### Using Android Studio Debugger

1. Build debug APK: `./gradlew assembleDebug`
2. Install on device: `adb install app-debug.apk`
3. In Android Studio: Run → Attach Debugger to Android Process
4. Select your app
5. Set breakpoints in code
6. Trigger the code path

### Command-line Debugging (jdb)

```bash
# Find app process
adb shell ps | grep com.example.myapp

# Forward debug port
adb forward tcp:8000 jdwp:<pid>

# Attach jdb
jdb -attach localhost:8000
```

## Network Debugging

### HTTP Traffic Inspection

```bash
# Use adb reverse to connect app to local proxy
adb reverse tcp:8888 tcp:8888

# App should connect to localhost:8888
# Proxy (mitmproxy, Charles, Burp Suite) runs on port 8888
```

### View Network Stats

```bash
adb shell dumpsys netstats
```

## Common Issues

### "adb: device offline"

```bash
adb kill-server
adb start-server
# Reconnect device
```

### "adb: device unauthorized"

- Check device screen for authorization prompt
- Accept "Always allow from this computer"
- If no prompt:
  ```bash
  adb kill-server
  rm ~/.android/adbkey*
  adb start-server
  ```

### "Logcat shows nothing"

```bash
# Clear buffer
adb logcat -c

# Check buffer size
adb logcat -g

# Increase buffer size
adb logcat -G 16M
```

### App not showing in process list

```bash
# Check if app is running
adb shell "ps -A | grep com.example.myapp"

# Start app if not running
adb shell am start -n com.example.myapp/.MainActivity
```

## Debug Helpers

### Monitor script

Create `monitor-app.sh`:
```bash
#!/usr/bin/env bash
set -euo pipefail

APP_PACKAGE="com.example.myapp"
LOG_TAG="MyApp"

# Clear logs
adb logcat -c

# Monitor logs with color
adb logcat | grep --color=always -E "$APP_PACKAGE|$LOG_TAG|FATAL|ERROR"
```

### Install and monitor script

Create `debug-install.sh`:
```bash
#!/usr/bin/env bash
set -euo pipefail

APK="app/build/outputs/apk/debug/app-debug.apk"
PACKAGE="com.example.myapp"
ACTIVITY=".MainActivity"

echo "Installing $APK..."
adb install -r "$APK"

echo "Starting app..."
adb shell am start -n "$PACKAGE/$ACTIVITY"

echo "Monitoring logs (Ctrl+C to stop)..."
adb logcat -c
adb logcat | grep "$PACKAGE"
```

## Next Steps

- **Run tests** - Use the **Test** workflow
- **Profile performance** - Advanced profiling techniques
- **Publish app** - Use the **Publish** workflow

## Resources

- [ADB Documentation](https://developer.android.com/studio/command-line/adb)
- [Logcat Command-line Tool](https://developer.android.com/studio/command-line/logcat)
- [Debug Your App](https://developer.android.com/studio/debug)
