# GomobileBind Workflow

Build Go libraries as Android AAR (Android Archive) packages using gomobile bind. This allows you to use Go code in Android apps written in Java or Kotlin.

## When to Use

- You have existing Go code (networking, crypto, business logic)
- You want to integrate Go into an existing Android app
- You need Go's concurrency or standard library in Android
- You want to share code between backend and mobile

## Prerequisites

- Go installed (`go version`)
- Android SDK and NDK installed
- `ANDROID_HOME` environment variable set
- Write access to the Go package you want to bind

## Steps

### 1. Verify Environment

Check that required tools are available:

```bash
# Check Go
go version

# Check Android SDK
echo $ANDROID_HOME
ls $ANDROID_HOME/ndk

# Check PATH includes Android tools
which adb
```

If Android SDK is not set up, invoke the **Setup** workflow first.

### 2. Install Gomobile

```bash
# Install gomobile command
go install golang.org/x/mobile/cmd/gomobile@latest

# Initialize gomobile (downloads Android toolchain)
gomobile init

# Verify installation
gomobile version
```

**Note**: `gomobile init` downloads NDK toolchain and can take several minutes on first run.

### 3. Prepare Go Package

Your Go package must follow gomobile binding rules:

**Good Go code for binding:**
```go
// File: golib/network.go
package network

import (
    "fmt"
    "net/http"
)

// FetchURL fetches content from a URL
// Exported function will be available in Android
func FetchURL(url string) (string, error) {
    resp, err := http.Get(url)
    if err != nil {
        return "", err
    }
    defer resp.Body.Close()

    // Implementation...
    return "content", nil
}

// Config represents network configuration
// Exported struct becomes Android class
type Config struct {
    Timeout int
    BaseURL string
}

// NewConfig creates a new Config
func NewConfig(timeout int, baseURL string) *Config {
    return &Config{
        Timeout: timeout,
        BaseURL: baseURL,
    }
}

// GetTimeout returns the timeout value
func (c *Config) GetTimeout() int {
    return c.Timeout
}

// Callback interface for async operations
type Callback interface {
    OnSuccess(data string)
    OnError(err error)
}

// FetchAsync fetches URL asynchronously
func FetchAsync(url string, callback Callback) {
    go func() {
        result, err := FetchURL(url)
        if err != nil {
            callback.OnError(err)
            return
        }
        callback.OnSuccess(result)
    }()
}
```

**What works in gomobile:**
- ✅ Exported functions with basic types
- ✅ Exported structs with exported fields
- ✅ Methods on exported structs
- ✅ Interfaces (for callbacks)
- ✅ Error return values (become exceptions)
- ✅ Slices of basic types (`[]byte`, `[]int`, `[]string`)

**What doesn't work:**
- ❌ Maps (use structs instead)
- ❌ Channels (use callbacks via interfaces)
- ❌ Generics (Go 1.18+ type parameters)
- ❌ Unexported types in function signatures
- ❌ Variadic functions
- ❌ Complex nested types

### 4. Create go.mod

```bash
cd golib
go mod init example.com/mylib
go mod tidy
```

### 5. Build AAR

**Basic build (all architectures):**
```bash
gomobile bind -target=android -o mylib.aar ./golib
```

**Build specific architectures (smaller AAR):**
```bash
# ARM64 only (most modern devices)
gomobile bind -target=android/arm64 -o mylib.aar ./golib

# ARM64 and ARM32
gomobile bind -target=android/arm64,android/arm -o mylib.aar ./golib

# All common architectures
gomobile bind -target=android/arm64,android/arm,android/amd64,android/386 -o mylib.aar ./golib
```

**Build with javadoc comments:**
```bash
gomobile bind -target=android -javapkg=com.example.mylib -o mylib.aar ./golib
```

**Architecture options:**
- `android/arm64`: 64-bit ARM (arm64-v8a) - modern devices
- `android/arm`: 32-bit ARM (armeabi-v7a) - older devices
- `android/amd64`: 64-bit x86 (x86_64) - emulators
- `android/386`: 32-bit x86 (x86) - old emulators

### 6. Integrate AAR into Android Project

**Copy AAR to Android project:**
```bash
cp mylib.aar /path/to/android-project/app/libs/
```

**Add dependency in `app/build.gradle.kts`:**
```kotlin
dependencies {
    implementation(files("libs/mylib.aar"))

    // Other dependencies...
}
```

**Sync Gradle:**
```bash
cd /path/to/android-project
./gradlew sync
```

### 7. Use in Android Code

**Kotlin example:**
```kotlin
package com.example.myapp

import android.os.Bundle
import androidx.appcompat.app.AppCompatActivity
import golib.Network  // Import generated package

class MainActivity : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        // Call Go function
        try {
            val result = Network.fetchURL("https://example.com")
            println("Result: $result")
        } catch (e: Exception) {
            println("Error: ${e.message}")
        }

        // Use Go struct
        val config = Network.newConfig(30, "https://api.example.com")
        println("Timeout: ${config.timeout}")

        // Async with callback
        Network.fetchAsync("https://example.com", object : Network.Callback {
            override fun onSuccess(data: String) {
                println("Success: $data")
            }

            override fun onError(err: Exception) {
                println("Error: ${err.message}")
            }
        })
    }
}
```

**Java example:**
```java
package com.example.myapp;

import golib.Network;

public class MainActivity extends AppCompatActivity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        try {
            String result = Network.fetchURL("https://example.com");
            System.out.println("Result: " + result);
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }

        Network.Config config = Network.newConfig(30, "https://api.example.com");
        System.out.println("Timeout: " + config.getTimeout());
    }
}
```

## Type Conversion Reference

| Go Type | Java/Kotlin Type | Notes |
|---------|------------------|-------|
| `bool` | `boolean` / `Boolean` | |
| `int`, `int32` | `int` / `Int` | 32-bit signed |
| `int64` | `long` / `Long` | 64-bit signed |
| `float32` | `float` / `Float` | |
| `float64` | `double` / `Double` | |
| `string` | `String` | UTF-8 encoded |
| `[]byte` | `byte[]` / `ByteArray` | |
| `[]int` | `long[]` / `LongArray` | Note: int becomes long[] |
| `error` | `Exception` | Go errors become Java exceptions |
| `struct` | `class` | Exported fields become getters/setters |
| `interface` | `interface` | Must have exported methods only |

## Optimization Tips

### 1. Minimize Boundary Crossings

Each call from Java/Kotlin to Go has overhead. Batch operations when possible:

**Bad (many crossings):**
```go
func ProcessItem(item string) string { ... }

// Called 1000 times from Android
for item in items {
    ProcessItem(item)  // 1000 Go calls
}
```

**Good (single crossing):**
```go
func ProcessItems(items []string) []string { ... }

// Called once from Android
ProcessItems(items)  // 1 Go call
```

### 2. Use Appropriate Architectures

**For development (faster builds):**
```bash
gomobile bind -target=android/arm64 -o mylib.aar ./golib
```

**For release (wider compatibility):**
```bash
gomobile bind -target=android/arm64,android/arm -o mylib.aar ./golib
```

### 3. Keep Go API Simple

- Flat function signatures (avoid nested types)
- Use basic types when possible
- Return errors as second value
- Document expected behavior

## Common Issues

### Issue: "gomobile: command not found"

**Solution:**
```bash
# Ensure Go bin is in PATH
export PATH=$PATH:$(go env GOPATH)/bin

# Reinstall gomobile
go install golang.org/x/mobile/cmd/gomobile@latest
```

### Issue: "NDK not found"

**Solution:**
```bash
# Set ANDROID_HOME
export ANDROID_HOME=$HOME/Android/Sdk

# Install NDK via sdkmanager
sdkmanager --install "ndk;26.1.10909125"

# Reinitialize gomobile
gomobile init
```

### Issue: "Cannot use map in function signature"

**Problem:**
```go
func BadFunc(data map[string]string) error { ... }  // Won't bind!
```

**Solution:** Use struct instead:
```go
type KeyValue struct {
    Key   string
    Value string
}

func GoodFunc(data []KeyValue) error { ... }  // Works!
```

### Issue: AAR is very large

**Problem:** All architectures included by default

**Solution:** Build only needed architectures:
```bash
# Just ARM64 (most devices)
gomobile bind -target=android/arm64 -o mylib.aar ./golib

# ARM64 + ARM (smaller than all 4)
gomobile bind -target=android/arm64,android/arm -o mylib.aar ./golib
```

## Testing Go Code

Test Go code separately before binding:

```bash
cd golib
go test ./...
go test -v ./...
go test -cover ./...
```

## Automation Script

Create `build-aar.sh` for consistent builds:

```bash
#!/usr/bin/env bash
set -euo pipefail

# Build AAR for Android
echo "Building AAR..."

# Set targets (can be overridden)
TARGETS=${ANDROID_TARGETS:-"android/arm64,android/arm"}
OUTPUT=${AAR_OUTPUT:-"mylib.aar"}
PACKAGE=${GO_PACKAGE:-"./golib"}

gomobile bind \
    -target="$TARGETS" \
    -o "$OUTPUT" \
    "$PACKAGE"

echo "AAR built successfully: $OUTPUT"

# Copy to Android project if path is set
if [ -n "${ANDROID_PROJECT:-}" ]; then
    cp "$OUTPUT" "$ANDROID_PROJECT/app/libs/"
    echo "Copied to Android project"
fi
```

Usage:
```bash
chmod +x build-aar.sh

# Build for ARM64 only
ANDROID_TARGETS=android/arm64 ./build-aar.sh

# Build and copy to Android project
ANDROID_PROJECT=/path/to/android-app ./build-aar.sh
```

## Debugging Tips

### 1. Check Generated Java Code

Extract AAR to inspect generated Java:

```bash
unzip mylib.aar -d mylib-extracted
cd mylib-extracted
javap -p classes.jar  # View Java classes
```

### 2. Add Logging in Go

Use Android's logging system from Go:

```go
package mylib

import "log"

func FetchURL(url string) (string, error) {
    log.Printf("Fetching URL: %s", url)  // Will appear in logcat
    // ...
}
```

View in logcat:
```bash
adb logcat | grep GoLog
```

### 3. Test Type Conversions

Create simple test functions to verify type conversion:

```go
func TestString(s string) string { return s }
func TestInt(i int) int { return i }
func TestBytes(b []byte) []byte { return b }
```

## Next Steps

After successfully binding your Go library:

1. **Build the Android app** - Use the **Build** workflow
2. **Test on device/emulator** - Use the **Debug** workflow
3. **Optimize AAR size** - Build only needed architectures
4. **Add tests** - Use the **Test** workflow
5. **Publish app** - Use the **Publish** workflow

## Resources

- [Gomobile Documentation](https://pkg.go.dev/golang.org/x/mobile/cmd/gomobile)
- [Gomobile Wiki](https://github.com/golang/go/wiki/Mobile)
- [Gomobile Examples](https://github.com/golang/mobile/tree/master/example)
- [Go on Mobile Tutorial](https://github.com/golang/mobile/blob/master/README.md)
