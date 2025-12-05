# GomobileApp Workflow

Build standalone Android applications entirely in Go using `gomobile build`. This creates a complete APK with Go code as the main application logic.

## When to Use

- You want to write the entire app in Go
- You're building a simple utility or demo app
- You want to prototype quickly without Java/Kotlin
- You're comfortable with limited UI capabilities
- You want to share code between CLI and mobile versions

## When NOT to Use

- You need complex native Android UI (use GomobileBind + Kotlin instead)
- You need full access to Android framework APIs
- You're building a production app with rich UI
- You need Material Design components

## Prerequisites

- Go installed (`go version`)
- Android SDK and NDK installed
- `ANDROID_HOME` environment variable set
- Gomobile installed (`gomobile version`)

## Steps

### 1. Install and Initialize Gomobile

```bash
# Install gomobile
go install golang.org/x/mobile/cmd/gomobile@latest

# Initialize (downloads Android toolchain)
gomobile init

# Verify
gomobile version
```

### 2. Create Go Mobile App

**Project structure:**
```
myapp/
├── main.go
├── go.mod
└── assets/         # Optional: app resources
    └── icon.png
```

**Basic app (`main.go`):**
```go
package main

import (
    "log"

    "golang.org/x/mobile/app"
    "golang.org/x/mobile/event/lifecycle"
    "golang.org/x/mobile/event/paint"
    "golang.org/x/mobile/event/size"
    "golang.org/x/mobile/event/touch"
    "golang.org/x/mobile/gl"
)

func main() {
    app.Main(func(a app.App) {
        var glctx gl.Context
        var sz size.Event

        for e := range a.Events() {
            switch e := a.Filter(e).(type) {
            case lifecycle.Event:
                switch e.Crosses(lifecycle.StageVisible) {
                case lifecycle.CrossOn:
                    glctx, _ = e.DrawContext.(gl.Context)
                    onStart(glctx)
                case lifecycle.CrossOff:
                    onStop(glctx)
                    glctx = nil
                }

            case size.Event:
                sz = e

            case paint.Event:
                if glctx == nil || e.External {
                    continue
                }
                onPaint(glctx, sz)
                a.Publish()
                a.Send(paint.Event{}) // Keep animating

            case touch.Event:
                // Handle touch events
                log.Printf("Touch: %v", e)
            }
        }
    })
}

func onStart(glctx gl.Context) {
    log.Println("App started")
    // Initialize OpenGL resources
}

func onStop(glctx gl.Context) {
    log.Println("App stopped")
    // Clean up resources
}

func onPaint(glctx gl.Context, sz size.Event) {
    // Clear screen
    glctx.ClearColor(0.2, 0.2, 0.3, 1.0)
    glctx.Clear(gl.COLOR_BUFFER_BIT)

    // Draw your content here
}
```

**Simple text-based app (no OpenGL):**
```go
package main

import (
    "fmt"
    "log"

    "golang.org/x/mobile/app"
    "golang.org/x/mobile/event/lifecycle"
    "golang.org/x/mobile/event/touch"
)

func main() {
    app.Main(func(a app.App) {
        for e := range a.Events() {
            switch e := a.Filter(e).(type) {
            case lifecycle.Event:
                switch e.Crosses(lifecycle.StageAlive) {
                case lifecycle.CrossOn:
                    log.Println("App is alive")
                    // Do work here
                    processData()
                case lifecycle.CrossOff:
                    log.Println("App is closing")
                }

            case touch.Event:
                if e.Type == touch.TypeBegin {
                    log.Printf("Touch at: %v, %v", e.X, e.Y)
                }
            }
        }
    })
}

func processData() {
    // Your app logic
    fmt.Println("Processing...")
}
```

**Practical example: Network utility**
```go
package main

import (
    "fmt"
    "io"
    "log"
    "net/http"
    "time"

    "golang.org/x/mobile/app"
    "golang.org/x/mobile/event/lifecycle"
)

func main() {
    app.Main(func(a app.App) {
        for e := range a.Events() {
            switch e := a.Filter(e).(type) {
            case lifecycle.Event:
                if e.Crosses(lifecycle.StageAlive) == lifecycle.CrossOn {
                    go runNetworkCheck()
                }
            }
        }
    })
}

func runNetworkCheck() {
    ticker := time.NewTicker(5 * time.Second)
    defer ticker.Stop()

    for range ticker.C {
        resp, err := http.Get("https://www.google.com")
        if err != nil {
            log.Printf("Network check failed: %v", err)
            continue
        }
        io.Copy(io.Discard, resp.Body)
        resp.Body.Close()

        log.Printf("Network OK: %s", resp.Status)
    }
}
```

### 3. Create go.mod

```bash
go mod init example.com/myapp
go get golang.org/x/mobile/app
go get golang.org/x/mobile/event/lifecycle
go mod tidy
```

### 4. Build APK

**Basic build:**
```bash
gomobile build -target=android .
```

This creates `myapp.apk` in the current directory.

**Build with custom app ID:**
```bash
gomobile build -target=android -appid=com.example.myapp .
```

**Build for specific architectures:**
```bash
# ARM64 only (smaller APK)
gomobile build -target=android/arm64 -appid=com.example.myapp .

# ARM64 and ARM32
gomobile build -target=android/arm64,android/arm -appid=com.example.myapp .
```

**Build with custom icon:**
```bash
# Place icon at assets/icon.png
gomobile build -target=android -icon=assets/icon.png .
```

**Build with version:**
```bash
gomobile build \
    -target=android \
    -appid=com.example.myapp \
    -ldflags="-X main.version=1.0.0" \
    .
```

### 5. Install and Run

**Install on connected device:**
```bash
# Build and install
gomobile install -target=android -appid=com.example.myapp .

# Or build then install separately
gomobile build -target=android -appid=com.example.myapp .
adb install -r myapp.apk
```

**View logs:**
```bash
adb logcat | grep GoLog
```

### 6. Test the App

**List installed apps:**
```bash
adb shell pm list packages | grep myapp
```

**Launch app:**
```bash
adb shell am start -n com.example.myapp/.MainActivity
```

**Stop app:**
```bash
adb shell am force-stop com.example.myapp
```

**Uninstall:**
```bash
adb uninstall com.example.myapp
```

## App Package Structure

The gomobile app package provides:

### Event Types

```go
import "golang.org/x/mobile/event"

// Lifecycle events
lifecycle.Event    // App start, stop, pause, resume

// Touch events
touch.Event        // Touch screen interactions
touch.TypeBegin    // Finger down
touch.TypeMove     // Finger dragging
touch.TypeEnd      // Finger up

// Paint events
paint.Event        // Redraw requests

// Size events
size.Event         // Screen size, orientation changes

// Key events
key.Event          // Keyboard input
```

### GL Context

```go
import "golang.org/x/mobile/gl"

// OpenGL ES 2.0 context
glctx gl.Context

// Common operations
glctx.ClearColor(r, g, b, a float32)
glctx.Clear(gl.COLOR_BUFFER_BIT)
glctx.Enable(gl.BLEND)
```

## Advanced Examples

### HTTP Server App

```go
package main

import (
    "fmt"
    "log"
    "net/http"

    "golang.org/x/mobile/app"
    "golang.org/x/mobile/event/lifecycle"
)

func main() {
    app.Main(func(a app.App) {
        var server *http.Server

        for e := range a.Events() {
            switch e := a.Filter(e).(type) {
            case lifecycle.Event:
                switch e.Crosses(lifecycle.StageAlive) {
                case lifecycle.CrossOn:
                    server = startServer()
                case lifecycle.CrossOff:
                    if server != nil {
                        server.Close()
                    }
                }
            }
        }
    })
}

func startServer() *http.Server {
    mux := http.NewServeMux()
    mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        fmt.Fprintf(w, "Hello from Go on Android!")
    })

    server := &http.Server{
        Addr:    ":8080",
        Handler: mux,
    }

    go func() {
        log.Println("Starting server on :8080")
        if err := server.ListenAndServe(); err != http.ErrServerClosed {
            log.Printf("Server error: %v", err)
        }
    }()

    return server
}
```

### File Operations

```go
package main

import (
    "log"
    "os"
    "path/filepath"

    "golang.org/x/mobile/app"
    "golang.org/x/mobile/event/lifecycle"
)

func main() {
    app.Main(func(a app.App) {
        for e := range a.Events() {
            switch e := a.Filter(e).(type) {
            case lifecycle.Event:
                if e.Crosses(lifecycle.StageAlive) == lifecycle.CrossOn {
                    workWithFiles()
                }
            }
        }
    })
}

func workWithFiles() {
    // Get app data directory
    dataDir, err := os.UserConfigDir()
    if err != nil {
        log.Printf("Error getting data dir: %v", err)
        return
    }

    // Create file
    filePath := filepath.Join(dataDir, "mydata.txt")
    err = os.WriteFile(filePath, []byte("Hello, Android!"), 0644)
    if err != nil {
        log.Printf("Error writing file: %v", err)
        return
    }

    // Read file
    data, err := os.ReadFile(filePath)
    if err != nil {
        log.Printf("Error reading file: %v", err)
        return
    }

    log.Printf("File content: %s", string(data))
}
```

## Customization

### AndroidManifest.xml

Gomobile generates AndroidManifest.xml automatically. To customize:

1. **Build APK first:**
   ```bash
   gomobile build -target=android -appid=com.example.myapp .
   ```

2. **Extract APK:**
   ```bash
   unzip myapp.apk -d myapp-extracted
   ```

3. **View manifest:**
   ```bash
   cat myapp-extracted/AndroidManifest.xml
   ```

4. **For advanced customization**, use gomobile bind instead and create a full Android project.

### Permissions

Gomobile apps have minimal permissions by default. For additional permissions, you'll need to:

1. Use gomobile bind to create AAR
2. Create full Android project
3. Add permissions in AndroidManifest.xml

## Limitations

### UI Limitations

- ❌ No native Android UI widgets (Button, TextView, etc.)
- ❌ No Material Design components
- ❌ Limited text rendering capabilities
- ✅ OpenGL ES 2.0 for custom graphics
- ✅ Touch event handling
- ✅ Basic drawing and animations

### Platform Limitations

- ❌ No direct access to Android framework APIs
- ❌ Can't use Java/Kotlin libraries
- ❌ No system services (Camera, GPS, etc.)
- ✅ Network access (HTTP, TCP, UDP)
- ✅ File I/O
- ✅ Concurrency with goroutines

### When to Use gomobile build vs gomobile bind

| Feature | gomobile build | gomobile bind |
|---------|----------------|---------------|
| UI | OpenGL only | Full Android UI |
| Android APIs | Limited | Full access |
| Development speed | Fast | Medium |
| Code sharing | Easy | Moderate |
| Production apps | Demos/utilities | Production ready |
| Learning curve | Low | Medium |

## Build Automation

**build.sh script:**
```bash
#!/usr/bin/env bash
set -euo pipefail

APP_ID="com.example.myapp"
VERSION="1.0.0"
TARGET="android/arm64,android/arm"

echo "Building Android app..."

# Build APK
gomobile build \
    -target="$TARGET" \
    -appid="$APP_ID" \
    -ldflags="-X main.version=$VERSION" \
    .

echo "APK built: myapp.apk"

# Optional: Install on device
if adb devices | grep -q "device$"; then
    echo "Installing on device..."
    adb install -r myapp.apk
    echo "Launch with: adb shell am start -n $APP_ID/.MainActivity"
else
    echo "No device connected. Install with: adb install myapp.apk"
fi
```

## Debugging

### View Logs

```bash
# All logs
adb logcat

# Go logs only
adb logcat | grep GoLog

# Your app logs
adb logcat | grep "com.example.myapp"

# Clear logs first
adb logcat -c && adb logcat
```

### Add Debug Logging

```go
import "log"

func main() {
    log.SetPrefix("[MyApp] ")
    log.Println("App starting...")

    // Your code
}
```

### Check App Status

```bash
# Check if app is running
adb shell ps | grep myapp

# Check app info
adb shell dumpsys package com.example.myapp
```

## Performance Tips

1. **Use goroutines wisely**: They're not free, even in Go
2. **Minimize allocations**: GC pauses affect frame rate
3. **Use buffered channels**: Reduce goroutine blocking
4. **Profile your code**: Use pprof before deploying
5. **Test on real devices**: Emulators don't show real performance

## Common Issues

### Issue: "No Android devices detected"

```bash
# Check connected devices
adb devices

# Restart adb if needed
adb kill-server
adb start-server
adb devices
```

### Issue: "Installation failed"

```bash
# Uninstall old version first
adb uninstall com.example.myapp

# Reinstall
adb install myapp.apk
```

### Issue: "App crashes immediately"

```bash
# View crash logs
adb logcat | grep FATAL

# Check for panic
adb logcat | grep "panic:"
```

## Migration Path

### From gomobile build to gomobile bind

When your app outgrows gomobile build:

1. **Extract core logic to library:**
   ```go
   // In mylib/network.go
   package mylib

   func FetchData(url string) (string, error) {
       // Your logic
   }
   ```

2. **Build AAR:**
   ```bash
   gomobile bind -target=android -o mylib.aar ./mylib
   ```

3. **Create Android project** (see **Build** workflow)

4. **Use AAR from Kotlin:**
   ```kotlin
   import mylib.Mylib

   val data = Mylib.fetchData("https://example.com")
   ```

## Next Steps

- **Debug your app** - Use the **Debug** workflow
- **Test on emulator** - Use the **Emulator** workflow
- **Build production APK** - Use the **Build** and **Publish** workflows
- **Add richer UI** - Consider migrating to gomobile bind + Android project

## Resources

- [Gomobile Documentation](https://pkg.go.dev/golang.org/x/mobile)
- [Mobile App Package](https://pkg.go.dev/golang.org/x/mobile/app)
- [Mobile GL Package](https://pkg.go.dev/golang.org/x/mobile/gl)
- [Gomobile Examples](https://github.com/golang/mobile/tree/master/example)
