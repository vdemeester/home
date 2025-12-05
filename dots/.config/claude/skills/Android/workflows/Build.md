# Build Workflow

Build Android applications (APK/AAB) using Gradle build system.

## Build Types

### APK (Android Package)
- Direct install on devices
- Can be shared and sideloaded
- Larger file size (contains all ABIs)
- Use for: Development, testing, direct distribution

### AAB (Android App Bundle)
- Play Store distribution format
- Smaller download size (dynamic delivery)
- Cannot be directly installed
- Use for: Play Store releases

## Quick Start

```bash
# Navigate to project
cd /path/to/android-project

# Build debug APK
./gradlew assembleDebug

# Build release APK
./gradlew assembleRelease

# Build release AAB
./gradlew bundleRelease

# Install debug on device
./gradlew installDebug
```

## Build Variants

Android projects typically have:

| Variant | Use Case | Signed | Minified |
|---------|----------|--------|----------|
| Debug | Development | Auto | No |
| Release | Production | Manual | Yes |

### Debug Build

```bash
# Build
./gradlew assembleDebug

# Output location
ls app/build/outputs/apk/debug/app-debug.apk

# Install on connected device
adb install app/build/outputs/apk/debug/app-debug.apk

# Or use Gradle install task
./gradlew installDebug
```

### Release Build

Requires signing configuration (see **Publish** workflow).

```bash
# Build signed release APK
./gradlew assembleRelease

# Output
ls app/build/outputs/apk/release/app-release.apk

# Build AAB for Play Store
./gradlew bundleRelease

# Output
ls app/build/outputs/bundle/release/app-release.aab
```

## Build Configuration

### Gradle Wrapper

Always use the wrapper (./gradlew) for consistent builds:

```bash
# Linux/Mac
./gradlew <task>

# Windows
gradlew.bat <task>

# Update wrapper
./gradlew wrapper --gradle-version=8.6
```

### Common Gradle Tasks

```bash
# List all tasks
./gradlew tasks

# Clean build outputs
./gradlew clean

# Build all variants
./gradlew build

# Build and run tests
./gradlew build test

# Check dependencies
./gradlew dependencies

# Show project structure
./gradlew projects
```

## Build Optimization

### Enable Build Cache

`gradle.properties`:
```properties
org.gradle.caching=true
org.gradle.parallel=true
org.gradle.daemon=true
org.gradle.jvmargs=-Xmx4096m -XX:MaxMetaspaceSize=1024m
```

### Optimize Build Time

```kotlin
// app/build.gradle.kts
android {
    // Use only needed ABIs during development
    splits {
        abi {
            isEnable = false
        }
    }

    // Disable PNG crunching in debug
    buildTypes {
        debug {
            isCrunchPngs = false
        }
    }
}
```

### Incremental Builds

Gradle automatically handles incremental builds. Avoid `clean` unless necessary.

```bash
# Good: Incremental build
./gradlew assembleDebug

# Bad: Full rebuild (slower)
./gradlew clean assembleDebug
```

## Multi-Module Projects

For projects with multiple modules:

```bash
# Build specific module
./gradlew :app:assembleDebug
./gradlew :library:build

# Build all modules
./gradlew build
```

## Build with Different ABIs

### Split APKs by ABI

Reduces APK size by creating separate APKs per architecture.

```kotlin
// app/build.gradle.kts
android {
    splits {
        abi {
            isEnable = true
            reset()
            include("arm64-v8a", "armeabi-v7a", "x86_64", "x86")
            isUniversalApk = false  // Set true to also create universal APK
        }
    }
}
```

Build output:
```
app/build/outputs/apk/release/
├── app-arm64-v8a-release.apk
├── app-armeabi-v7a-release.apk
├── app-x86_64-release.apk
└── app-x86-release.apk
```

### Filter ABIs (Development)

Include only needed ABIs for faster builds:

```kotlin
android {
    defaultConfig {
        ndk {
            // Only build for emulator during development
            abiFilters.addAll(listOf("x86_64"))

            // Or for physical device
            // abiFilters.addAll(listOf("arm64-v8a"))
        }
    }
}
```

## Build with Gomobile AAR

If using gomobile bind:

```bash
# 1. Build AAR from Go code
cd golib
gomobile bind -target=android -o mylib.aar .

# 2. Copy to Android project
cp mylib.aar /path/to/android-project/app/libs/

# 3. Build Android app
cd /path/to/android-project
./gradlew assembleDebug
```

Automate in script:
```bash
#!/usr/bin/env bash
set -euo pipefail

echo "Building Go library..."
cd golib
gomobile bind -target=android -o mylib.aar .

echo "Copying AAR to Android project..."
cp mylib.aar ../app/libs/

echo "Building Android app..."
cd ..
./gradlew assembleDebug

echo "Done! APK: app/build/outputs/apk/debug/app-debug.apk"
```

## Build Flavors

Create different versions of your app:

```kotlin
// app/build.gradle.kts
android {
    flavorDimensions += "version"

    productFlavors {
        create("free") {
            dimension = "version"
            applicationIdSuffix = ".free"
            versionNameSuffix = "-free"
        }

        create("paid") {
            dimension = "version"
            applicationIdSuffix = ".paid"
            versionNameSuffix = "-paid"
        }
    }
}
```

Build specific flavor:
```bash
# Free debug
./gradlew assembleFreeDebug

# Paid release
./gradlew assemblePaidRelease

# All flavors and variants
./gradlew assemble
```

## Continuous Integration

### GitHub Actions Example

`.github/workflows/android.yml`:
```yaml
name: Android CI

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Set up JDK 17
        uses: actions/setup-java@v4
        with:
          distribution: 'temurin'
          java-version: '17'

      - name: Setup Android SDK
        uses: android-actions/setup-android@v3

      - name: Setup Go
        uses: actions/setup-go@v5
        with:
          go-version: '1.22'

      - name: Install gomobile
        run: |
          go install golang.org/x/mobile/cmd/gomobile@latest
          gomobile init

      - name: Build Go library
        run: |
          cd golib
          gomobile bind -target=android -o mylib.aar .
          cp mylib.aar ../app/libs/

      - name: Build with Gradle
        run: ./gradlew assembleDebug

      - name: Upload APK
        uses: actions/upload-artifact@v4
        with:
          name: app-debug
          path: app/build/outputs/apk/debug/app-debug.apk
```

## Troubleshooting

### Build fails with "SDK not found"

```bash
# Create local.properties
echo "sdk.dir=$ANDROID_HOME" > local.properties
```

### OutOfMemoryError during build

Increase Gradle memory in `gradle.properties`:
```properties
org.gradle.jvmargs=-Xmx8192m -XX:MaxMetaspaceSize=2048m
```

### Duplicate class errors

```kotlin
// Use packagingOptions to exclude duplicates
android {
    packagingOptions {
        resources {
            excludes += "META-INF/*.kotlin_module"
        }
    }
}
```

### AAR not found

```kotlin
// Ensure AAR is in libs directory
dependencies {
    implementation(fileTree(mapOf("dir" to "libs", "include" to listOf("*.aar"))))
    // Or
    implementation(files("libs/mylib.aar"))
}
```

## Build Output

### APK Structure

```
app-debug.apk
├── AndroidManifest.xml
├── classes.dex           # Compiled Kotlin/Java code
├── lib/                  # Native libraries
│   ├── arm64-v8a/
│   ├── armeabi-v7a/
│   └── x86_64/
├── res/                  # Resources
└── resources.arsc        # Compiled resources
```

### Inspect APK

```bash
# Extract APK
unzip app-debug.apk -d extracted

# View manifest
aapt dump badging app-debug.apk

# View method count
# Install dex-method-counts first
dex-method-counts app-debug.apk
```

## Next Steps

- **Install and test** - Use the **Debug** workflow
- **Sign and publish** - Use the **Publish** workflow
- **Run tests** - Use the **Test** workflow

## Resources

- [Gradle Build Documentation](https://developer.android.com/studio/build)
- [Configure Build Variants](https://developer.android.com/studio/build/build-variants)
- [Shrink, Obfuscate, and Optimize](https://developer.android.com/studio/build/shrink-code)
