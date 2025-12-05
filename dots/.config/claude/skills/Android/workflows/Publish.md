# Publish Workflow

Sign, build, and publish Android applications to Google Play Store.

## Overview

Steps to publish:
1. Create signing key (keystore)
2. Configure signing in Gradle
3. Build release AAB
4. Test release build
5. Upload to Play Console
6. Complete store listing
7. Submit for review

## Create Signing Key

### Generate Keystore

```bash
keytool -genkey -v \
    -keystore release.keystore \
    -alias myapp-key \
    -keyalg RSA \
    -keysize 2048 \
    -validity 10000

# You'll be prompted for:
# - Keystore password
# - Key password
# - Name, organization, etc.
```

**Important:**
- **NEVER commit keystore to git**
- **Backup keystore securely** (losing it means you can't update your app)
- **Remember passwords** (write them down securely)

### Keystore Information

View keystore details:
```bash
keytool -list -v -keystore release.keystore
```

### Alternative: Use Play App Signing

Google can manage signing for you:
1. Google generates and stores the key
2. You upload signed AAB
3. Google re-signs with production key
4. Safer but requires Play App Signing enrollment

## Configure Gradle Signing

### Option 1: Environment Variables (Recommended)

**Store credentials outside repository:**

`app/build.gradle.kts`:
```kotlin
android {
    signingConfigs {
        create("release") {
            storeFile = file("../release.keystore")
            storePassword = System.getenv("KEYSTORE_PASSWORD")
            keyAlias = "myapp-key"
            keyPassword = System.getenv("KEY_PASSWORD")
        }
    }

    buildTypes {
        release {
            signingConfig = signingConfigs.getByName("release")
            isMinifyEnabled = true
            isShrinkResources = true
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
        }
    }
}
```

**Build with environment variables:**
```bash
export KEYSTORE_PASSWORD="your-keystore-password"
export KEY_PASSWORD="your-key-password"
./gradlew bundleRelease
```

### Option 2: gradle.properties (Local only)

**Add to `~/.gradle/gradle.properties` (NOT project gradle.properties):**
```properties
MYAPP_RELEASE_STORE_FILE=../release.keystore
MYAPP_RELEASE_KEY_ALIAS=myapp-key
MYAPP_RELEASE_STORE_PASSWORD=your-keystore-password
MYAPP_RELEASE_KEY_PASSWORD=your-key-password
```

`app/build.gradle.kts`:
```kotlin
android {
    signingConfigs {
        create("release") {
            storeFile = file(project.properties["MYAPP_RELEASE_STORE_FILE"] as String)
            storePassword = project.properties["MYAPP_RELEASE_STORE_PASSWORD"] as String
            keyAlias = project.properties["MYAPP_RELEASE_KEY_ALIAS"] as String
            keyPassword = project.properties["MYAPP_RELEASE_KEY_PASSWORD"] as String
        }
    }

    buildTypes {
        release {
            signingConfig = signingConfigs.getByName("release")
        }
    }
}
```

## Build Release

### Build AAB (Play Store)

```bash
./gradlew bundleRelease

# Output location
ls app/build/outputs/bundle/release/app-release.aab
```

### Build APK (Direct distribution)

```bash
./gradlew assembleRelease

# Output location
ls app/build/outputs/apk/release/app-release.apk
```

### Verify Signature

```bash
# Check AAB signature
jarsigner -verify -verbose app-release.aab

# Check APK signature
apksigner verify --verbose app-release.apk
```

## Test Release Build

**Important: Test before uploading!**

### Install Release APK

```bash
# Build APK from AAB (using bundletool)
# Download bundletool from:
# https://github.com/google/bundletool/releases

# Generate APKs
java -jar bundletool-all.jar build-apks \
    --bundle=app-release.aab \
    --output=app.apks \
    --mode=universal

# Extract universal APK
unzip app.apks universal.apk

# Install
adb install universal.apk
```

### Test Checklist

- [ ] App installs successfully
- [ ] App launches without crashes
- [ ] All features work correctly
- [ ] ProGuard hasn't broken anything
- [ ] Gomobile integration works
- [ ] Network requests succeed
- [ ] UI looks correct
- [ ] Performance is acceptable

## Upload to Play Console

### Prerequisites

1. **Google Play Developer account** ($25 one-time fee)
2. **App created in Play Console**
3. **Store listing completed**

### Upload AAB

1. Go to [Play Console](https://play.google.com/console)
2. Select your app
3. Production → Releases
4. Create new release
5. Upload AAB file
6. Fill release notes
7. Review and rollout

### Release Tracks

| Track | Purpose | Audience |
|-------|---------|----------|
| Internal testing | Quick testing | Up to 100 testers |
| Closed testing | Alpha/beta | Invited testers |
| Open testing | Public beta | Anyone can join |
| Production | Public release | All users |

## Versioning

### Version Code

Monotonically increasing integer:
```kotlin
android {
    defaultConfig {
        versionCode = 1  // Increment for each release
        versionName = "1.0"
    }
}
```

### Version Name

Human-readable version string:
```
1.0.0 - Major.Minor.Patch (Semantic Versioning)
```

### Automated Versioning

```kotlin
// Read version from git tags
fun getVersionCode(): Int {
    val process = Runtime.getRuntime().exec("git rev-list --count HEAD")
    return process.inputStream.bufferedReader().readText().trim().toInt()
}

fun getVersionName(): String {
    val process = Runtime.getRuntime().exec("git describe --tags --always")
    return process.inputStream.bufferedReader().readText().trim()
}

android {
    defaultConfig {
        versionCode = getVersionCode()
        versionName = getVersionName()
    }
}
```

## Store Listing

### Required Assets

**Screenshots:**
- Minimum 2 screenshots
- Recommended: 4-8 screenshots
- Resolutions: phone, tablet, TV, Wear OS

**Icon:**
- 512x512 PNG
- 32-bit color + alpha channel

**Feature Graphic:**
- 1024x500 PNG
- Displayed in Play Store

### Required Information

- App name
- Short description (80 characters)
- Full description (4000 characters)
- Category
- Content rating (via questionnaire)
- Privacy policy URL (if app collects data)
- Contact email

## CI/CD Publishing

### GitHub Actions Example

`.github/workflows/release.yml`:
```yaml
name: Release

on:
  push:
    tags:
      - 'v*'

jobs:
  release:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Set up JDK 17
        uses: actions/setup-java@v4
        with:
          distribution: 'temurin'
          java-version: '17'

      - name: Setup Go
        uses: actions/setup-go@v5
        with:
          go-version: '1.22'

      - name: Build gomobile AAR
        run: |
          go install golang.org/x/mobile/cmd/gomobile@latest
          gomobile init
          cd golib
          gomobile bind -target=android -o mylib.aar .
          cp mylib.aar ../app/libs/

      - name: Build Release AAB
        env:
          KEYSTORE_PASSWORD: ${{ secrets.KEYSTORE_PASSWORD }}
          KEY_PASSWORD: ${{ secrets.KEY_PASSWORD }}
        run: |
          echo "${{ secrets.KEYSTORE_BASE64 }}" | base64 -d > release.keystore
          ./gradlew bundleRelease

      - name: Upload to Play Store
        uses: r0adkll/upload-google-play@v1
        with:
          serviceAccountJsonPlainText: ${{ secrets.SERVICE_ACCOUNT_JSON }}
          packageName: com.example.myapp
          releaseFiles: app/build/outputs/bundle/release/app-release.aab
          track: production
          status: completed
```

**Setup secrets in GitHub:**
1. Settings → Secrets → Actions
2. Add:
   - `KEYSTORE_PASSWORD`
   - `KEY_PASSWORD`
   - `KEYSTORE_BASE64` (base64 encoded keystore file)
   - `SERVICE_ACCOUNT_JSON` (Play Console API credentials)

### Get Play Console API Credentials

1. Google Cloud Console → Create Service Account
2. Download JSON key
3. Play Console → API access
4. Link service account
5. Grant permissions

## Post-Release

### Monitor Crashes

- Play Console → Quality → Crashes
- View crash reports and ANR (Application Not Responding) reports
- Consider using Firebase Crashlytics for detailed reports

### Monitor Reviews

- Respond to user reviews
- Address common issues in updates

### Track Metrics

- Installs
- Uninstalls
- Active users
- Retention rates
- Crashes

## Update Releases

### Prepare Update

1. Fix bugs / add features
2. Increment `versionCode`
3. Update `versionName`
4. Update release notes

### Release Process

```bash
# Build new release
./gradlew bundleRelease

# Test thoroughly
# Upload to Play Console
# Submit for review
```

### Staged Rollout (Recommended)

Release to percentage of users:
1. Start with 5-10%
2. Monitor crashes/issues
3. Increase to 25%, 50%, 100%

If issues found:
- Halt rollout
- Fix and release new version

## Best Practices

### Security

- ✅ Enable ProGuard/R8
- ✅ Remove logging in release builds
- ✅ Validate all inputs
- ✅ Use HTTPS for network
- ✅ Store keystore securely
- ✅ Use Play App Signing
- ❌ Never commit keystore passwords

### Build Configuration

```kotlin
buildTypes {
    release {
        isMinifyEnabled = true
        isShrinkResources = true
        proguardFiles(...)

        // Remove debug code
        buildConfigField("Boolean", "DEBUG_MODE", "false")

        // Disable logging
        buildConfigField("Boolean", "ENABLE_LOGGING", "false")
    }

    debug {
        applicationIdSuffix = ".debug"
        isDebuggable = true
        buildConfigField("Boolean", "DEBUG_MODE", "true")
        buildConfigField("Boolean", "ENABLE_LOGGING", "true")
    }
}
```

### Testing

- Test release build on multiple devices
- Test all features thoroughly
- Check ProGuard hasn't broken anything
- Verify gomobile integration
- Test with poor network conditions
- Test edge cases

## Troubleshooting

### "App not signed" error

```bash
# Check signing configuration
./gradlew :app:signingReport

# Verify keystore exists and credentials are correct
```

### ProGuard breaks app

Add keep rules in `proguard-rules.pro`:
```proguard
-keep class com.your.package.** { *; }
```

### Upload rejected by Play Console

Common reasons:
- Version code not incremented
- Signature mismatch
- Missing required permissions
- Policy violations

## Next Steps

- **Monitor app performance** - Play Console metrics
- **Respond to user feedback** - Reviews and ratings
- **Plan updates** - New features and bug fixes

## Resources

- [Publish Your App](https://developer.android.com/studio/publish)
- [Play Console](https://play.google.com/console)
- [App Signing](https://developer.android.com/studio/publish/app-signing)
- [Launch Checklist](https://developer.android.com/distribute/best-practices/launch/launch-checklist)
