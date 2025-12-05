# Gradle Workflow

Manage Android project dependencies, build configuration, and Gradle tasks.

## Gradle Basics

### Gradle Wrapper

Always use the wrapper for reproducible builds:

```bash
# Linux/Mac
./gradlew <task>

# Windows
gradlew.bat <task>

# Update wrapper
./gradlew wrapper --gradle-version=8.6
```

### Project Structure

```
myproject/
├── gradle/
│   ├── libs.versions.toml      # Version catalog (recommended)
│   └── wrapper/
│       ├── gradle-wrapper.jar
│       └── gradle-wrapper.properties
├── app/
│   └── build.gradle.kts        # App module build file
├── build.gradle.kts            # Root build file
├── settings.gradle.kts         # Project settings
├── gradle.properties           # Global Gradle properties
└── local.properties            # Local config (not in git)
```

## Version Catalogs (Modern Approach)

### gradle/libs.versions.toml

```toml
[versions]
agp = "8.3.0"                       # Android Gradle Plugin
kotlin = "1.9.22"
compileSdk = "34"
minSdk = "24"
targetSdk = "34"

# Dependencies
androidx-core = "1.12.0"
androidx-appcompat = "1.6.1"
material = "1.11.0"
androidx-activity = "1.8.2"
androidx-constraintlayout = "2.1.4"

# Testing
junit = "4.13.2"
androidx-test-ext-junit = "1.1.5"
espresso-core = "3.5.1"

[libraries]
androidx-core-ktx = { group = "androidx.core", name = "core-ktx", version.ref = "androidx-core" }
androidx-appcompat = { group = "androidx.appcompat", name = "appcompat", version.ref = "androidx-appcompat" }
material = { group = "com.google.android.material", name = "material", version.ref = "material" }
androidx-activity = { group = "androidx.activity", name = "activity", version.ref = "androidx-activity" }
androidx-constraintlayout = { group = "androidx.constraintlayout", name = "constraintlayout", version.ref = "androidx-constraintlayout" }

# Testing
junit = { group = "junit", name = "junit", version.ref = "junit" }
androidx-test-ext-junit = { group = "androidx.test.ext", name = "junit", version.ref = "androidx-test-ext-junit" }
androidx-espresso-core = { group = "androidx.test.espresso", name = "espresso-core", version.ref = "espresso-core" }

[plugins]
android-application = { id = "com.android.application", version.ref = "agp" }
kotlin-android = { id = "org.jetbrains.kotlin.android", version.ref = "kotlin" }
```

### Root build.gradle.kts

```kotlin
// Top-level build file
plugins {
    alias(libs.plugins.android.application) apply false
    alias(libs.plugins.kotlin.android) apply false
}
```

### App build.gradle.kts

```kotlin
plugins {
    alias(libs.plugins.android.application)
    alias(libs.plugins.kotlin.android)
}

android {
    namespace = "com.example.myapp"
    compileSdk = 34

    defaultConfig {
        applicationId = "com.example.myapp"
        minSdk = 24
        targetSdk = 34
        versionCode = 1
        versionName = "1.0"

        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
    }

    buildTypes {
        release {
            isMinifyEnabled = true
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
        }
    }

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }

    kotlinOptions {
        jvmTarget = "17"
    }
}

dependencies {
    implementation(libs.androidx.core.ktx)
    implementation(libs.androidx.appcompat)
    implementation(libs.material)
    implementation(libs.androidx.activity)
    implementation(libs.androidx.constraintlayout)

    testImplementation(libs.junit)
    androidTestImplementation(libs.androidx.test.ext.junit)
    androidTestImplementation(libs.androidx.espresso.core)

    // Gomobile AAR
    implementation(files("libs/mylib.aar"))
}
```

## Managing Dependencies

### Add Dependencies

**Using version catalog (recommended):**

1. Add to `gradle/libs.versions.toml`:
```toml
[versions]
retrofit = "2.9.0"

[libraries]
retrofit = { group = "com.squareup.retrofit2", name = "retrofit", version.ref = "retrofit" }
```

2. Use in `app/build.gradle.kts`:
```kotlin
dependencies {
    implementation(libs.retrofit)
}
```

**Direct declaration:**
```kotlin
dependencies {
    implementation("com.squareup.retrofit2:retrofit:2.9.0")
}
```

### Dependency Scopes

```kotlin
dependencies {
    // Compile and runtime
    implementation("...")      // Recommended: Not exposed to consumers

    // Compile only (not in APK)
    compileOnly("...")         // Example: annotations

    // Runtime only
    runtimeOnly("...")         // Example: JDBC drivers

    // Exposed to consumers
    api("...")                 // Use sparingly in libraries

    // Testing
    testImplementation("...")           // Unit tests (JVM)
    androidTestImplementation("...")    // Instrumentation tests (Android)

    // Debug build only
    debugImplementation("...")          // Example: debug tools
}
```

### Local AAR/JAR Files

```kotlin
dependencies {
    // Single AAR
    implementation(files("libs/mylib.aar"))

    // All JARs in libs directory
    implementation(fileTree(mapOf("dir" to "libs", "include" to listOf("*.jar"))))

    // All AARs in libs directory
    implementation(fileTree(mapOf("dir" to "libs", "include" to listOf("*.aar"))))
}
```

### Multi-module Dependencies

```kotlin
dependencies {
    // Depend on another module in the project
    implementation(project(":library"))
}
```

## Configuration

### gradle.properties

```properties
# Performance
org.gradle.jvmargs=-Xmx4096m -XX:MaxMetaspaceSize=1024m
org.gradle.daemon=true
org.gradle.parallel=true
org.gradle.caching=true
org.gradle.configureondemand=true

# AndroidX
android.useAndroidX=true
android.enableJetifier=true

# Kotlin
kotlin.code.style=official

# Build
android.nonTransitiveRClass=true
android.defaults.buildfeatures.buildconfig=true
```

### local.properties

```properties
# SDK location (don't commit to git)
sdk.dir=/home/user/Android/Sdk

# NDK location (optional)
ndk.dir=/home/user/Android/Sdk/ndk/26.1.10909125
```

## Build Variants

### Build Types

```kotlin
android {
    buildTypes {
        debug {
            applicationIdSuffix = ".debug"
            isDebuggable = true
            isMinifyEnabled = false
        }

        release {
            isMinifyEnabled = true
            isShrinkResources = true
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
        }

        create("staging") {
            initWith(getByName("debug"))
            applicationIdSuffix = ".staging"
        }
    }
}
```

### Product Flavors

```kotlin
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

Build combinations:
```bash
./gradlew assembleFreeDebug
./gradlew assembleFreeRelease
./gradlew assemblePaidDebug
./gradlew assemblePaidRelease
```

## ProGuard / R8

### Enable Code Shrinking

```kotlin
android {
    buildTypes {
        release {
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

### proguard-rules.pro

```proguard
# Keep gomobile generated classes
-keep class go.** { *; }
-keep class mylib.** { *; }

# Keep model classes
-keep class com.example.myapp.models.** { *; }

# Retrofit
-keepattributes Signature
-keepattributes Exceptions

# Kotlin
-keep class kotlin.** { *; }
-keep class kotlin.Metadata { *; }
-dontwarn kotlin.**
```

## Common Tasks

```bash
# List all tasks
./gradlew tasks

# List dependencies
./gradlew app:dependencies

# Show dependency tree
./gradlew app:dependencies --configuration debugRuntimeClasspath

# Check for updates
./gradlew dependencyUpdates

# Clean build
./gradlew clean

# Build all variants
./gradlew build

# Build specific variant
./gradlew assembleDebug
./gradlew assembleRelease

# Run tests
./gradlew test
./gradlew connectedAndroidTest

# Install on device
./gradlew installDebug
```

## Troubleshooting

### Dependency Conflicts

```bash
# View conflict resolution
./gradlew app:dependencies

# Force specific version
dependencies {
    implementation("com.example:library:1.0") {
        force = true
    }
}

# Exclude transitive dependency
dependencies {
    implementation("com.example:library:1.0") {
        exclude(group = "com.unwanted", module = "module")
    }
}
```

### Sync Issues

```bash
# Refresh dependencies
./gradlew --refresh-dependencies

# Clear cache
./gradlew clean cleanBuildCache

# Delete .gradle folder
rm -rf .gradle
./gradlew build
```

### Memory Issues

Increase memory in `gradle.properties`:
```properties
org.gradle.jvmargs=-Xmx8192m -XX:MaxMetaspaceSize=2048m
```

## Multi-module Projects

### settings.gradle.kts

```kotlin
pluginManagement {
    repositories {
        google()
        mavenCentral()
        gradlePluginPortal()
    }
}

dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)
    repositories {
        google()
        mavenCentral()
    }
}

rootProject.name = "MyProject"
include(":app")
include(":library")
```

### Library Module

`library/build.gradle.kts`:
```kotlin
plugins {
    alias(libs.plugins.android.library)
    alias(libs.plugins.kotlin.android)
}

android {
    namespace = "com.example.library"
    compileSdk = 34

    defaultConfig {
        minSdk = 24
    }
}
```

Use from app:
```kotlin
dependencies {
    implementation(project(":library"))
}
```

## Optimization Tips

1. **Use version catalogs**: Centralize dependency versions
2. **Enable build cache**: Faster incremental builds
3. **Use Gradle daemon**: Reduces startup time
4. **Parallel execution**: Builds modules in parallel
5. **Configure on demand**: Only configure needed projects
6. **Avoid clean**: Let Gradle handle incremental builds

## Next Steps

- **Build APK/AAB** - Use the **Build** workflow
- **Manage gomobile integration** - Use the **GomobileBind** workflow
- **Run tests** - Use the **Test** workflow

## Resources

- [Gradle Plugin User Guide](https://developer.android.com/build)
- [Version Catalogs](https://docs.gradle.org/current/userguide/platforms.html)
- [Dependency Management](https://developer.android.com/studio/build/dependencies)
