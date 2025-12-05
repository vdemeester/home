# Test Workflow

Write and run tests for Android applications: unit tests, instrumentation tests, and UI tests.

## Test Types

| Type | Runs On | Speed | Use For |
|------|---------|-------|---------|
| Unit Tests | JVM | Fast | Business logic, utilities |
| Instrumentation Tests | Android device/emulator | Slow | Android APIs, database |
| UI Tests | Android device/emulator | Slowest | User interface, interactions |

## Unit Tests (Local Tests)

### Location

`app/src/test/java/com/example/myapp/`

### Dependencies

`app/build.gradle.kts`:
```kotlin
dependencies {
    testImplementation("junit:junit:4.13.2")
    testImplementation("org.mockito:mockito-core:5.7.0")
    testImplementation("org.mockito.kotlin:mockito-kotlin:5.2.1")
}
```

### Example Unit Test

```kotlin
// app/src/test/java/com/example/myapp/CalculatorTest.kt
package com.example.myapp

import org.junit.Test
import org.junit.Assert.*

class CalculatorTest {

    @Test
    fun addition_isCorrect() {
        val calculator = Calculator()
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }

    @Test
    fun division_byZero_throwsException() {
        val calculator = Calculator()
        assertThrows(ArithmeticException::class.java) {
            calculator.divide(10, 0)
        }
    }
}
```

### Run Unit Tests

```bash
# All unit tests
./gradlew test

# Specific variant
./gradlew testDebug
./gradlew testRelease

# Specific test class
./gradlew test --tests CalculatorTest

# Specific test method
./gradlew test --tests CalculatorTest.addition_isCorrect

# With coverage report
./gradlew testDebugUnitTest jacocoTestReport
```

### View Results

```bash
# HTML report location
open app/build/reports/tests/testDebugUnitTest/index.html

# Coverage report
open app/build/reports/jacoco/jacocoTestReport/html/index.html
```

## Instrumentation Tests

### Location

`app/src/androidTest/java/com/example/myapp/`

### Dependencies

`app/build.gradle.kts`:
```kotlin
android {
    defaultConfig {
        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
    }
}

dependencies {
    androidTestImplementation("androidx.test.ext:junit:1.1.5")
    androidTestImplementation("androidx.test:runner:1.5.2")
    androidTestImplementation("androidx.test:rules:1.5.0")
    androidTestImplementation("androidx.test.espresso:espresso-core:3.5.1")
}
```

### Example Instrumentation Test

```kotlin
// app/src/androidTest/java/com/example/myapp/DatabaseTest.kt
package com.example.myapp

import androidx.test.ext.junit.runners.AndroidJUnit4
import androidx.test.platform.app.InstrumentationRegistry
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Assert.*

@RunWith(AndroidJUnit4::class)
class DatabaseTest {

    @Test
    fun useAppContext() {
        val appContext = InstrumentationRegistry.getInstrumentation().targetContext
        assertEquals("com.example.myapp", appContext.packageName)
    }

    @Test
    fun database_insertAndQuery() {
        val appContext = InstrumentationRegistry.getInstrumentation().targetContext
        // Test database operations
    }
}
```

### Run Instrumentation Tests

```bash
# Connect device/emulator first
adb devices

# All instrumentation tests
./gradlew connectedAndroidTest

# Specific variant
./gradlew connectedDebugAndroidTest

# Specific test
./gradlew connectedAndroidTest -Pandroid.testInstrumentationRunnerArguments.class=com.example.myapp.DatabaseTest
```

## UI Tests (Espresso)

### Dependencies

```kotlin
dependencies {
    androidTestImplementation("androidx.test.espresso:espresso-core:3.5.1")
    androidTestImplementation("androidx.test.espresso:espresso-intents:3.5.1")
    androidTestImplementation("androidx.test.espresso:espresso-contrib:3.5.1")
}
```

### Example UI Test

```kotlin
// app/src/androidTest/java/com/example/myapp/MainActivityTest.kt
package com.example.myapp

import androidx.test.ext.junit.rules.ActivityScenarioRule
import androidx.test.ext.junit.runners.AndroidJUnit4
import androidx.test.espresso.Espresso.onView
import androidx.test.espresso.action.ViewActions.*
import androidx.test.espresso.assertion.ViewAssertions.matches
import androidx.test.espresso.matcher.ViewMatchers.*
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(AndroidJUnit4::class)
class MainActivityTest {

    @get:Rule
    val activityRule = ActivityScenarioRule(MainActivity::class.java)

    @Test
    fun button_click_showsText() {
        // Type text in EditText
        onView(withId(R.id.editText))
            .perform(typeText("Hello"), closeSoftKeyboard())

        // Click button
        onView(withId(R.id.button))
            .perform(click())

        // Verify text is displayed
        onView(withId(R.id.textView))
            .check(matches(withText("Hello")))
    }

    @Test
    fun recyclerView_scrollAndClick() {
        // Scroll to position
        onView(withId(R.id.recyclerView))
            .perform(scrollToPosition<RecyclerView.ViewHolder>(10))

        // Click item
        onView(withText("Item 10"))
            .perform(click())
    }
}
```

### Common Espresso Operations

```kotlin
// Find views
onView(withId(R.id.viewId))
onView(withText("text"))
onView(withContentDescription("description"))

// Actions
perform(click())
perform(typeText("text"))
perform(replaceText("new text"))
perform(clearText())
perform(closeSoftKeyboard())
perform(swipeLeft())
perform(swipeRight())
perform(scrollTo())

// Assertions
check(matches(isDisplayed()))
check(matches(withText("expected")))
check(matches(isEnabled()))
check(matches(isChecked()))
check(doesNotExist())
```

## Testing Gomobile Integration

### Unit Test Go Code (Before binding)

```bash
cd golib
go test ./...
go test -v ./...
go test -cover ./...
```

### Test AAR Integration

```kotlin
// app/src/androidTest/java/com/example/myapp/GomobileTest.kt
package com.example.myapp

import androidx.test.ext.junit.runners.AndroidJUnit4
import mylib.Mylib  // Import gomobile generated package
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Assert.*

@RunWith(AndroidJUnit4::class)
class GomobileTest {

    @Test
    fun goFunction_returnsExpectedResult() {
        val result = Mylib.fetchURL("https://example.com")
        assertNotNull(result)
        assertTrue(result.isNotEmpty())
    }

    @Test
    fun goFunction_handlesError() {
        try {
            Mylib.fetchURL("invalid-url")
            fail("Should have thrown exception")
        } catch (e: Exception) {
            // Expected
            assertTrue(e.message?.contains("error") == true)
        }
    }
}
```

## Test Configuration

### JaCoCo (Code Coverage)

`app/build.gradle.kts`:
```kotlin
plugins {
    id("jacoco")
}

android {
    buildTypes {
        debug {
            enableAndroidTestCoverage = true
            enableUnitTestCoverage = true
        }
    }
}

tasks.register<JacocoReport>("jacocoTestReport") {
    dependsOn("testDebugUnitTest", "createDebugCoverageReport")

    reports {
        xml.required.set(true)
        html.required.set(true)
    }

    val fileFilter = listOf(
        "**/R.class",
        "**/R$*.class",
        "**/BuildConfig.*",
        "**/Manifest*.*"
    )

    val debugTree = fileTree("${project.buildDir}/intermediates/javac/debug") {
        exclude(fileFilter)
    }

    val mainSrc = "${project.projectDir}/src/main/java"

    sourceDirectories.setFrom(files(mainSrc))
    classDirectories.setFrom(files(debugTree))
    executionData.setFrom(fileTree(project.buildDir) {
        include("jacoco/testDebugUnitTest.exec", "outputs/code_coverage/debugAndroidTest/connected/**/*.ec")
    })
}
```

Run coverage:
```bash
./gradlew jacocoTestReport
open app/build/reports/jacoco/jacocoTestReport/html/index.html
```

## Test Best Practices

### 1. Follow AAA Pattern

```kotlin
@Test
fun testName() {
    // Arrange - Set up test data
    val calculator = Calculator()
    val a = 2
    val b = 3

    // Act - Execute the function
    val result = calculator.add(a, b)

    // Assert - Verify the result
    assertEquals(5, result)
}
```

### 2. Use Descriptive Names

```kotlin
// Good
@Test
fun addition_withPositiveNumbers_returnsSum() { }

@Test
fun division_withZeroDivisor_throwsException() { }

// Bad
@Test
fun test1() { }

@Test
fun testAdd() { }
```

### 3. Test One Thing

```kotlin
// Good - separate tests
@Test
fun add_returnsCorrectSum() { }

@Test
fun add_handlesNegativeNumbers() { }

// Bad - testing multiple things
@Test
fun testAddition() {
    // Tests multiple scenarios in one test
}
```

### 4. Mock External Dependencies

```kotlin
import org.mockito.Mock
import org.mockito.Mockito.*

class UserRepositoryTest {

    @Mock
    lateinit var api: ApiService

    @Test
    fun getUser_callsApi() {
        val userId = "123"
        `when`(api.fetchUser(userId)).thenReturn(User("John"))

        val repository = UserRepository(api)
        val user = repository.getUser(userId)

        verify(api).fetchUser(userId)
        assertEquals("John", user.name)
    }
}
```

## CI/CD Testing

### GitHub Actions

`.github/workflows/test.yml`:
```yaml
name: Android CI

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  test:
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

      - name: Test Go code
        run: |
          cd golib
          go test -v ./...

      - name: Build gomobile AAR
        run: |
          go install golang.org/x/mobile/cmd/gomobile@latest
          gomobile init
          cd golib
          gomobile bind -target=android -o mylib.aar .
          cp mylib.aar ../app/libs/

      - name: Run unit tests
        run: ./gradlew test

      - name: Run instrumented tests
        uses: reactivecircus/android-emulator-runner@v2
        with:
          api-level: 34
          target: google_apis
          arch: x86_64
          script: ./gradlew connectedCheck

      - name: Upload test results
        if: always()
        uses: actions/upload-artifact@v4
        with:
          name: test-results
          path: app/build/reports/tests/

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          files: app/build/reports/jacoco/jacocoTestReport/jacocoTestReport.xml
```

## Troubleshooting

### Tests fail on CI but pass locally

- Check Android API level compatibility
- Verify emulator configuration
- Check for timing issues (add timeouts)
- Ensure deterministic test data

### Instrumentation tests hang

```bash
# Clear app data between tests
adb shell pm clear com.example.myapp

# Restart adb
adb kill-server
adb start-server
```

### Espresso can't find view

```kotlin
// Wait for view with idling resources
import androidx.test.espresso.IdlingRegistry
import androidx.test.espresso.IdlingResource

// Or use explicit waits (not ideal)
Thread.sleep(1000)  // Avoid this in production tests
```

## Next Steps

- **Increase coverage** - Aim for >80%
- **Add UI tests** - Cover critical user flows
- **Integrate CI/CD** - Automated testing on every commit
- **Monitor flaky tests** - Fix non-deterministic tests

## Resources

- [Android Testing](https://developer.android.com/training/testing)
- [Espresso Documentation](https://developer.android.com/training/testing/espresso)
- [JUnit Documentation](https://junit.org/junit4/)
- [Mockito Documentation](https://site.mockito.org/)
