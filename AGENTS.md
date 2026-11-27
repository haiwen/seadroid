# AGENTS.md

## Scope and Collaboration Principles

This document applies to the entire Seafile Android client repository. If a subdirectory contains a more specific `AGENTS.md`, also adhere to its conventions when modifying that directory.

- Read relevant implementations, callers, and tests before making modifications; base decisions on the current source code, Gradle configurations, and CI, rather than blindly following potentially outdated README instructions.
- Keep changes focused; do not opportunistically migrate architectures, upgrade dependencies, format unrelated files, or bump version numbers.
- Check workspace status first to preserve any existing user modifications; do not commit, push, or execute destructive Git operations without explicit instructions.
- Briefly describe changes, tests actually performed, and results upon delivery; clearly state unrun or environment-blocked verifications without claiming they passed.

## Project Overview

- Single Android application module: `settings.gradle` includes only `:app`.
- Main package and release application ID: `com.seafile.seadroid2`; debug application ID has the `.debug` suffix.
- Business logic is primarily Java with a small amount of Kotlin; UI uses Activity/Fragment, XML, ViewBinding, and ViewModel/LiveData (this is not a Compose project).
- Networking uses Retrofit, OkHttp, and RxJava 2; database includes Room and legacy SQLiteOpenHelper; preferences storage includes SharedPreferences.
- C++17/JNI provides HEIC/HEIF, JPEG, and motion photo capabilities.

## Directory Navigation

| Path | Purpose |
| --- | --- |
| `app/src/main/java/com/seafile/seadroid2/` | Main application Java/Kotlin source code |
| `…/SeadroidApplication.java`, `…/ui/SplashActivity.java` | Application and launcher entry points |
| `…/ui/` | UI and business logic organized by feature (accounts, libraries, files, photo/folder backup, transfers, etc.) |
| `…/baseviewmodel/` | Shared ViewModel logic and RxJava lifecycle management |
| `…/framework/http/` | Retrofit/OkHttp clients, authentication, and request adapters |
| `…/framework/db/` | Room database, entities, DAOs, and migrations |
| `…/framework/service/`, `…/framework/worker/` | Transfers, scanning, backup, and background scheduling |
| `…/account/`, `…/provider/`, `…/ssl/` | System accounts, document/content providers, certificate trust |
| `…/framework/motionphoto/`, `…/jni/HeicNative.java` | Motion photo handling and JNI facade |
| `app/src/main/res/`, `app/src/debug/res/` | Main resources and debug resource overrides |
| `app/src/main/cpp/` | JNI implementations, CMake configuration, and prebuilt dependencies |
| `app/src/test/java/` | JUnit 4 / Robolectric local tests |
| `app/src/androidTest/java/` | On-device test source code |
| `script/verify_motion_photo.sh` | Helper script for motion photo structure verification and video extraction |
| `build_libheif.sh` | Script to rebuild third-party native dependencies |
| `.github/workflows/build-debug-apk.yml` | Current GitHub Actions debug APK build workflow |

In the table above, `…/` denotes `app/src/main/java/com/seafile/seadroid2/`.

## Build Environment and Prerequisites

Current configuration is as follows; re-check corresponding files whenever toolchain changes occur instead of relying solely on this section:

- JDK 17; Java source/target and Kotlin JVM target are all 17.
- Gradle Wrapper is configured to 8.13, Android Gradle Plugin is 8.13.0, and Kotlin plugin is 2.0.0.
- Android `compileSdk` / `targetSdk` is 36, `minSdk` is 28; some version comments in the root build scripts might be outdated, so verify actual numerical values.
- CMake version is 3.22.1. Requires Android SDK, NDK, and prebuilt libraries for the current ABI; the NDK version is not explicitly pinned in the app's Gradle config.
- Supported ABIs are `arm64-v8a`, `armeabi-v7a`, `x86_64`; the `ANDROID_PLATFORM` CMake parameter is currently `android-34`—do not confuse this with the application `minSdk`.
- SDK path is configured via the local environment or untracked `local.properties`; do not commit machine-specific paths.

### Special Signing Configuration Requirements

`app/build.gradle` reads `app/key.properties` directly when configuring `signingConfigs.release`. Therefore, even when running debug builds or tests, this file must exist with valid properties.

- If the file is missing locally, create it following `app/key.properties.example`; CI also copies this example before building the debug APK. Do not overwrite existing configurations.
- The example points to `debug.keystore`; for release builds, an actual usable keystore must be configured manually—do not treat the example as a formal release signing configuration.
- Do not expose signing passwords, private keys, account tokens, or other sensitive information in task logs, patches, or documentation.
- When encountering toolchain, signing, SDK/NDK, or dependency download errors, document the specific blocker; do not arbitrarily alter build versions, disable security measures, or remove native integration just to get a build to pass.

## Common Verification Commands

The following commands are executed from the repository root and require satisfying the environment and signing configuration prerequisites above. They are optional verification entry points and are not guaranteed to succeed in all environments.

```bash
# Build debug APK; aligns with the primary build step in current CI
./gradlew :app:assembleDebug --no-daemon --stacktrace

# Run local unit tests (including Robolectric tests)
./gradlew :app:testDebugUnitTest

# Android Lint
./gradlew :app:lintDebug

# Instrumented tests on device/emulator (requires a connected device)
./gradlew :app:connectedDebugAndroidTest
```

- Debug APK output directory is `app/build/outputs/apk/debug/`, currently named `seafile-debug-<versionName>.apk`.
- Current lint configuration sets `abortOnError false` and disables `MissingTranslation`; successful command exit does not mean there are no lint issues, so inspect the reports.
- `ConfirmCredentialsIntegrationTest` is currently an empty class; running instrumented tests cannot be assumed to cover the credentials integration flow.
- Choose verification according to the scope of changes: prioritize unit tests for business logic; complement with debug build and lint for resources/Manifest/dependency changes; device verification is required for UI, system permissions, background execution, and JNI behaviors.
- Pure documentation changes typically only require verifying paths, configs, and diffs—building an APK is unnecessary.
- Do not run release builds by default: they require valid signing, and assembling a release APK updates `build_count.txt` in the root directory and renames output artifacts.

## Coding Standards

### Language, Comments, and Logging

- All comments, log messages, and printed output must be written in English. This includes Java/Kotlin/C++ comments, Javadoc/KDoc, XML comments, Gradle/shell script comments, exception messages authored by the application, and console/debug output.
- Keep user-facing text in localized Android string resources as described under UI and Internationalization; the English-only rule for diagnostic output does not replace UI localization.
- Comments should explain intent, constraints, or non-obvious decisions rather than restating code. Keep them accurate when changing the implementation; do not leave commented-out code behind.
- Use the existing project logging mechanism and appropriate log levels; avoid `System.out.println`, `printStackTrace`, or temporary debug prints in application code. Include useful operation context without logging passwords, tokens, authorization headers, or sensitive user data.

### Naming and Formatting

- Use descriptive English identifiers: `UpperCamelCase` for classes/interfaces, `lowerCamelCase` for methods/variables, and `UPPER_SNAKE_CASE` for constants. Follow existing Android resource naming with `lower_snake_case`.
- Match the enclosing file's formatting, indentation, brace placement, and import order. Avoid wildcard imports, unused imports, trailing whitespace, and unrelated formatting changes.
- Replace non-obvious magic numbers or repeated protocol values with meaningfully named constants. Prefer existing constants and resources over introducing duplicates.

### Implementation and Resource Safety

- Keep methods focused and control flow readable; prefer guard clauses to unnecessary nesting. Reuse existing helpers, but avoid speculative abstractions or unrelated refactoring.
- Make nullability and ownership explicit at API boundaries. Use existing nullability annotations where applicable, check nullable external results, and do not suppress warnings or force non-null values merely to silence tooling.
- Catch specific exceptions where recovery or useful context is possible; never silently swallow failures. Preserve the original cause when wrapping exceptions and retain cancellation/interruption semantics in background work.
- Close owned streams, cursors, and other closeable resources deterministically using Java try-with-resources or Kotlin `use` where applicable. Release listeners, callbacks, and subscriptions according to the owning component's lifecycle.
- Keep mutable state as narrowly scoped as possible; prefer `final`/`val` for references that do not change. Use the existing synchronization or executor model for shared state rather than introducing unsynchronized cross-thread access.
- Add or update focused regression tests for changed behavior and bug fixes. Keep tests deterministic, avoid arbitrary sleeps and real network dependencies, and cover relevant failure, cancellation, and boundary cases.
- Apply these standards to new or modified code; do not bulk-rewrite unrelated legacy or third-party code solely for compliance.

## Code Modification Conventions

### Style and Architecture

- Follow the Java/Kotlin style of the enclosing file, typically 4-space indentation. New code should integrate into existing feature packages rather than introducing new architectures or async frameworks for localized needs.
- Classes named `*Service` in feature packages may be Retrofit API interfaces, not necessarily Android Services; check declarations and call sites first.
- ViewModels may directly access the network layer and DAOs without assuming a unified Repository layer exists. `DataStoreManager` currently mainly uses SharedPreferences; do not alter it assuming Jetpack DataStore solely based on its name.
- Do not perform time-consuming network, disk, or image processing on the main thread; follow existing RxJava scheduling and subscription disposal mechanisms (e.g., in `BaseViewModel`) to prevent lifecycle leaks and callbacks to destroyed UI.

### Networking, Accounts, and Storage

- Reuse clients, authentication interceptors, and certificate trust logic in `framework/http`; do not build alternate request pathways that bypass authentication or TLS verification.
- When modifying login, logout, account switching, or multi-account tasks, check `HttpManager` client cache invalidation and ensure tokens, server URLs, caches, and background tasks are properly associated with the correct account.
- When modifying Room entities or SQL, verify the `AppDatabase` version and explicit migrations; preserve user data rather than replacing migrations with destructive rebuilding. The legacy SQLiteOpenHelper also still exists, so verify actual data sources.
- For file operations, be mindful of URI/ContentResolver, system document providers, and Android storage permissions; do not treat all URIs as local file paths.
- When modifying account types, provider authorities, or related BuildConfig fields in the Manifest, check debug/release differences to prevent conflicts between the two build types.

### Transfers and Background Tasks

- Trace `BackupThreadExecutor`, the actual scanning/transfer call chains, and corresponding tests first; do not assume all background tasks run on WorkManager. Services, SyncAdapters, JobServices, and Workers coexist, and deprecation annotations do not mean they are unused.
- Preserve existing semantics for startup deduplication, request merging, cancellation, queuing, notification cleanup, and file stability checks. Camera photo backup running concurrently with duplicate start requests has different handling policies than folder backup append requests; do not arbitrarily unify them.
- When modifying upload, download, backup, or document writing behavior, add corresponding regression tests; changes involving system background restrictions, permissions, or notifications must be verified on target Android versions.

### UI and Internationalization

- Place user-visible text into string resources; default `res/values/strings.xml` serves as the English source text; existing Transifex configuration is in `.tx/config`.
- Preserve resource keys, format placeholders, plural forms, and `translatable="false"` flags; preference keys and default values in `donottranslate_prefs.xml` are not standard translatable strings.
- Do not bulk-rewrite all language files for a single string change; maintain only translations relevant to the task and follow the established Transifex workflow.
- When modifying layouts, colors, or themes, check dark mode, `layout-sw600dp`, and debug overrides; icon font JSON mappings and TTF files must be maintained together.

## Native and Motion Photos

- Java facade is `jni/HeicNative.java`, implemented in `app/src/main/cpp/heicgen.cpp`; JNI exported symbols contain the full package/class name, so verify signatures when renaming or modifying them.
- Retain existing fallback behavior when native libraries are missing; prioritize file-writing APIs for large video extraction to avoid loading entire videos into Java `byte[]`.
- CMake imports `libheif.so` and `libjpeg.so` from `app/src/main/cpp/libheif/lib/<ABI>/`; check all enabled ABIs when modifying native dependencies, not just the local development device's ABI.
- Preserve the existing 16 KB page-size link alignment setting in CMake; when updating prebuilt libraries, verify their compatibility as well—do not assume the whole APK is compatible based solely on the JNI library's link options.
- Routine Java/UI changes do not require rebuilding third-party native dependencies. If rebuilding is genuinely needed, first review the platform, toolchain, and ABI settings in `build_libheif.sh`; this script downloads/compiles dependencies and overwrites library files, so it should not be executed casually.
- `libheif-android-build/` is an ignored third-party build workspace, not an entry point for regular business code modifications.

Motion photo structure can be verified with:

```bash
bash script/verify_motion_photo.sh "/path/to/photo.jpg" -n
```

`-n` means inspect only without extracting; exit code `0` indicates a motion photo, `2` indicates not a motion photo, and `1` indicates an argument or file error. This script cannot replace on-device testing for Android JNI, preview, or upload flows.

## Repository Hygiene and Delivery

- Do not commit `local.properties`, `app/key.properties`, keystores, tokens, or other local credentials; do not remove corresponding ignore rules for build convenience.
- Generated content that should not be manually modified or included in routine patches includes: `.gradle/`, `.kotlin/`, `app/build/`, `app/.cxx/`, `app/release/`, `build_count.txt`, and third-party build caches.
- Prebuilt `.so` files, third-party source code, and licenses should only be modified when a task explicitly involves native dependencies; check `THIRD_PARTY_LICENSE.md` when altering dependencies.
- Inspect diffs before delivering to ensure no unintended generated files, sensitive information, unnecessary reformatting, or overwritten user changes exist.
