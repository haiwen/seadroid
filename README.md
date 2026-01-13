# Seafile Android Client [![Build Status](https://secure.travis-ci.org/haiwen/seadroid.png?branch=master)](http://travis-ci.org/haiwen/seadroid)

The application has been published onto the market for easy access:

[![Get it on Google Play](https://play.google.com/intl/en_us/badges/images/badge_new.png)](https://play.google.com/store/apps/details?id=com.seafile.seadroid2)
[![Get it on F-Droid](https://cloud.githubusercontent.com/assets/12447257/8024903/ce8dca32-0d44-11e5-95b0-e97d1d027351.png)](https://f-droid.org/repository/browse/?fdid=com.seafile.seadroid2)

## Contributors

See [Contributors Graph](https://github.com/haiwen/seadroid/graphs/contributors)

## Build the APK

* Make sure you have installed the [Android SDK](http://developer.android.com/sdk/index.html) then:

* cd into seadroid directory
* Create `key.properties` file or simply rename `key.properties.example` and change configurations to match yours.

* Create keystore file if you don't have one

 ```
 keytool -genkey -v -keystore app/debug.keystore -alias AndroidDebugKey -keyalg RSA -keysize 2048 -validity 1 -storepass android -keypass android -dname "cn=TEST, ou=TEST, o=TEST, c=TE"
 ```
* Build with `./gradlew assembleRelease`

You will get `app/build/outputs/apk/seafile-${versionName}.apk` after the build finishes.

## Build Native Libraries

The app uses native libraries for HEIC/HEIF image format support. Prebuilt libraries are included in `app/src/main/cpp/libheif/`, but if you need to rebuild them:

### Prerequisites

**macOS only (currently)**
- macOS operating system
- Android NDK (r21 or later recommended)
- Command line tools:
  ```bash
  brew install cmake ninja pkg-config git
  brew install nasm  # Optional, for libjpeg-turbo optimization
  ```

**Environment variables** (optional, script will auto-detect):
```bash
export ANDROID_NDK_HOME=/path/to/ndk
# or
export ANDROID_NDK=/path/to/ndk
# or
export ANDROID_SDK_ROOT=/path/to/sdk
```

### Build libheif and dependencies

Run the build script:

```bash
chmod +x build_libheif.sh
./build_libheif.sh
```

This will:
1. Download and compile the following libraries for Android (armeabi-v7a, arm64-v8a):
   - **libjpeg-turbo** - JPEG image codec
   - **x265** - H.265/HEVC video encoder
   - **libde265** - H.265/HEVC video decoder
   - **libheif** v1.20.2 - HEIF/HEIC image format support
2. Copy compiled libraries to `app/src/main/cpp/libheif/lib/`
3. Strip debug symbols to reduce size
4. Keep source code in `libheif-android-build/src/` for future builds

**Note**: The build process may take 30-60 minutes depending on your system.

### Manual build configuration

To customize the build, edit these variables in `build_libheif.sh`:

```bash
# Android API level (default: 28)
export ANDROID_API=28

# Target ABIs (default: armeabi-v7a, arm64-v8a)
ABIS=(
    "armeabi-v7a"
    "arm64-v8a"
)

# LibHeif version (default: v1.20.2)
LIBHEIF_VERSION="v1.20.2"
```

### Output

After successful build, the following libraries will be available:
- `app/src/main/cpp/libheif/lib/{ABI}/libheif.so`
- `app/src/main/cpp/libheif/lib/{ABI}/libde265.so`
- `app/src/main/cpp/libheif/lib/{ABI}/libx265.so`
- `app/src/main/cpp/libheif/lib/{ABI}/libjpeg.so`

Where `{ABI}` is one of: `armeabi-v7a`, `arm64-v8a`

## Develop in Android Studio

### Prerequisites

* Android Studio
* OpenJDK 17 / OracleJDK 17

### Import project

* Open Android Studio
* Import project
* Select seadroid directory
* Choose import from gradle
* Click next until import is completed

## Develop in IntelliJ/Eclipse
For those who are using maven build structures, checkout the project from [maven](https://github.com/haiwen/seadroid/tree/maven) branch.

## Internationalization

### Contribute your translation

Please submit translations via Transifex:

Steps:

1. Visit the webpage of Transifex ([https://explore.transifex.com/haiwen/seadroid/](https://explore.transifex.com/haiwen/seadroid/)).

2. Click the "Join this project" button in the bottom right corner.

3. Use an email or GitHub account(recommended) to create an account.

4. Select a language and click 'Join project' to join the language translation.

5. After accepted by the project maintainer, then you can upload your file or translate online.
