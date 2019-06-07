# Seafile Android App [![Build Status](https://img.shields.io/travis/haiwen/seadroid/master.svg)](http://travis-ci.org/haiwen/seadroid) ![F-Droid](https://img.shields.io/f-droid/v/com.seafile.seadroid2.svg)

This app has been published on Google Play and F-Droid.

<a href="https://play.google.com/store/apps/details?id=com.seafile.seadroid2">
    <img src="https://play.google.com/intl/en_us/badges/images/generic/en_badge_web_generic.png"
         alt="Get it on Google Play" height="80">
</a>

<a href="https://f-droid.org/app/com.seafile.seadroid2">
    <img src="https://f-droid.org/badge/get-it-on.png"
         alt="Get it on F-Droid" height="80">
</a>

## Contributors

See [Contributors Graph](https://github.com/haiwen/seadroid/graphs/contributors)

## Build the project

1. First, make sure you have installed the [Android SDK](http://developer.android.com/sdk/index.html) (Hint: In case you're familiar with docker it might be worth looking at Docker Hub and build the app within an container to not have to install the SDK locally. There is e.g. `runmymind/docker-android-sdk:ubuntu-standalone`).
2. Go into the seadroid directory.
3. Create `app/key.properties` or simply rename `app/key.properties.example`. Adjust the configuration if you want to use a specific keystore.
4. Create keystore file if you don't already have one
```bash
keytool -genkey -v -keystore app/debug.keystore -alias AndroidDebugKey -keyalg RSA -keysize 2048 -validity 1 -storepass android -keypass android -dname "cn=TEST, ou=TEST, o=TEST, c=TE"
 ```
5. Build with `./gradlew assembleRelease`
6. The apk will be located at `app/build/outputs/apk/release/seafile-${versionName}.apk` after the build finishes.

## Develop in Android Studio

### Prerequisites

* Android Studio
* OpenJDK 8 / OracleJDK 8

### Import project

* Open Android Studio
* Import project
* Select seadroid directory
* Choose import from gradle
* Click next until import is completed

## Internationalization

### Contribute your translation

Please submit translations via Transifex: https://www.transifex.com/haiwen/seadroid/

Steps:

1. Create a free account on Transifex (https://www.transifex.com/).
2. Send a request to join the language translation.
3. After accepted by the project maintainer, you can upload translations or translate online.
