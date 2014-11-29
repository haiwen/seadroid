# Seafile Android Client [![Build Status](https://secure.travis-ci.org/haiwen/seadroid.png?branch=master)](http://travis-ci.org/haiwen/seadroid)

## Build the APK

* cd into seadroid directory
* Create `key.properties` file or copy `key.properties.example` in `app` dir
* Create keystore file if you don't have one `keytool -genkey -v -keystore app/debug.keystore -alias AndroidDebugKey -keyalg RSA -keysize 2048 -validity 1 -storepass android -keypass android -dname "cn=TEST, ou=TEST, o=TEST, c=TE"`
* Build with `./gradlew assembleRelease`

You will get `app/build/outputs/Seadroid-release-*.apk` after the build finishes.

## Sign the APK

Change `key.properties` to match your keystore file.

Build the APK with `./gradlew assembleRelease`.

## Develop in Android Studio / IntelliJ

### Prerequisites

* Android Studio 0.9 or IntelliJ 14
* OpenJDK 7 / OracleJDK 7

### Import project

* Open Android Studio / IntelliJ
* Import project
* Select seafile directory
* Choose import from gradle
* Click next until import is completed

## Internationalization

### Contribute your translation

Please submit translations via Transifex: https://www.transifex.com/projects/p/seafile-client/

Steps:

1. Create a free account on Transifex (https://www.transifex.com/).
2. Send a request to join the language translation.
3. After accepted by the project maintainer, then you can upload your file or translate online.