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

## Develop in IntelliJ/Eclipse
For those who are using maven build structures, checkout the project from [maven](https://github.com/haiwen/seadroid/tree/maven) branch.

## Internationalization

### Contribute your translation

Please submit translations via Transifex: https://www.transifex.com/haiwen/seadroid/

Steps:

1. Create a free account on Transifex (https://www.transifex.com/).
2. Send a request to join the language translation.
3. After accepted by the project maintainer, then you can upload your file or translate online.
