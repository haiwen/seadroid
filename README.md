# Seafile Android Client [![Build Status](https://secure.travis-ci.org/haiwen/seadroid.png?branch=master)](http://travis-ci.org/haiwen/seadroid)

## Build the APK

* cd into seadroid directory
* Build the apk with:

```
./gradlew assembleRelease
```

You will get `app/build/outputs/Seadroid-release-*.apk` after the build finishes.

## Sign the APK

Uncomment the following lines:

In `app/build.gradle`:

    buildTypes {
         release {
             //signingConfig signingConfigs.release
             ...
         }
    }

    signingConfigs {
            release {
                // Signing code for manual signing
                //storeFile file(System.console().readLine("\n\$ Enter keystore path: "))
                //storePassword System.console().readPassword("\n\$ Enter keystore password: ").toString()
                //keyAlias System.console().readLine("\n\$ Enter key alias: ")
                //keyPassword System.console().readPassword("\n\$ Enter key password: ").toString()
            }
        }

Build the APK with `./gradlew assembleRelease` and enter the signing config data.

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