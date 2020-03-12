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
