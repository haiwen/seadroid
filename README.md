# Seafile Android Client [![Build Status](https://secure.travis-ci.org/haiwen/seadroid.png?branch=master)](http://travis-ci.org/haiwen/seadroid)

## Build the APK

* Make sure you have installed [Maven](http://maven.apache.org/) 3.1.1+
* Build the apk by:

```
mvn clean package
```

You will get `target/seadroid.apk` after the build finishes.

## Develop in Eclipse

### Android Dependencies

* [ActionBarSherlock](https://github.com/JakeWharton/ActionBarSherlock)
* [NewQuickAction](https://github.com/haiwen/NewQuickAction)
* [ViewPagerIndicator](https://github.com/JakeWharton/Android-ViewPagerIndicator)

### Build

- Download `ActionBarSherlock` 4.4.0 from http://actionbarsherlock.com/download.html
- Download `ViewPagerIndicator` 2.4.1 from http://viewpagerindicator.com

- Git clone `NewQuickAction`

        git clone https://github.com/haiwen/NewQuickAction

- Add ActionBarSherlock/NewQuickAction/ViewPagerIndicator as library according to <http://developer.android.com/guide/developing/projects/projects-eclipse.html#ReferencingLibraryProject>

- Replace the android-support-v4.jar in `ActionBarSherlock` and `ViewPagerIndicator` with the jar in seadroid to make sure that all versions of this library be the same at this time.

- Download these JARs to `seadroid/libs` directory (check pom.xml to verify versions):
    - [http-request-5.6.jar](http://mvnrepository.com/artifact/com.github.kevinsawicki/http-request/5.6)
    - [commons-io-2.4.jar](http://repo1.maven.org/maven2/commons-io/commons-io/2.4/commons-io-2.4.jar)
    - [guava-17.0.jar](http://search.maven.org/remotecontent?filepath=com/google/guava/guava/17.0/guava-17.0.jar)
    - [universal-image-loader-1.9.3.jar](https://raw.githubusercontent.com/nostra13/Android-Universal-Image-Loader/master/downloads/universal-image-loader-1.9.3.jar)

Now you can build seadroid in eclipse.

## Internationalization

### Contribute your translation

Please submit translations via Transifex: https://www.transifex.com/projects/p/seafile-client/

Steps:

1. Create a free account on Transifex (https://www.transifex.com/).
2. Send a request to join the language translation.
3. After accepted by the project maintainer, then you can upload your file or translate online.
