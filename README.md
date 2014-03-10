# Seafile Android Client

## Android Dependencies

* [ActionBarSherlock](https://github.com/JakeWharton/ActionBarSherlock)
* [NewQuickAction](https://github.com/haiwen/NewQuickAction)
* [ViewPagerIndicator](https://github.com/JakeWharton/Android-ViewPagerIndicator)

## Build

- Download `ActionBarSherlock` 4.2.0 from http://actionbarsherlock.com/download.html
- Download `ViewPagerIndicator` 2.4.1 from http://viewpagerindicator.com

- Git clone `NewQuickAction`, checkout commit `89f7f1ee572996787096992df5c2991c406d6967`

        git clone https://github.com/xgouchet/NewQuickAction
        git checkout 89f7f1ee572996787096992df5c2991c406d6967

- Add ActionBarSherlock/NewQuickAction/ViewPagerIndicator as library according to <http://developer.android.com/guide/developing/projects/projects-eclipse.html#ReferencingLibraryProject>

- Replace the android-support-v4.jar in `ActionBarSherlock` and `ViewPagerIndicator` with the jar in seadroid to make sure that all versions of this library be the same at this time.

- Download these JARs to `seadroid/libs` directory:  
    - [http-request-5.3.jar](http://mvnrepository.com/artifact/com.github.kevinsawicki/http-request/5.3)
    - [commons-io-2.4.jar](http://repo1.maven.org/maven2/commons-io/commons-io/2.4/commons-io-2.4.jar)
    - [guava-16.0.1.jar](http://search.maven.org/remotecontent?filepath=com/google/guava/guava/16.0.1/guava-16.0.1.jar)

Now you can build seadroid in eclipse.
