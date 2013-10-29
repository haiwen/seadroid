# Seafile Android Client

## Dependencies

* [ActionBarSherlock](https://github.com/JakeWharton/ActionBarSherlock)
* [aFileChooser](https://github.com/iPaulPro/aFileChooser)
* [NewQuickAction](https://github.com/haiwen/NewQuickAction)
* [http-request](https://github.com/kevinsawicki/http-request)

## Build

- Download `ActionBarSherlock` 4.2.0 from http://actionbarsherlock.com/download.html
- Git clone `aFileChooser`, checkout commit `bd54d1411f2f115df38bb49c2a1e43dd26e37f36`

        git clone https://github.com/iPaulPro/aFileChooser
        git checkout bd54d1411f2f115df38bb49c2a1e43dd26e37f36

- Git clone `NewQuickAction`, checkout commit `89f7f1ee572996787096992df5c2991c406d6967`

        git clone https://github.com/xgouchet/NewQuickAction
        git checkout 89f7f1ee572996787096992df5c2991c406d6967

- Add ActionBarSherlock/aFileChhooser/NewQuickAction as library according to <http://developer.android.com/guide/developing/projects/projects-eclipse.html#ReferencingLibraryProject>

- Replace the android-support-v4.jar in `aFileChooser` with the jar in seadroid

- Download [http-request-5.3.jar](http://mvnrepository.com/artifact/com.github.kevinsawicki/http-request/5.3) to `seadroid/libs` directory.

Now you can build seadroid in eclipse.
