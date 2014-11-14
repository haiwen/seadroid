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

## License

    Copyright (c) 2013-2014 Seafile ltd.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

### Third party Licenses
Both the source and binary distributions of this software contain
some third party software. All the third party software included
or linked is redistributed under the terms and conditions of their 
original licenses. These licenses are compatible the GPL license 
that govern this software, for the purposes they are being used.

The third party software included and used by this project is:

 * ActionBarSherlock, master branch.  
   Copyright (c) 2012 Jake Wharton.  
   Licensed under Apache License, Version 2.0.  
   The official repository is referenced as a external library in the  
   Seadroid repository.  
   A binary JAR file must be generated from this referenced project  
   and included in the Seadroid APK.  
   See http://http://actionbarsherlock.com/

 * NewQuickAction, master branch  
   Copyright (c) 2011 Lorensius W. L. T  
   Licensed under Apache License, Version 2.0.  
   The official repository is referenced as a external library in the  
   Seadroid repository.  
   A binary JAR file must be generated from this referenced project  
   and included in the Seadroid APK.  
   See http://www.londatiga.net/it/how-to-create-quickaction-dialog-in-android/  

 * Android-ViewPagerIndicator, master branch  
   Copyright (c) 2012 Jake Wharton  
   Copyright (c) 2011 Patrik Åkerfeldt  
   Copyright (c) 2011 Francisco Figueiredo Jr.  
   Licensed under Apache License, Version 2.0.  
   The official repository is referenced as a external library in the  
   Seadroid repository.  
   A binary JAR file must be generated from this referenced project  
   and included in the Seadroid APK.  
   See http://viewpagerindicator.com

 * Universal Image Loader 1.9.3.  
   Copyright (c) 2011-2014 Sergey Tarasevich.  
   Licensed under the Apache License, Version 2.0.  
   Placed at libs/universal-image-loader-1.9.3.jar.  
   The jar file must be included in the Seafile APK.  
   See https://github.com/nostra13/Android-Universal-Image-Loader

