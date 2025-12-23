## For more information about using CMake with Android Studio, read the
## documentation: https://d.android.com/studio/projects/add-native-code.html.
## For more examples on how to use CMake, see https://github.com/android/ndk-samples.
#
## Sets the minimum CMake version required for this project.
#cmake_minimum_required(VERSION 3.22.1)
#
## Declares the project name.
#project("heicgen")
#
## Specify C++ standard
#set(CMAKE_CXX_STANDARD 17)
#set(CMAKE_CXX_STANDARD_REQUIRED ON)
#
## Set visibility to hidden to avoid exporting unnecessary symbols
#set(CMAKE_CXX_VISIBILITY_PRESET hidden)
#set(CMAKE_VISIBILITY_INLINES_HIDDEN ON)
#
## Define library path for convenience
#set(LIB_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libheif/lib/${ANDROID_ABI}")
#set(INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libheif/include")
#
## Add libheif (prebuilt)
#add_library(heif SHARED IMPORTED)
#set_target_properties(heif PROPERTIES IMPORTED_LOCATION "${LIB_DIR}/libheif.so")
#
#add_library(de265 SHARED IMPORTED)
#set_target_properties(de265 PROPERTIES IMPORTED_LOCATION "${LIB_DIR}/libde265.so")
#
#add_library(x265 SHARED IMPORTED)
#set_target_properties(x265 PROPERTIES IMPORTED_LOCATION "${LIB_DIR}/libx265.so")
#
## Add libjpeg-turbo (prebuilt)
#add_library(jpeg SHARED IMPORTED)
#set_target_properties(jpeg PROPERTIES IMPORTED_LOCATION "${LIB_DIR}/libjpeg.so")
#
#add_library(${CMAKE_PROJECT_NAME} SHARED
#        # List C/C++ source files with relative paths to this CMakeLists.txt.
#        heicgen.cpp)
#
## Specifies libraries CMake should link to your target library.
#target_link_libraries(${CMAKE_PROJECT_NAME}
#        # Android System Libraries
#        android
#        log
#        jnigraphics
#
#        # Third-party Libraries (Order matters: dependents first)
#        jpeg
#
#        # libheif (depends on de265, x265)
#        de265
#        x265
#        heif
#
#        # Standard Libraries
#        atomic
#        m
#)
#
#target_include_directories(${CMAKE_PROJECT_NAME} PRIVATE "${INCLUDE_DIR}")
#
