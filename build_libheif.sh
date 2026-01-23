#!/bin/bash
#
# LibHeif 1.20.2 Android Build Script for macOS
#
# Usage:
#   1. chmod +x build_libheif.sh
#   2. ./build_libheief.sh
#

set -e  # exit immediately when encountering an error

# ============================================
# Configuration
# ============================================

# Get the directory where the script is located (i.e. the project root)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Target project path (directory where the script is located)
export PROJECT_DIR="$SCRIPT_DIR"

# work directory relative paths within the project
export BUILD_ROOT="$PROJECT_DIR/libheif-android-build"

# Android NDK path (taken from environment variables)
# Priority: ANDROID_NDK_HOME > ANDROID_NDK > ANDROID_SDK_ROOT/ndk/the latest version
if [ -n "$ANDROID_NDK_HOME" ] && [ -d "$ANDROID_NDK_HOME" ]; then
    export ANDROID_NDK="$ANDROID_NDK_HOME"
elif [ -n "$ANDROID_NDK" ] && [ -d "$ANDROID_NDK" ]; then
    # android_ndk set left unchanged
    :
elif [ -n "$ANDROID_SDK_ROOT" ] && [ -d "$ANDROID_SDK_ROOT/ndk" ]; then
    # automatically find the latest ndk version
    NDK_VERSION=$(ls -1 "$ANDROID_SDK_ROOT/ndk" 2>/dev/null | sort -V | tail -n1)
    if [ -n "$NDK_VERSION" ]; then
        export ANDROID_NDK="$ANDROID_SDK_ROOT/ndk/$NDK_VERSION"
    fi
elif [ -n "$ANDROID_HOME" ] && [ -d "$ANDROID_HOME/ndk" ]; then
    # compatible with android_home environment variables
    NDK_VERSION=$(ls -1 "$ANDROID_HOME/ndk" 2>/dev/null | sort -V | tail -n1)
    if [ -n "$NDK_VERSION" ]; then
        export ANDROID_NDK="$ANDROID_HOME/ndk/$NDK_VERSION"
    fi
fi

# Android API Level
export ANDROID_API=28

# The schema to compile
ABIS=(
    "armeabi-v7a"
    "arm64-v8a"
#    "x86"
    "x86_64"
)

# LibHeif version
LIBHEIF_VERSION="v1.20.2"

# ============================================
# macos system configuration
# ============================================

# get the number of cpu cores
NPROC=$(sysctl -n hw.ncpu)
echo "macos system detected cpu cores: $NPROC"

# NDK strip tool prefix
STRIP_PREFIX="darwin-x86_64"

# ============================================
# color output
# ============================================

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# ============================================
# Check environment
# ============================================

check_environment() {
    log_info "Checking build environment..."

    # Check operating system
    if [[ "$OSTYPE" != "darwin"* ]]; then
        log_error "This script is only for macOS"
        log_error "Linux users please use another version of the script"
        exit 1
    fi

    # Check NDK
    if [ ! -d "$ANDROID_NDK" ]; then
        log_error "Android NDK not found: $ANDROID_NDK"
        exit 1
    fi
    log_success "Found Android NDK: $ANDROID_NDK"

    # Check project directory
    if [ ! -d "$PROJECT_DIR" ]; then
        log_error "Project directory not found: $PROJECT_DIR"
        exit 1
    fi
    log_success "Found project directory: $PROJECT_DIR"

    # Check Homebrew tools
    local tools=("cmake" "git" "ninja" "pkg-config")
    for tool in "${tools[@]}"; do
        if ! command -v $tool &> /dev/null; then
            log_error "Tool not found: $tool"
            log_error "Please install using Homebrew: brew install $tool"
            exit 1
        fi
    done
    log_success "All required tools installed"

    # Check NASM
    if ! command -v nasm &> /dev/null; then
        log_warn "NASM not found"
        log_warn "Recommended to install: brew install nasm"
    else
        log_success "Found NASM: $(which nasm)"
    fi
}

build_libjpeg() {
    log_info "=========================================="
    log_info "Starting to build libjpeg-turbo..."
    log_info "=========================================="
    cd $SRC_DIR
    if [ ! -d "libjpeg-turbo" ]; then
        log_info "Cloning libjpeg-turbo source code..."
        git clone https://github.com/libjpeg-turbo/libjpeg-turbo.git
    else
        log_info "libjpeg-turbo source code already exists, skipping download"
    fi
    cd libjpeg-turbo
    for ABI in "${ABIS[@]}"; do
        log_info "Building libjpeg-turbo for $ABI..."
        BUILD_DIR="build-$ABI"
        INSTALL_DIR="$PREFIX_DIR/libjpeg/$ABI"
        rm -rf $BUILD_DIR
        mkdir -p $BUILD_DIR
        cd $BUILD_DIR
        cmake .. \
            -G Ninja \
            -DCMAKE_TOOLCHAIN_FILE=$ANDROID_NDK/build/cmake/android.toolchain.cmake \
            -DANDROID_ABI=$ABI \
            -DANDROID_PLATFORM=android-$ANDROID_API \
            -DCMAKE_BUILD_TYPE=Release \
            -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR \
            -DENABLE_SHARED=ON \
            -DENABLE_STATIC=OFF
        ninja -j$NPROC
        ninja install
        cd ..
        log_success "libjpeg-turbo for $ABI build complete"
    done
    log_success "✅ libjpeg-turbo build complete!"
}

# ============================================
# Initialize directories
# ============================================

init_directories() {
    log_info "Initializing build directories..."

    export PREFIX_DIR="$BUILD_ROOT/prefix"
    export SRC_DIR="$BUILD_ROOT/src"

    mkdir -p $PREFIX_DIR
    mkdir -p $SRC_DIR

    log_success "Build directories created: $BUILD_ROOT"
}

# ============================================
# Build x265
# ============================================

build_x265() {
    log_info "=========================================="
    log_info "Starting to build x265..."
    log_info "=========================================="

    cd $SRC_DIR

    # Download x265
    if [ ! -d "x265" ]; then
        log_info "Cloning x265 source code..."
        git clone https://bitbucket.org/multicoreware/x265_git.git x265
    else
        log_info "x265 source code already exists, skipping download"
    fi

    # Patch x265 CMakeLists.txt
    log_info "Patching x265 CMakeLists.txt..."
    if [ -f "x265/source/CMakeLists.txt" ]; then
         # Fix unknown processor armv7-a
         sed -i '' 's/set(ARM_ALIASES armv6l armv7l)/set(ARM_ALIASES armv6l armv7l armv7-a)/g' x265/source/CMakeLists.txt
         # Fix pthread linking on Android
          sed -i '' 's/list(APPEND PLATFORM_LIBS pthread)/#list(APPEND PLATFORM_LIBS pthread)/g' x265/source/CMakeLists.txt
          # Fix unsupported flags for Android ARM build
          sed -i '' 's/-mcpu=native -mfloat-abi=hard//g' x265/source/CMakeLists.txt
     fi

    cd x265/build

    # Build for each architecture
    for ABI in "${ABIS[@]}"; do
        log_info "Building x265 for $ABI..."

        BUILD_DIR="android-$ABI"
        INSTALL_DIR="$PREFIX_DIR/x265/$ABI"

        rm -rf $BUILD_DIR
        mkdir -p $BUILD_DIR
        cd $BUILD_DIR

        cmake ../../source \
            -G Ninja \
            -DCMAKE_TOOLCHAIN_FILE=$ANDROID_NDK/build/cmake/android.toolchain.cmake \
            -DANDROID_ABI=$ABI \
            -DANDROID_PLATFORM=android-$ANDROID_API \
            -DCMAKE_BUILD_TYPE=Release \
            -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR \
            -DENABLE_SHARED=ON \
            -DENABLE_CLI=OFF \
            -DENABLE_ASSEMBLY=OFF \
            -DHIGH_BIT_DEPTH=OFF \
            -DENABLE_PTHREADS=OFF \
            -DTHREADS=OFF \
            -DENABLE_PIC=ON \
            -DENABLE_LIBNUMA=OFF \
            -DCMAKE_CXX_FLAGS="-DANDROID -fPIC" \
            -DCMAKE_C_FLAGS="-DANDROID -fPIC"

        ninja -j$NPROC
        ninja install

        cd ..

        log_success "x265 for $ABI build complete"
    done

    log_success "✅ x265 build complete!"
}

# ============================================
# Build libde265
# ============================================

build_libde265() {
    log_info "=========================================="
    log_info "Starting to build libde265..."
    log_info "=========================================="

    cd $SRC_DIR

    # Download libde265
    if [ ! -d "libde265" ]; then
        log_info "Cloning libde265 source code..."
        git clone https://github.com/strukturag/libde265.git
    else
        log_info "libde265 source code already exists, skipping download"
    fi

    cd libde265

    # Build for each architecture
    for ABI in "${ABIS[@]}"; do
        log_info "Building libde265 for $ABI..."

        BUILD_DIR="build-$ABI"
        INSTALL_DIR="$PREFIX_DIR/libde265/$ABI"

        rm -rf $BUILD_DIR
        mkdir -p $BUILD_DIR
        cd $BUILD_DIR

        cmake .. \
            -G Ninja \
            -DCMAKE_TOOLCHAIN_FILE=$ANDROID_NDK/build/cmake/android.toolchain.cmake \
            -DANDROID_ABI=$ABI \
            -DANDROID_PLATFORM=android-$ANDROID_API \
            -DCMAKE_BUILD_TYPE=Release \
            -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR \
            -DBUILD_SHARED_LIBS=ON \
            -DENABLE_SDL=OFF \
            -DENABLE_DECODER=ON

        ninja -j$NPROC
        ninja install

        cd ..

        log_success "libde265 for $ABI build complete"
    done

    log_success "✅ libde265 build complete!"
}

# ============================================
# build Bento4 (not use)
# ============================================
build_bento4() {
    log_info "=========================================="
    log_info "Starting to build Bento4..."
    log_info "=========================================="

    cd $SRC_DIR

    # Download Bento4
    if [ ! -d "Bento4" ]; then
        log_info "Cloning Bento4 source code..."
        git clone https://github.com/axiomatic-systems/Bento4.git
    else
        log_info "Bento4 source code already exists, skipping download"
    fi

    cd Bento4

    # Bento4 doesn't provide CMake install support, we need to manually compile source files
    # Create a simple CMakeLists.txt for compilation

    cat > CMakeLists.txt <<EOF
cmake_minimum_required(VERSION 3.10)
project(ap4)

file(GLOB CORE_SOURCES "Source/C++/Core/*.cpp")
file(GLOB CODEC_SOURCES "Source/C++/Codecs/*.cpp")
file(GLOB METADATA_SOURCES "Source/C++/MetaData/*.cpp")
file(GLOB CRYPTO_SOURCES "Source/C++/Crypto/*.cpp")

# Android System Sources
set(SYSTEM_SOURCES
    "Source/C++/System/StdC/Ap4StdCFileByteStream.cpp"
    "Source/C++/System/Posix/Ap4PosixRandom.cpp"
)

add_library(ap4 SHARED
    \${CORE_SOURCES}
    \${CODEC_SOURCES}
    \${METADATA_SOURCES}
    \${CRYPTO_SOURCES}
    \${SYSTEM_SOURCES}
)

target_include_directories(ap4 PUBLIC
    Source/C++/Core
    Source/C++/Codecs
    Source/C++/MetaData
    Source/C++/Crypto
)
EOF

    # Build for each architecture
    for ABI in "${ABIS[@]}"; do
        log_info "Building Bento4 for $ABI..."

        BUILD_DIR="cmake-build-$ABI"
        INSTALL_DIR="$PREFIX_DIR/bento4/$ABI"

        rm -rf $BUILD_DIR
        mkdir -p $BUILD_DIR
        cd $BUILD_DIR

        cmake .. \
            -G Ninja \
            -DCMAKE_TOOLCHAIN_FILE=$ANDROID_NDK/build/cmake/android.toolchain.cmake \
            -DANDROID_ABI=$ABI \
            -DANDROID_PLATFORM=android-$ANDROID_API \
            -DCMAKE_BUILD_TYPE=Release

        ninja -j$NPROC

        # Manual installation
        mkdir -p $INSTALL_DIR/lib
        mkdir -p $INSTALL_DIR/include/Bento4

        cp libap4.so $INSTALL_DIR/lib/
        cp -r ../Source/C++/Core/*.h $INSTALL_DIR/include/Bento4/
        cp -r ../Source/C++/Codecs/*.h $INSTALL_DIR/include/Bento4/
        cp -r ../Source/C++/MetaData/*.h $INSTALL_DIR/include/Bento4/
        cp -r ../Source/C++/Crypto/*.h $INSTALL_DIR/include/Bento4/

        cd ..

        log_success "Bento4 for $ABI build complete"
    done

    log_success "✅ Bento4 build complete!"
}

# ============================================
# build ffmpeg (not use)
# ============================================

build_ffmpeg() {
    log_info "=========================================="
    log_info "Starting to build ffmpeg..."
    log_info "=========================================="

    cd $SRC_DIR

    # Download ffmpeg
    if [ ! -d "ffmpeg" ]; then
        log_info "Cloning ffmpeg source code..."
        if ! git clone --depth 1 --branch n6.1 https://github.com/FFmpeg/FFmpeg.git ffmpeg; then
            log_warn "GitHub clone failed, trying official mirror"
            git clone --depth 1 --branch n6.1 https://git.ffmpeg.org/ffmpeg.git ffmpeg
        fi
    else
        log_info "ffmpeg source code already exists, skipping download"
    fi

    cd ffmpeg

    for ABI in "${ABIS[@]}"; do
        log_info "build ffmpeg for $ABI..."

        BUILD_DIR="build-$ABI"
        INSTALL_DIR="$PREFIX_DIR/ffmpeg/$ABI"

        rm -rf $BUILD_DIR
        mkdir -p $BUILD_DIR
        cd $BUILD_DIR

        # Set cross-compilation toolchain
        case $ABI in
            "armeabi-v7a")
                ARCH="arm"
                CPU="armv7-a"
                CROSS_PREFIX="$ANDROID_NDK/toolchains/llvm/prebuilt/$STRIP_PREFIX/bin/armv7a-linux-androideabi$ANDROID_API-"
                ;;
            "arm64-v8a")
                ARCH="arm64"
                CPU="armv8-a"
                CROSS_PREFIX="$ANDROID_NDK/toolchains/llvm/prebuilt/$STRIP_PREFIX/bin/aarch64-linux-android$ANDROID_API-"
                ;;
            "x86")
                ARCH="x86"
                CPU="i686"
                CROSS_PREFIX="$ANDROID_NDK/toolchains/llvm/prebuilt/$STRIP_PREFIX/bin/i686-linux-android$ANDROID_API-"
                ;;
        esac

        SYSROOT="$ANDROID_NDK/toolchains/llvm/prebuilt/$STRIP_PREFIX/sysroot"

        # x86 architecture needs to disable assembly optimization to avoid PIC linking errors
        EXTRA_FLAGS=""
        if [ "$ABI" = "x86" ]; then
            EXTRA_FLAGS="--disable-asm"
            log_info "x86 architecture: disabling assembly optimization"
        fi

        # Configure ffmpeg
        ../configure \
            --prefix=$INSTALL_DIR \
            --enable-cross-compile \
            --cross-prefix=$CROSS_PREFIX \
            --target-os=android \
            --arch=$ARCH \
            --cpu=$CPU \
            --sysroot=$SYSROOT \
            --cc=${CROSS_PREFIX}clang \
            --cxx=${CROSS_PREFIX}clang++ \
            --extra-cflags="-fPIC -O3" \
            --enable-pic \
            --enable-shared \
            --disable-static \
            --disable-doc \
            --disable-autodetect \
            --disable-programs \
            --disable-avdevice \
            --disable-postproc \
            --disable-avfilter \
            --disable-network \
            --disable-debug \
            --disable-stripping \
            --disable-vulkan \
            --disable-hwaccels \
            $EXTRA_FLAGS

        make -j$NPROC
        make install

        cd ..
        log_success "ffmpeg for $ABI build complete"
    done

    log_success "✅ ffmpeg build complete!"
}

# ============================================
# Build libheif
# ============================================

build_libheif() {
    log_info "=========================================="
    log_info "build libheif $LIBHEIF_VERSION..."
    log_info "=========================================="

    cd $SRC_DIR

    # Download libheif
    if [ ! -d "libheif" ]; then
        log_info "clone libheif source code..."
        git clone https://github.com/strukturag/libheif.git
    else
        log_info "libheif source code already exists, skipping download"
    fi

    cd libheif
    git fetch --tags
    git checkout $LIBHEIF_VERSION

    # Build for each architecture
    for ABI in "${ABIS[@]}"; do
        log_info "build libheif for $ABI..."

        BUILD_DIR="build-$ABI"
        INSTALL_DIR="$PREFIX_DIR/libheif/$ABI"

        rm -rf $BUILD_DIR
        mkdir -p $BUILD_DIR
        cd $BUILD_DIR

         export PKG_CONFIG_PATH="$PREFIX_DIR/x265/$ABI/lib/pkgconfig:$PREFIX_DIR/libde265/$ABI/lib/pkgconfig"

        cmake .. \
            -G Ninja \
            -DCMAKE_TOOLCHAIN_FILE=$ANDROID_NDK/build/cmake/android.toolchain.cmake \
            -DANDROID_ABI=$ABI \
            -DANDROID_PLATFORM=android-$ANDROID_API \
            -DCMAKE_BUILD_TYPE=Release \
            -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR \
            -DBUILD_SHARED_LIBS=ON \
            -DWITH_EXAMPLES=OFF \
            -DBUILD_TESTING=OFF \
            -DWITH_LIBDE265=ON \
            -DWITH_X265=ON \
            -DWITH_AOM_DECODER=OFF \
            -DWITH_AOM_ENCODER=OFF \
            -DWITH_DAV1D=OFF \
            -DWITH_RAV1E=OFF \
            -DWITH_SvtEnc=OFF \
            -DWITH_KVAZAAR=OFF \
            -DWITH_FFMPEG_DECODER=OFF \
            -DWITH_JPEG_DECODER=ON \
            -DWITH_JPEG_ENCODER=ON \
            -DWITH_OpenJPEG_DECODER=OFF \
            -DWITH_OpenJPEG_ENCODER=OFF \
            -DWITH_OPENJPH_ENCODER=OFF \
            -DWITH_VVDEC=OFF \
            -DWITH_VVENC=OFF \
            -DWITH_UVG266=OFF \
            -DENABLE_PLUGIN_LOADING=OFF \
            -DWITH_UNCOMPRESSED_CODEC=OFF \
            -DWITH_HEADER_COMPRESSION=OFF \
            -DENABLE_MULTITHREADING_SUPPORT=ON \
            -DENABLE_PARALLEL_TILE_DECODING=ON \
             -DLIBDE265_LIBRARY=$PREFIX_DIR/libde265/$ABI/lib/libde265.so \
             -DLIBDE265_INCLUDE_DIR=$PREFIX_DIR/libde265/$ABI/include \
             -DX265_LIBRARY=$PREFIX_DIR/x265/$ABI/lib/libx265.so \
             -DX265_INCLUDE_DIR=$PREFIX_DIR/x265/$ABI/include \
             -DJPEG_LIBRARY=$PREFIX_DIR/libjpeg/$ABI/lib/libjpeg.so \
             -DJPEG_INCLUDE_DIR=$PREFIX_DIR/libjpeg/$ABI/include

        ninja -j$NPROC
        ninja install

        cd ..

        log_success "libheif for $ABI build complete"
    done

    log_success "✅ libheif $LIBHEIF_VERSION build complete"
}

# ============================================
# Copy to project
# ============================================

copy_to_project() {
    log_info "=========================================="
    log_info "copy libheif files to project..."
    log_info "=========================================="

    TARGET_DIR="$PROJECT_DIR/app/src/main/cpp/libheif"

    # Create directories
    mkdir -p $TARGET_DIR/include
    mkdir -p $TARGET_DIR/lib

    # Copy header files
    log_info "copy libheif header files..."
    if [ "$NEED_LIBHEIF" = "true" ]; then
        cp -r $PREFIX_DIR/libheif/arm64-v8a/include/libheif $TARGET_DIR/include/
    fi
    if [ "$NEED_LIBJPEG" = "true" ] || [ -d "$PREFIX_DIR/libjpeg/arm64-v8a/include" ]; then
        mkdir -p $TARGET_DIR/include/libjpeg
        cp -r $PREFIX_DIR/libjpeg/arm64-v8a/include/* $TARGET_DIR/include/libjpeg/
    fi
    # [Commented] Copy Bento4 headers - currently only building libheif and libjpeg
    # if [ "$NEED_BENTO4" = "true" ]; then
    #     mkdir -p $TARGET_DIR/include/Bento4
    #     cp -r $PREFIX_DIR/bento4/arm64-v8a/include/Bento4/* $TARGET_DIR/include/Bento4/
    # fi
    # [Commented] Copy ffmpeg headers - currently only building libheif and libjpeg
    # if [ "$NEED_FFMPEG" = "true" ] || [ -d "$PREFIX_DIR/ffmpeg/arm64-v8a/include" ]; then
    #     mkdir -p $TARGET_DIR/include/ffmpeg
    #     cp -r $PREFIX_DIR/ffmpeg/arm64-v8a/include/* $TARGET_DIR/include/ffmpeg/
    # fi

    # Copy library files
    for ABI in "${ABIS[@]}"; do
        log_info "Copying $ABI library files..."
        mkdir -p $TARGET_DIR/lib/$ABI

        # Copy libheif
        if [ "$NEED_LIBHEIF" = "true" ]; then
            cp $PREFIX_DIR/libheif/$ABI/lib/libheif.so $TARGET_DIR/lib/$ABI/
        fi

         if [ "$NEED_X265" = "true" ]; then
             cp $PREFIX_DIR/x265/$ABI/lib/libx265.so $TARGET_DIR/lib/$ABI/
         fi
         if [ "$NEED_LIBDE265" = "true" ]; then
             cp $PREFIX_DIR/libde265/$ABI/lib/libde265.so $TARGET_DIR/lib/$ABI/
         fi
        # [Commented] Copy Bento4 - currently only building libheif and libjpeg
        # if [ "$NEED_BENTO4" = "true" ]; then
        #     cp $PREFIX_DIR/bento4/$ABI/lib/libap4.so $TARGET_DIR/lib/$ABI/
        # fi

        if [ -f "$PREFIX_DIR/libjpeg/$ABI/lib/libjpeg.so" ]; then
            cp $PREFIX_DIR/libjpeg/$ABI/lib/libjpeg.so $TARGET_DIR/lib/$ABI/
        fi

        # [Commented] Copy ffmpeg library files - currently only building libheif and libjpeg
        # if [ "$NEED_FFMPEG" = "true" ] || [ -d "$PREFIX_DIR/ffmpeg/$ABI/lib" ]; then
        #     if [ -d "$PREFIX_DIR/ffmpeg/$ABI/lib" ]; then
        #         cp $PREFIX_DIR/ffmpeg/$ABI/lib/*.so $TARGET_DIR/lib/$ABI/ 2>/dev/null || true
        #     fi
        # fi

        log_success "$ABI lib files copy complete"
    done

    log_success "✅ libheif $LIBHEIF_VERSION files copy complete"
}

# ============================================
# Strip library files
# ============================================

strip_libraries() {
    log_info "=========================================="
    log_info "Strip libheif $LIBHEIF_VERSION libraries to reduce size..."
    log_info "=========================================="

    TARGET_DIR="$PROJECT_DIR/app/src/main/cpp/libheif/lib"

    for ABI in "${ABIS[@]}"; do
        log_info "Strip $ABI lib files..."

        # Select corresponding strip tool
        case $ABI in
            "armeabi-v7a")
                STRIP_TOOL="$ANDROID_NDK/toolchains/llvm/prebuilt/$STRIP_PREFIX/bin/arm-linux-androideabi-strip"
                ;;
            "arm64-v8a")
                STRIP_TOOL="$ANDROID_NDK/toolchains/llvm/prebuilt/$STRIP_PREFIX/bin/aarch64-linux-android-strip"
                ;;
            "x86")
                STRIP_TOOL="$ANDROID_NDK/toolchains/llvm/prebuilt/$STRIP_PREFIX/bin/i686-linux-android-strip"
                ;;
        esac

        if [ -f "$STRIP_TOOL" ]; then
            [ -f "$TARGET_DIR/$ABI/libheif.so" ] && $STRIP_TOOL $TARGET_DIR/$ABI/libheif.so
            [ -f "$TARGET_DIR/$ABI/libjpeg.so" ] && $STRIP_TOOL $TARGET_DIR/$ABI/libjpeg.so
            # [Commented] Currently only building libheif and libjpeg
            # [ -f "$TARGET_DIR/$ABI/libx265.so" ] && $STRIP_TOOL $TARGET_DIR/$ABI/libx265.so
            # [ -f "$TARGET_DIR/$ABI/libde265.so" ] && $STRIP_TOOL $TARGET_DIR/$ABI/libde265.so
            # [ -f "$TARGET_DIR/$ABI/libap4.so" ] && $STRIP_TOOL $TARGET_DIR/$ABI/libap4.so
            # [Commented] Strip ffmpeg library files - currently only building libheif and libjpeg
            # for ffmpeg_lib in $TARGET_DIR/$ABI/libav*.so $TARGET_DIR/$ABI/libsw*.so $TARGET_DIR/$ABI/libpostproc.so; do
            #     [ -f "$ffmpeg_lib" ] && $STRIP_TOOL "$ffmpeg_lib"
            # done
            log_success "$ABI lib files strip complete"
        else
            log_warn "Strip tool not found: $STRIP_TOOL"
        fi
    done

    log_success "✅ libheif $LIBHEIF_VERSION libraries strip complete"
}

# ============================================
# Show statistics
# ============================================

show_statistics() {
    log_info "=========================================="
    log_info "libheif $LIBHEIF_VERSION libraries statistics"
    log_info "=========================================="

    TARGET_DIR="$PROJECT_DIR/app/src/main/cpp/libheif/lib"

    for ABI in "${ABIS[@]}"; do
    if [ -d "$TARGET_DIR/$ABI" ]; then
        log_info "Architecture: $ABI"
            ls -lh $TARGET_DIR/$ABI/*.so
        echo ""
    fi
    done

    log_info "Total size:"
    du -sh $TARGET_DIR
}

# ============================================
# Cleanup temporary files
# ============================================

cleanup() {
    echo ""
    log_info "Delete temporary build directories? (Source code will be kept) [y/N]"
    read -r response
    if [[ "$response" =~ ^([yY][eE][sS]|[yY])$ ]]; then
        log_info "Cleaning up temporary build files..."
        # Only delete build directories, keep source code
        if [ -d "$SRC_DIR" ]; then
            find $SRC_DIR -type d -name "build-*" -exec rm -rf {} + 2>/dev/null || true
            find $SRC_DIR -type d -name "android-*" -exec rm -rf {} + 2>/dev/null || true
            find $SRC_DIR -type d -name "cmake-build-*" -exec rm -rf {} + 2>/dev/null || true
        fi
        log_info "Source code directory preserved: $SRC_DIR"
        log_info "Installation directory preserved: $PREFIX_DIR"
        log_success "Cleanup complete! (All source code preserved)"
    else
        log_info "Keeping all files: $BUILD_ROOT"
        log_info "Source code directory: $SRC_DIR"
        log_info "You can run this script again to rebuild"
    fi
}

# ============================================
# Check dependency requirements
# ============================================

check_needs() {
    log_info "Checking existing library files..."

    local TARGET_LIB_DIR="$PROJECT_DIR/app/src/main/cpp/libheif/lib"

    # Default libraries to build (currently only building libheif and libjpeg)
    NEED_X265=false
    NEED_LIBDE265=false
    NEED_BENTO4=false
    NEED_LIBHEIF=true
    NEED_LIBJPEG=true
    NEED_FFMPEG=false

    # Helper function: Check if a library exists for all ABIs
    check_lib_exists() {
        local lib_name=$1
        for ABI in "${ABIS[@]}"; do
            if [ ! -f "$TARGET_LIB_DIR/$ABI/$lib_name" ]; then
                return 1 # Not exists
            fi
        done
        return 0 # Exists
    }

    # Check library status
    # [Commented] Currently only building libheif and libjpeg, not checking other libraries
     if check_lib_exists "libx265.so"; then APP_HAS_X265=true; else APP_HAS_X265=false; fi
     if check_lib_exists "libde265.so"; then APP_HAS_DE265=true; else APP_HAS_DE265=false; fi
    # if check_lib_exists "libap4.so"; then APP_HAS_BENTO4=true; else APP_HAS_BENTO4=false; fi
    # if check_lib_exists "libavcodec.so"; then APP_HAS_FFMPEG=true; else APP_HAS_FFMPEG=false; fi
    if check_lib_exists "libheif.so"; then APP_HAS_HEIF=true; else APP_HAS_HEIF=false; fi
    if check_lib_exists "libjpeg.so"; then APP_HAS_JPEG=true; else APP_HAS_JPEG=false; fi

    # [Commented] 1. Bento4 (no dependencies)
    # if [ "$APP_HAS_BENTO4" = "true" ]; then
    #     NEED_BENTO4=false
    #     log_info "Bento4 already exists, skipping build"
    # fi

    # 2. LibHeif check (currently doesn't depend on x265, libde265)
    if [ "$APP_HAS_HEIF" = "true" ]; then
        NEED_LIBHEIF=false
        log_info "libheif already exists, skipping build"
    fi

     if [ "$APP_HAS_HEIF" = "true" ]; then
         NEED_LIBHEIF=false
         log_info "libheif already exists, skipping build"

         # If libheif doesn't need to be built, dependencies only need to check if they exist in App
         if [ "$APP_HAS_X265" = "true" ]; then
             NEED_X265=false
             log_info "x265 already exists, skipping build"
         fi
         if [ "$APP_HAS_DE265" = "true" ]; then
             NEED_LIBDE265=false
             log_info "libde265 already exists, skipping build"
         fi
     else
         # LibHeif needs to be built, must ensure dependencies are available in PREFIX_DIR
         log_info "libheif needs to be built, checking dependencies..."

         # Check x265 in PREFIX
         local prefix_has_x265=true
         for ABI in "${ABIS[@]}"; do
             if [ ! -f "$PREFIX_DIR/x265/$ABI/lib/libx265.so" ]; then prefix_has_x265=false; break; fi
         done

         if [ "$APP_HAS_X265" = "true" ] && [ "$prefix_has_x265" = "true" ]; then
             NEED_X265=false
             log_info "x265 exists and build files complete, skipping build"
         elif [ "$APP_HAS_X265" = "true" ] && [ "$prefix_has_x265" = "false" ]; then
             NEED_X265=true
             log_warn "x265 exists in App but missing from build directory (required for libheif build), will rebuild"
         elif [ "$APP_HAS_X265" = "false" ]; then
             NEED_X265=true
         fi

         # Check libde265 in PREFIX
         local prefix_has_de265=true
         for ABI in "${ABIS[@]}"; do
             if [ ! -f "$PREFIX_DIR/libde265/$ABI/lib/libde265.so" ]; then prefix_has_de265=false; break; fi
         done

         if [ "$APP_HAS_DE265" = "true" ] && [ "$prefix_has_de265" = "true" ]; then
             NEED_LIBDE265=false
             log_info "libde265 exists and build files complete, skipping build"
         elif [ "$APP_HAS_DE265" = "true" ] && [ "$prefix_has_de265" = "false" ]; then
             NEED_LIBDE265=true
             log_warn "libde265 exists in App but missing from build directory (required for libheif build), will rebuild"
         elif [ "$APP_HAS_DE265" = "false" ]; then
             NEED_LIBDE265=true
         fi
     fi

    if [ "$APP_HAS_JPEG" = "true" ]; then
        NEED_LIBJPEG=false
        log_info "libjpeg already exists, skipping build"
    fi

    # [Commented] Check ffmpeg
    # if [ "$APP_HAS_FFMPEG" = "true" ]; then
    #     NEED_FFMPEG=false
    #     log_info "ffmpeg already exists, skipping build"
    # fi
}

# ============================================
# Main function
# ============================================

main() {
    echo ""
    log_info "=========================================="
    log_info "LibHeif $LIBHEIF_VERSION Android Build Script"
    log_info "macOS specific version"
    log_info "=========================================="
    echo ""

    # Check environment
    check_environment
    echo ""

    # Initialize
    init_directories
    echo ""

    # Check dependency requirements
    check_needs
    echo ""

    # Record start time
    local start_time=$(date +%s)

    log_info "Starting build, this may take 30-60 minutes..."
    log_info "Using $NPROC CPU cores for parallel compilation"
    echo ""

    # Build dependencies
     if [ "$NEED_X265" = "true" ]; then
         build_x265
         echo ""
     fi

     if [ "$NEED_LIBDE265" = "true" ]; then
         build_libde265
         echo ""
     fi

    if [ "$NEED_LIBJPEG" = "true" ]; then
        build_libjpeg
        echo ""
    fi

    # [Commented] Bento4 build - currently only building libheif and libjpeg
    # if [ "$NEED_BENTO4" = "true" ]; then
    #     build_bento4
    #     echo ""
    # fi

    # [Commented] ffmpeg build - currently only building libheif and libjpeg
    # if [ "$NEED_FFMPEG" = "true" ]; then
    #     build_ffmpeg
    #     echo ""
    # fi

    # Build libheif
    if [ "$NEED_LIBHEIF" = "true" ]; then
        build_libheif
        echo ""
    fi

    # Copy files
    copy_to_project
    echo ""

    # Strip library files
    strip_libraries
    echo ""

    # Show statistics
    show_statistics
    echo ""

    # Calculate elapsed time
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    local minutes=$((duration / 60))
    local seconds=$((duration % 60))

    log_success "=========================================="
    log_success "🎉 Build complete!"
    log_success "=========================================="
    log_success "Total time: ${minutes} minutes ${seconds} seconds"
    log_success "Output directory: $PROJECT_DIR/app/src/main/cpp/libheif"
    echo ""

    log_info "Next steps:"
    log_info "1. cd $PROJECT_DIR"
    log_info "2. ./gradlew clean"
    log_info "3. ./gradlew assembleDebug"
    log_info "4. Run tests"
    echo ""

    # Cleanup
    cleanup

    log_success "All complete! 🎊"
}

# Run main function
main
