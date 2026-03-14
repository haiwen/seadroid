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
export ANDROID_API=34

# The schema to compile
ABIS=(
    "armeabi-v7a"
    "arm64-v8a"
#    "x86"
    "x86_64"
)

# LibHeif version
LIBHEIF_VERSION="v1.21.2"

# LibJpeg version
LIBJPEG_VERSION="3.1.3"

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
    log_info "Starting to build libjpeg-turbo $LIBJPEG_VERSION..."
    log_info "=========================================="
    cd $SRC_DIR
    if [ ! -d "libjpeg-turbo" ]; then
        log_info "Cloning libjpeg-turbo source code..."
        git clone --branch $LIBJPEG_VERSION https://github.com/libjpeg-turbo/libjpeg-turbo.git
    else
        log_info "libjpeg-turbo source code already exists, skipping download"
        cd libjpeg-turbo
        git fetch --tags
        git checkout $LIBJPEG_VERSION
        cd ..
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
            -DCMAKE_SHARED_LINKER_FLAGS="-Wl,-z,max-page-size=16384" \
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
    log_success "✅ libjpeg-turbo $LIBJPEG_VERSION build complete!"
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
        git clone --branch $LIBHEIF_VERSION https://github.com/strukturag/libheif.git
    else
        log_info "libheif source code already exists, skipping download"
        cd libheif
        git fetch --tags
        git checkout $LIBHEIF_VERSION
        cd ..
    fi

    cd libheif

    # Build for each architecture
    for ABI in "${ABIS[@]}"; do
        log_info "build libheif for $ABI..."

        BUILD_DIR="build-$ABI"
        INSTALL_DIR="$PREFIX_DIR/libheif/$ABI"

        rm -rf $BUILD_DIR
        mkdir -p $BUILD_DIR
        cd $BUILD_DIR

        cmake .. \
            -G Ninja \
            -DCMAKE_TOOLCHAIN_FILE=$ANDROID_NDK/build/cmake/android.toolchain.cmake \
            -DCMAKE_SHARED_LINKER_FLAGS="-Wl,-z,max-page-size=16384" \
            -DANDROID_ABI=$ABI \
            -DANDROID_PLATFORM=android-$ANDROID_API \
            -DCMAKE_BUILD_TYPE=Release \
            -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR \
            -DBUILD_SHARED_LIBS=ON \
            -DWITH_EXAMPLES=OFF \
            -DBUILD_TESTING=OFF \
            -DWITH_LIBDE265=OFF \
            -DWITH_X265=OFF \
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
    # Copy library files
    for ABI in "${ABIS[@]}"; do
        log_info "Copying $ABI library files..."
        mkdir -p $TARGET_DIR/lib/$ABI

        # Copy libheif
        if [ "$NEED_LIBHEIF" = "true" ]; then
            cp $PREFIX_DIR/libheif/$ABI/lib/libheif.so $TARGET_DIR/lib/$ABI/
        fi

        if [ -f "$PREFIX_DIR/libjpeg/$ABI/lib/libjpeg.so" ]; then
            cp $PREFIX_DIR/libjpeg/$ABI/lib/libjpeg.so $TARGET_DIR/lib/$ABI/
        fi

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
    log_info "=========================================="
    log_info "Cleaning up temporary build files..."
    log_info "=========================================="

    # Only delete build directories, keep source code and installation files
    if [ -d "$SRC_DIR" ]; then
        log_info "Removing build directories from: $SRC_DIR"
        find $SRC_DIR -type d -name "build-*" -exec rm -rf {} + 2>/dev/null || true
        find $SRC_DIR -type d -name "android-*" -exec rm -rf {} + 2>/dev/null || true
        find $SRC_DIR -type d -name "cmake-build-*" -exec rm -rf {} + 2>/dev/null || true

        # Count removed directories
        local removed_count=$(find $SRC_DIR -maxdepth 1 -type d 2>/dev/null | wc -l | tr -d ' ')
        log_info "Removed temporary build directories"
    fi

    log_success "✅ Cleanup complete!"
    log_info "Source code preserved: $SRC_DIR"
    log_info "Installation files preserved: $PREFIX_DIR"
    log_info "You can run this script again to rebuild"
}

# ============================================
# Check dependency requirements
# ============================================

check_needs() {
    log_info "Checking existing library files..."

    local TARGET_LIB_DIR="$PROJECT_DIR/app/src/main/cpp/libheif/lib"

    # Default libraries to build (currently only building libheif and libjpeg)
    NEED_LIBHEIF=true
    NEED_LIBJPEG=true

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
    # Currently only building libheif and libjpeg, not checking other libraries
    if check_lib_exists "libheif.so"; then APP_HAS_HEIF=true; else APP_HAS_HEIF=false; fi
    if check_lib_exists "libjpeg.so"; then APP_HAS_JPEG=true; else APP_HAS_JPEG=false; fi

    # LibHeif check (currently doesn't depend on x265, libde265)
    if [ "$APP_HAS_HEIF" = "true" ]; then
        NEED_LIBHEIF=false
        log_info "libheif already exists, skipping build"
    fi

    if [ "$APP_HAS_JPEG" = "true" ]; then
        NEED_LIBJPEG=false
        log_info "libjpeg already exists, skipping build"
    fi
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
    if [ "$NEED_LIBJPEG" = "true" ]; then
        build_libjpeg
        echo ""
    fi

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

    log_success "All complete!"
}

# Run main function
main
