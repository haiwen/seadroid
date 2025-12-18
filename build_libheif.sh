#!/bin/bash
#
# LibHeif 1.20.2 Android 编译脚本 - macOS 专用版
#
# 使用方法:
#   1. 编辑下面的配置
#   2. chmod +x build_libheif_macos.sh
#   3. ./build_libheief_macos.sh
#

set -e  # 遇到错误立即退出

# ============================================
# 配置部分
# ============================================

# 获取脚本所在目录（即项目根目录）
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 目标项目路径（脚本所在目录）
export PROJECT_DIR="$SCRIPT_DIR"

# 工作目录（项目内的相对路径）
export BUILD_ROOT="$PROJECT_DIR/libheif-android-build"

# Android NDK 路径（从环境变量获取）
# 优先级: ANDROID_NDK_HOME > ANDROID_NDK > ANDROID_SDK_ROOT/ndk/最新版本
if [ -n "$ANDROID_NDK_HOME" ] && [ -d "$ANDROID_NDK_HOME" ]; then
    export ANDROID_NDK="$ANDROID_NDK_HOME"
elif [ -n "$ANDROID_NDK" ] && [ -d "$ANDROID_NDK" ]; then
    # ANDROID_NDK 已设置，保持不变
    :
elif [ -n "$ANDROID_SDK_ROOT" ] && [ -d "$ANDROID_SDK_ROOT/ndk" ]; then
    # 自动查找最新的 NDK 版本
    NDK_VERSION=$(ls -1 "$ANDROID_SDK_ROOT/ndk" 2>/dev/null | sort -V | tail -n1)
    if [ -n "$NDK_VERSION" ]; then
        export ANDROID_NDK="$ANDROID_SDK_ROOT/ndk/$NDK_VERSION"
    fi
elif [ -n "$ANDROID_HOME" ] && [ -d "$ANDROID_HOME/ndk" ]; then
    # 兼容 ANDROID_HOME 环境变量
    NDK_VERSION=$(ls -1 "$ANDROID_HOME/ndk" 2>/dev/null | sort -V | tail -n1)
    if [ -n "$NDK_VERSION" ]; then
        export ANDROID_NDK="$ANDROID_HOME/ndk/$NDK_VERSION"
    fi
fi

# Android API Level
export ANDROID_API=28

# 要编译的架构 (不包含 x86_64，因为存在编译兼容性问题)
ABIS=(
    "armeabi-v7a"
    "arm64-v8a"
    "x86"
    "x86_64"
)

# 版本信息
LIBHEIF_VERSION="v1.20.2"

# ============================================
# macOS 系统配置
# ============================================

# 获取 CPU 核心数
NPROC=$(sysctl -n hw.ncpu)
echo "检测到 macOS 系统, CPU 核心数: $NPROC"

# NDK strip 工具前缀
STRIP_PREFIX="darwin-x86_64"

# ============================================
# 颜色输出
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
# 检查环境
# ============================================

check_environment() {
    log_info "检查编译环境..."

    # 检查操作系统
    if [[ "$OSTYPE" != "darwin"* ]]; then
        log_error "此脚本仅适用于 macOS"
        log_error "Linux 用户请使用其他版本的脚本"
        exit 1
    fi

    # 检查 NDK
    if [ ! -d "$ANDROID_NDK" ]; then
        log_error "Android NDK 未找到: $ANDROID_NDK"
        exit 1
    fi
    log_success "找到 Android NDK: $ANDROID_NDK"

    # 检查项目目录
    if [ ! -d "$PROJECT_DIR" ]; then
        log_error "项目目录未找到: $PROJECT_DIR"
        exit 1
    fi
    log_success "找到项目目录: $PROJECT_DIR"

    # 检查 Homebrew 工具
    local tools=("cmake" "git" "ninja" "pkg-config")
    for tool in "${tools[@]}"; do
        if ! command -v $tool &> /dev/null; then
            log_error "未找到工具: $tool"
            log_error "请使用 Homebrew 安装: brew install $tool"
            exit 1
        fi
    done
    log_success "所有必需工具已安装"

    # 检查 NASM
    if ! command -v nasm &> /dev/null; then
        log_warn "未找到 NASM"
        log_warn "建议安装: brew install nasm"
    else
        log_success "找到 NASM: $(which nasm)"
    fi
}

build_libjpeg() {
    log_info "=========================================="
    log_info "开始编译 libjpeg-turbo..."
    log_info "=========================================="
    cd $SRC_DIR
    if [ ! -d "libjpeg-turbo" ]; then
        log_info "克隆 libjpeg-turbo 源码..."
        git clone https://github.com/libjpeg-turbo/libjpeg-turbo.git
    else
        log_info "libjpeg-turbo 源码已存在,跳过下载"
    fi
    cd libjpeg-turbo
    for ABI in "${ABIS[@]}"; do
        log_info "编译 libjpeg-turbo for $ABI..."
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
        log_success "libjpeg-turbo for $ABI 编译完成"
    done
    log_success "✅ libjpeg-turbo 全部编译完成!"
}

# ============================================
# 初始化目录
# ============================================

init_directories() {
    log_info "初始化编译目录..."

    export PREFIX_DIR="$BUILD_ROOT/prefix"
    export SRC_DIR="$BUILD_ROOT/src"

    mkdir -p $PREFIX_DIR
    mkdir -p $SRC_DIR

    log_success "编译目录创建完成: $BUILD_ROOT"
}

# ============================================
# 编译 x265
# ============================================

build_x265() {
    log_info "=========================================="
    log_info "开始编译 x265..."
    log_info "=========================================="

    cd $SRC_DIR

    # 下载 x265
    if [ ! -d "x265" ]; then
        log_info "克隆 x265 源码..."
        git clone https://bitbucket.org/multicoreware/x265_git.git x265
    else
        log_info "x265 源码已存在,跳过下载"
    fi

    cd x265/build

    # 为每个架构编译
    for ABI in "${ABIS[@]}"; do
        log_info "编译 x265 for $ABI..."

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
            -DENABLE_LIBNUMA=OFF \
            -DCMAKE_EXE_LINKER_FLAGS="-Wl,--no-undefined" \
            -DCMAKE_SHARED_LINKER_FLAGS="-Wl,--no-undefined -Wl,--as-needed" \
            -DCMAKE_CXX_FLAGS="-DANDROID -fPIC" \
            -DCMAKE_C_FLAGS="-DANDROID -fPIC"

        ninja -j$NPROC
        ninja install

        cd ..

        log_success "x265 for $ABI 编译完成"
    done

    log_success "✅ x265 全部编译完成!"
}

# ============================================
# 编译 libde265
# ============================================

build_libde265() {
    log_info "=========================================="
    log_info "开始编译 libde265..."
    log_info "=========================================="

    cd $SRC_DIR

    # 下载 libde265
    if [ ! -d "libde265" ]; then
        log_info "克隆 libde265 源码..."
        git clone https://github.com/strukturag/libde265.git
    else
        log_info "libde265 源码已存在,跳过下载"
    fi

    cd libde265

    # 为每个架构编译
    for ABI in "${ABIS[@]}"; do
        log_info "编译 libde265 for $ABI..."

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

        log_success "libde265 for $ABI 编译完成"
    done

    log_success "✅ libde265 全部编译完成!"
}

# ============================================
# 编译 Bento4
# ============================================

build_bento4() {
    log_info "=========================================="
    log_info "开始编译 Bento4..."
    log_info "=========================================="

    cd $SRC_DIR

    # 下载 Bento4
    if [ ! -d "Bento4" ]; then
        log_info "克隆 Bento4 源码..."
        git clone https://github.com/axiomatic-systems/Bento4.git
    else
        log_info "Bento4 源码已存在,跳过下载"
    fi

    cd Bento4

    # Bento4 没有直接提供 CMake 安装支持，我们需要手动编译源文件
    # 创建一个简单的 CMakeLists.txt 用于编译

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

    # 为每个架构编译
    for ABI in "${ABIS[@]}"; do
        log_info "编译 Bento4 for $ABI..."

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

        # 手动安装
        mkdir -p $INSTALL_DIR/lib
        mkdir -p $INSTALL_DIR/include/Bento4

        cp libap4.so $INSTALL_DIR/lib/
        cp -r ../Source/C++/Core/*.h $INSTALL_DIR/include/Bento4/
        cp -r ../Source/C++/Codecs/*.h $INSTALL_DIR/include/Bento4/
        cp -r ../Source/C++/MetaData/*.h $INSTALL_DIR/include/Bento4/
        cp -r ../Source/C++/Crypto/*.h $INSTALL_DIR/include/Bento4/

        cd ..

        log_success "Bento4 for $ABI 编译完成"
    done

    log_success "✅ Bento4 全部编译完成!"
}

# ============================================
# 编译 ffmpeg
# ============================================

build_ffmpeg() {
    log_info "=========================================="
    log_info "开始编译 ffmpeg..."
    log_info "=========================================="

    cd $SRC_DIR

    # 下载 ffmpeg
    if [ ! -d "ffmpeg" ]; then
        log_info "克隆 ffmpeg 源码..."
        if ! git clone --depth 1 --branch n6.1 https://github.com/FFmpeg/FFmpeg.git ffmpeg; then
            log_warn "GitHub 克隆失败，尝试官方镜像"
            git clone --depth 1 --branch n6.1 https://git.ffmpeg.org/ffmpeg.git ffmpeg
        fi
    else
        log_info "ffmpeg 源码已存在,跳过下载"
    fi

    cd ffmpeg

    # 为每个架构编译
    for ABI in "${ABIS[@]}"; do
        log_info "编译 ffmpeg for $ABI..."

        BUILD_DIR="build-$ABI"
        INSTALL_DIR="$PREFIX_DIR/ffmpeg/$ABI"

        rm -rf $BUILD_DIR
        mkdir -p $BUILD_DIR
        cd $BUILD_DIR

        # 设置交叉编译工具链
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

        # x86 架构需要禁用汇编优化以避免 PIC 链接错误
        EXTRA_FLAGS=""
        if [ "$ABI" = "x86" ]; then
            EXTRA_FLAGS="--disable-asm"
            log_info "x86 架构：禁用汇编优化"
        fi

        # 配置 ffmpeg
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
        log_success "ffmpeg for $ABI 编译完成"
    done

    log_success "✅ ffmpeg 全部编译完成!"
}

# ============================================
# 编译 libheif
# ============================================

build_libheif() {
    log_info "=========================================="
    log_info "开始编译 libheif $LIBHEIF_VERSION..."
    log_info "=========================================="

    cd $SRC_DIR

    # 下载 libheif
    if [ ! -d "libheif" ]; then
        log_info "克隆 libheif 源码..."
        git clone https://github.com/strukturag/libheif.git
    else
        log_info "libheif 源码已存在,跳过下载"
    fi

    cd libheif
    git fetch --tags
    git checkout $LIBHEIF_VERSION

    # 为每个架构编译
    for ABI in "${ABIS[@]}"; do
        log_info "编译 libheif for $ABI..."

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
            -DENABLE_PARALLEL_TILE_DECODING=ON
             -DLIBDE265_LIBRARY=$PREFIX_DIR/libde265/$ABI/lib/libde265.so \
             -DLIBDE265_INCLUDE_DIR=$PREFIX_DIR/libde265/$ABI/include \
             -DX265_LIBRARY=$PREFIX_DIR/x265/$ABI/lib/libx265.so \
             -DX265_INCLUDE_DIR=$PREFIX_DIR/x265/$ABI/include

        ninja -j$NPROC
        ninja install

        cd ..

        log_success "libheif for $ABI 编译完成"
    done

    log_success "✅ libheif 全部编译完成!"
}

# ============================================
# 复制到项目
# ============================================

copy_to_project() {
    log_info "=========================================="
    log_info "复制库文件到项目..."
    log_info "=========================================="

    TARGET_DIR="$PROJECT_DIR/app/src/main/cpp/libheif"

    # 创建目录
    mkdir -p $TARGET_DIR/include
    mkdir -p $TARGET_DIR/lib

    # 复制头文件
    log_info "复制头文件..."
    if [ "$NEED_LIBHEIF" = "true" ]; then
        cp -r $PREFIX_DIR/libheif/arm64-v8a/include/libheif $TARGET_DIR/include/
    fi
    if [ "$NEED_LIBJPEG" = "true" ] || [ -d "$PREFIX_DIR/libjpeg/arm64-v8a/include" ]; then
        mkdir -p $TARGET_DIR/include/libjpeg
        cp -r $PREFIX_DIR/libjpeg/arm64-v8a/include/* $TARGET_DIR/include/libjpeg/
    fi
    # [已注释] 复制 Bento4 头文件 - 当前只编译 libheif 和 libjpeg
    # if [ "$NEED_BENTO4" = "true" ]; then
    #     mkdir -p $TARGET_DIR/include/Bento4
    #     cp -r $PREFIX_DIR/bento4/arm64-v8a/include/Bento4/* $TARGET_DIR/include/Bento4/
    # fi
    # [已注释] 复制 ffmpeg 头文件 - 当前只编译 libheif 和 libjpeg
    # if [ "$NEED_FFMPEG" = "true" ] || [ -d "$PREFIX_DIR/ffmpeg/arm64-v8a/include" ]; then
    #     mkdir -p $TARGET_DIR/include/ffmpeg
    #     cp -r $PREFIX_DIR/ffmpeg/arm64-v8a/include/* $TARGET_DIR/include/ffmpeg/
    # fi

    # 复制库文件
    for ABI in "${ABIS[@]}"; do
        log_info "复制 $ABI 库文件..."
        mkdir -p $TARGET_DIR/lib/$ABI

        # 复制 libheif
        if [ "$NEED_LIBHEIF" = "true" ]; then
            cp $PREFIX_DIR/libheif/$ABI/lib/libheif.so $TARGET_DIR/lib/$ABI/
        fi

         if [ "$NEED_X265" = "true" ]; then
             cp $PREFIX_DIR/x265/$ABI/lib/libx265.so $TARGET_DIR/lib/$ABI/
         fi
         if [ "$NEED_LIBDE265" = "true" ]; then
             cp $PREFIX_DIR/libde265/$ABI/lib/libde265.so $TARGET_DIR/lib/$ABI/
         fi
        # [已注释] 复制 Bento4 - 当前只编译 libheif 和 libjpeg
        # if [ "$NEED_BENTO4" = "true" ]; then
        #     cp $PREFIX_DIR/bento4/$ABI/lib/libap4.so $TARGET_DIR/lib/$ABI/
        # fi

        if [ -f "$PREFIX_DIR/libjpeg/$ABI/lib/libjpeg.so" ]; then
            cp $PREFIX_DIR/libjpeg/$ABI/lib/libjpeg.so $TARGET_DIR/lib/$ABI/
        fi

        # [已注释] 复制 ffmpeg 库文件 - 当前只编译 libheif 和 libjpeg
        # if [ "$NEED_FFMPEG" = "true" ] || [ -d "$PREFIX_DIR/ffmpeg/$ABI/lib" ]; then
        #     if [ -d "$PREFIX_DIR/ffmpeg/$ABI/lib" ]; then
        #         cp $PREFIX_DIR/ffmpeg/$ABI/lib/*.so $TARGET_DIR/lib/$ABI/ 2>/dev/null || true
        #     fi
        # fi

        log_success "$ABI 库文件复制完成"
    done

    log_success "✅ 文件复制完成!"
}

# ============================================
# Strip 库文件
# ============================================

strip_libraries() {
    log_info "=========================================="
    log_info "Strip 库文件以减小体积..."
    log_info "=========================================="

    TARGET_DIR="$PROJECT_DIR/app/src/main/cpp/libheif/lib"

    for ABI in "${ABIS[@]}"; do
        log_info "Strip $ABI 库文件..."

        # 选择对应的 strip 工具
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
            # [已注释] 当前只编译 libheif 和 libjpeg
            # [ -f "$TARGET_DIR/$ABI/libx265.so" ] && $STRIP_TOOL $TARGET_DIR/$ABI/libx265.so
            # [ -f "$TARGET_DIR/$ABI/libde265.so" ] && $STRIP_TOOL $TARGET_DIR/$ABI/libde265.so
            # [ -f "$TARGET_DIR/$ABI/libap4.so" ] && $STRIP_TOOL $TARGET_DIR/$ABI/libap4.so
            # [已注释] Strip ffmpeg 库文件 - 当前只编译 libheif 和 libjpeg
            # for ffmpeg_lib in $TARGET_DIR/$ABI/libav*.so $TARGET_DIR/$ABI/libsw*.so $TARGET_DIR/$ABI/libpostproc.so; do
            #     [ -f "$ffmpeg_lib" ] && $STRIP_TOOL "$ffmpeg_lib"
            # done
            log_success "$ABI 库文件 strip 完成"
        else
            log_warn "未找到 strip 工具: $STRIP_TOOL"
        fi
    done

    log_success "✅ Strip 完成!"
}

# ============================================
# 显示统计信息
# ============================================

show_statistics() {
    log_info "=========================================="
    log_info "编译统计信息"
    log_info "=========================================="

    TARGET_DIR="$PROJECT_DIR/app/src/main/cpp/libheif/lib"

    for ABI in "${ABIS[@]}"; do
    if [ -d "$TARGET_DIR/$ABI" ]; then
        log_info "架构: $ABI"
            ls -lh $TARGET_DIR/$ABI/*.so
        echo ""
    fi
    done

    log_info "总大小:"
    du -sh $TARGET_DIR
}

# ============================================
# 清理临时文件
# ============================================

cleanup() {
    echo ""
    log_info "是否删除临时构建目录? (源代码将始终保留) [y/N]"
    read -r response
    if [[ "$response" =~ ^([yY][eE][sS]|[yY])$ ]]; then
        log_info "清理临时构建文件..."
        # 只删除构建目录，保留源代码
        if [ -d "$SRC_DIR" ]; then
            find $SRC_DIR -type d -name "build-*" -exec rm -rf {} + 2>/dev/null || true
            find $SRC_DIR -type d -name "android-*" -exec rm -rf {} + 2>/dev/null || true
            find $SRC_DIR -type d -name "cmake-build-*" -exec rm -rf {} + 2>/dev/null || true
        fi
        log_info "源代码目录已保留: $SRC_DIR"
        log_info "安装目录已保留: $PREFIX_DIR"
        log_success "清理完成! (所有源代码已保留)"
    else
        log_info "保留所有文件: $BUILD_ROOT"
        log_info "源代码目录: $SRC_DIR"
        log_info "如需重新编译,直接运行此脚本即可"
    fi
}

# ============================================
# 检查依赖需求
# ============================================

check_needs() {
    log_info "检查已存在的库文件..."

    local TARGET_LIB_DIR="$PROJECT_DIR/app/src/main/cpp/libheif/lib"

    # 默认需要编译的库（当前只编译 libheif 和 libjpeg）
    NEED_X265=false
    NEED_LIBDE265=false
    NEED_BENTO4=false
    NEED_LIBHEIF=true
    NEED_LIBJPEG=true
    NEED_FFMPEG=false

    # 辅助函数：检查某个库是否在所有 ABI 下都存在
    check_lib_exists() {
        local lib_name=$1
        for ABI in "${ABIS[@]}"; do
            if [ ! -f "$TARGET_LIB_DIR/$ABI/$lib_name" ]; then
                return 1 # 不存在
            fi
        done
        return 0 # 存在
    }

    # 检查各库状态
    # [已注释] 当前只编译 libheif 和 libjpeg，不检查其他库
     if check_lib_exists "libx265.so"; then APP_HAS_X265=true; else APP_HAS_X265=false; fi
     if check_lib_exists "libde265.so"; then APP_HAS_DE265=true; else APP_HAS_DE265=false; fi
    # if check_lib_exists "libap4.so"; then APP_HAS_BENTO4=true; else APP_HAS_BENTO4=false; fi
    # if check_lib_exists "libavcodec.so"; then APP_HAS_FFMPEG=true; else APP_HAS_FFMPEG=false; fi
    if check_lib_exists "libheif.so"; then APP_HAS_HEIF=true; else APP_HAS_HEIF=false; fi
    if check_lib_exists "libjpeg.so"; then APP_HAS_JPEG=true; else APP_HAS_JPEG=false; fi

    # [已注释] 1. Bento4 (无依赖)
    # if [ "$APP_HAS_BENTO4" = "true" ]; then
    #     NEED_BENTO4=false
    #     log_info "Bento4 已存在，跳过编译"
    # fi

    # 2. LibHeif 检查（当前不依赖 x265, libde265）
    if [ "$APP_HAS_HEIF" = "true" ]; then
        NEED_LIBHEIF=false
        log_info "libheif 已存在，跳过编译"
    fi

     if [ "$APP_HAS_HEIF" = "true" ]; then
         NEED_LIBHEIF=false
         log_info "libheif 已存在，跳过编译"

         # 如果 libheif 不需要编译，那么依赖项只需要检查是否在 App 中存在
         if [ "$APP_HAS_X265" = "true" ]; then
             NEED_X265=false
             log_info "x265 已存在，跳过编译"
         fi
         if [ "$APP_HAS_DE265" = "true" ]; then
             NEED_LIBDE265=false
             log_info "libde265 已存在，跳过编译"
         fi
     else
         # LibHeif 需要编译，必须确保依赖项在 PREFIX_DIR 中可用
         log_info "libheif 需要编译，检查依赖项..."

         # 检查 PREFIX 中的 x265
         local prefix_has_x265=true
         for ABI in "${ABIS[@]}"; do
             if [ ! -f "$PREFIX_DIR/x265/$ABI/lib/libx265.so" ]; then prefix_has_x265=false; break; fi
         done

         if [ "$APP_HAS_X265" = "true" ] && [ "$prefix_has_x265" = "true" ]; then
             NEED_X265=false
             log_info "x265 已存在且构建文件完整，跳过编译"
         elif [ "$APP_HAS_X265" = "true" ] && [ "$prefix_has_x265" = "false" ]; then
             NEED_X265=true
             log_warn "x265 在 App 中存在，但在构建目录缺失（编译 libheif 需要），将重新编译"
         elif [ "$APP_HAS_X265" = "false" ]; then
             NEED_X265=true
         fi

         # 检查 PREFIX 中的 libde265
         local prefix_has_de265=true
         for ABI in "${ABIS[@]}"; do
             if [ ! -f "$PREFIX_DIR/libde265/$ABI/lib/libde265.so" ]; then prefix_has_de265=false; break; fi
         done

         if [ "$APP_HAS_DE265" = "true" ] && [ "$prefix_has_de265" = "true" ]; then
             NEED_LIBDE265=false
             log_info "libde265 已存在且构建文件完整，跳过编译"
         elif [ "$APP_HAS_DE265" = "true" ] && [ "$prefix_has_de265" = "false" ]; then
             NEED_LIBDE265=true
             log_warn "libde265 在 App 中存在，但在构建目录缺失（编译 libheif 需要），将重新编译"
         elif [ "$APP_HAS_DE265" = "false" ]; then
             NEED_LIBDE265=true
         fi
     fi

    if [ "$APP_HAS_JPEG" = "true" ]; then
        NEED_LIBJPEG=false
        log_info "libjpeg 已存在，跳过编译"
    fi

    # [已注释] 检查 ffmpeg
    # if [ "$APP_HAS_FFMPEG" = "true" ]; then
    #     NEED_FFMPEG=false
    #     log_info "ffmpeg 已存在，跳过编译"
    # fi
}

# ============================================
# 主函数
# ============================================

main() {
    echo ""
    log_info "=========================================="
    log_info "LibHeif $LIBHEIF_VERSION Android 编译脚本"
    log_info "macOS 专用版本"
    log_info "=========================================="
    echo ""

    # 检查环境
    check_environment
    echo ""

    # 初始化
    init_directories
    echo ""

    # 检查依赖需求
    check_needs
    echo ""

    # 记录开始时间
    local start_time=$(date +%s)

    log_info "开始编译,这可能需要 30-60 分钟..."
    log_info "使用 $NPROC 个 CPU 核心并行编译"
    echo ""

    # 编译依赖
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

    # [已注释] Bento4 编译 - 当前只编译 libheif 和 libjpeg
    # if [ "$NEED_BENTO4" = "true" ]; then
    #     build_bento4
    #     echo ""
    # fi

    # [已注释] ffmpeg 编译 - 当前只编译 libheif 和 libjpeg
    # if [ "$NEED_FFMPEG" = "true" ]; then
    #     build_ffmpeg
    #     echo ""
    # fi

    # 编译 libheif
    if [ "$NEED_LIBHEIF" = "true" ]; then
        build_libheif
        echo ""
    fi

    # 复制文件
    copy_to_project
    echo ""

    # Strip 库文件
    strip_libraries
    echo ""

    # 显示统计
    show_statistics
    echo ""

    # 计算耗时
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    local minutes=$((duration / 60))
    local seconds=$((duration % 60))

    log_success "=========================================="
    log_success "🎉 编译完成!"
    log_success "=========================================="
    log_success "总耗时: ${minutes}分${seconds}秒"
    log_success "输出目录: $PROJECT_DIR/app/src/main/cpp/libheif"
    echo ""

    log_info "下一步操作:"
    log_info "1. cd $PROJECT_DIR"
    log_info "2. ./gradlew clean"
    log_info "3. ./gradlew assembleDebug"
    log_info "4. 运行测试"
    echo ""

    # 清理
    cleanup

    log_success "全部完成! 🎊"
}

# 运行主函数
main
