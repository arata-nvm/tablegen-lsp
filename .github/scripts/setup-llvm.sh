#!/bin/bash
set -euxo pipefail

script_dir=$(dirname "$(readlink -f "$0")")
llvm_version=$(cat "${script_dir}/../LLVM_VERSION" | tr -d '\r\n')

base_url="https://github.com/arata-nvm/tablegen-lsp/releases/download/llvm-${llvm_version}"
os=$(uname -s)
arch=$(uname -m)

filename=""
if [ "$os" = "Linux" ]; then
    if [ "$arch" = "x86_64" ]; then
        filename="LLVM-${llvm_version}-Linux-X64.tar.xz"
    elif [ "$arch" = "aarch64" ]; then
        filename="LLVM-${llvm_version}-Linux-ARM64.tar.xz"
    fi
elif [ "$os" = "Darwin" ]; then
    if [ "$arch" = "arm64" ]; then
        filename="LLVM-${llvm_version}-macOS-ARM64.tar.xz"
    fi
elif [[ "$os" == MINGW* ]] || [[ "$os" == MSYS* ]]; then
    if [ "$arch" = "x86_64" ]; then
        filename="clang+llvm-${llvm_version}-x86_64-pc-windows-msvc.tar.xz"
    elif [ "$arch" = "aarch64" ]; then
        filename="clang+llvm-${llvm_version}-aarch64-pc-windows-msvc.tar.xz"
    fi
fi

if [ -z "$filename" ]; then
    echo "Unsupported platform: $os-$arch"
    exit 1
fi

llvm_prefix="${RUNNER_TEMP}/llvm-download"
mkdir "$llvm_prefix"
cd "$llvm_prefix"

curl -LO "${base_url}/${filename}"
tar -xf "$filename" --strip-components 1

llvm_version_suffix=$(echo "$llvm_version" | awk -F. '{print $1"0"}')
echo "TABLEGEN_${llvm_version_suffix}_PREFIX=${llvm_prefix}" >>$GITHUB_ENV
