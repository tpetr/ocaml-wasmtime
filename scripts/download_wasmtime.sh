#!/bin/bash
set -euox pipefail

VERSION="41.0.3"
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

# Map darwin->macos, arm64->aarch64
if [ "$OS" = "darwin" ]; then OS="macos"; fi
if [ "$ARCH" = "arm64" ]; then ARCH="aarch64"; fi

TARBALL="wasmtime-v${VERSION}-${ARCH}-${OS}-c-api.tar.xz"
URL="https://github.com/bytecodealliance/wasmtime/releases/download/v${VERSION}/${TARBALL}"
DEST="_wasmtime"

if [ -d "$DEST" ] && [ -f "$DEST/lib/libwasmtime.a" ]; then
  echo "wasmtime v${VERSION} already downloaded"
  exit 0
fi

echo "Downloading wasmtime v${VERSION} for ${ARCH}-${OS} ($URL) to $DEST..."
mkdir -p "$DEST"
curl -L "$URL" > file.tar.xz
cat file.tar.xz | tar xJ --strip-components=1 -C "$DEST"
echo "Done: $DEST/lib/libwasmtime.a"
