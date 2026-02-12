#!/bin/bash
# Extract the platform-appropriate wasmtime C API tarball.
# When run via opam, the tarball is pre-downloaded by extra-source.
# When run locally, falls back to download_wasmtime.sh.
set -euo pipefail

DEST="_wasmtime"

if [ -d "$DEST" ] && [ -f "$DEST/lib/libwasmtime.a" ]; then
  echo "wasmtime already available"
  exit 0
fi

# Look for a pre-downloaded tarball (from opam extra-source)
TARBALL=$(ls wasmtime-c-api-*.tar.xz 2>/dev/null | head -1)

if [ -n "$TARBALL" ]; then
  echo "Extracting ${TARBALL}..."
  mkdir -p "$DEST"
  tar xJf "$TARBALL" --strip-components=1 -C "$DEST"
  echo "Done: $DEST/lib/libwasmtime.a"
else
  # Fall back to downloading (for local development)
  exec bash "$(dirname "$0")/download_wasmtime.sh"
fi
