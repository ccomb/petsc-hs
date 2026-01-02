#!/bin/bash
# Build petsc-hs standalone with cabal
# Requires PETSC_DIR, SLEPC_DIR, and PETSC_ARCH to be set

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Auto-detect paths if not set
PETSC_DIR=${PETSC_DIR:-"$SCRIPT_DIR/../petsc"}
SLEPC_DIR=${SLEPC_DIR:-"$SCRIPT_DIR/../slepc"}
PETSC_ARCH=${PETSC_ARCH:-"arch-linux-c-opt"}

if [[ ! -d "$PETSC_DIR/$PETSC_ARCH" ]]; then
    echo "ERROR: PETSc not found at $PETSC_DIR/$PETSC_ARCH"
    echo "Set PETSC_DIR and PETSC_ARCH environment variables"
    exit 1
fi

if [[ ! -d "$SLEPC_DIR/$PETSC_ARCH" ]]; then
    echo "ERROR: SLEPc not found at $SLEPC_DIR/$PETSC_ARCH"
    echo "Set SLEPC_DIR environment variable"
    exit 1
fi

echo "Using PETSc: $PETSC_DIR/$PETSC_ARCH"
echo "Using SLEPc: $SLEPC_DIR/$PETSC_ARCH"

export LD_LIBRARY_PATH="$PETSC_DIR/$PETSC_ARCH/lib:$SLEPC_DIR/$PETSC_ARCH/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

# Write cabal.project.local
cat > "$SCRIPT_DIR/cabal.project.local" << EOF
extra-lib-dirs: $PETSC_DIR/$PETSC_ARCH/lib
              , $SLEPC_DIR/$PETSC_ARCH/lib
extra-include-dirs: $PETSC_DIR/include
                  , $PETSC_DIR/$PETSC_ARCH/include
                  , $SLEPC_DIR/include
                  , $SLEPC_DIR/$PETSC_ARCH/include
EOF

cd "$SCRIPT_DIR"

# Build library only by default (test executables have inline-c issues when standalone)
if [[ $# -eq 0 ]]; then
    cabal build lib:petsc-hs
else
    cabal build "$@"
fi

echo ""
echo "Build successful!"
echo "To use: export LD_LIBRARY_PATH=\"$PETSC_DIR/$PETSC_ARCH/lib:$SLEPC_DIR/$PETSC_ARCH/lib:\$LD_LIBRARY_PATH\""
