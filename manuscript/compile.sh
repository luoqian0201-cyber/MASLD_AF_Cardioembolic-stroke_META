#!/bin/bash
# ============================================================
#  compile.sh — Compile MASLD-Stroke Meta-Analysis Manuscript
#  Usage: ./compile.sh [--clean]
# ============================================================
set -e

export PATH="$HOME/texlive/usr/local/texlive/2026basic/bin/universal-darwin:$PATH"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BUILD_DIR="$SCRIPT_DIR/build"
TEX_FILE="main.tex"
PDF_NAME="MASLD_Stroke_MASLD_Manuscript.pdf"

cd "$SCRIPT_DIR"
mkdir -p "$BUILD_DIR"

# Clean mode
if [ "$1" = "--clean" ]; then
    echo "🧹 Cleaning build directory..."
    rm -rf "$BUILD_DIR"/*
fi

echo "📝 Compiling $TEX_FILE..."

# Pass 1
pdflatex -interaction=nonstopmode -output-directory="$BUILD_DIR" "$TEX_FILE" > /dev/null 2>&1
echo "   Pass 1/2 done"

# Pass 2 (resolve cross-references)
pdflatex -interaction=nonstopmode -output-directory="$BUILD_DIR" "$TEX_FILE" > /dev/null 2>&1
echo "   Pass 2/2 done"

# Copy PDF to manuscript directory
cp "$BUILD_DIR/main.pdf" "$SCRIPT_DIR/$PDF_NAME"

# Count pages
PAGES=$(pdfinfo "$BUILD_DIR/main.pdf" 2>/dev/null | grep "Pages" | awk '{print $2}')
if [ -z "$PAGES" ]; then
    PAGES="?"
fi

echo ""
echo "✅ Compilation successful!"
echo "   PDF: $SCRIPT_DIR/$PDF_NAME"
echo "   Pages: $PAGES"
echo "   Size: $(du -h "$BUILD_DIR/main.pdf" | cut -f1)"

# Quick error/warning summary
echo ""
echo "--- Warnings/Errors ---"
grep -c "Warning" "$BUILD_DIR/main.log" 2>/dev/null && echo "   warnings in log" || echo "   No warnings"
grep -c "Error" "$BUILD_DIR/main.log" 2>/dev/null && echo "   ⚠️  errors in log — check build/main.log" || echo "   No errors"

echo ""
echo "Done. Open with: open $SCRIPT_DIR/$PDF_NAME"
