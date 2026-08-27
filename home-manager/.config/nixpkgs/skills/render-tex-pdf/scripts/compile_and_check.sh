#!/usr/bin/env sh
set -eu

usage() {
  echo "usage: $0 /absolute/or/relative/path/to/report.tex" >&2
  exit 2
}

[ "$#" -eq 1 ] || usage

tex_input=$1
case "$tex_input" in
  /*) ;;
  *) tex_input=$(pwd)/$tex_input ;;
esac

[ -f "$tex_input" ] || {
  echo "TeX source not found: $tex_input" >&2
  exit 2
}

source_dir=$(dirname "$tex_input")
source_name=$(basename "$tex_input")
stem=${source_name%.tex}
[ "$stem" != "$source_name" ] || usage

pdf_path=$source_dir/$stem.pdf
log_path=$source_dir/$stem.log

run_tectonic() {
  if command -v tectonic >/dev/null 2>&1; then
    (cd "$source_dir" && tectonic --keep-logs "$source_name")
    return
  fi

  search_dir=$source_dir
  flake_root=
  while [ "$search_dir" != "/" ]; do
    if [ -f "$search_dir/flake.nix" ]; then
      flake_root=$search_dir
      break
    fi
    search_dir=$(dirname "$search_dir")
  done

  if [ -n "$flake_root" ] && command -v nix >/dev/null 2>&1; then
    if nix develop "$flake_root" -c sh -c 'command -v tectonic >/dev/null 2>&1'; then
      # The variables intentionally expand in the child shell.
      # shellcheck disable=SC2016
      TEX_PDF_SOURCE_DIR=$source_dir TEX_PDF_SOURCE_NAME=$source_name \
        nix develop "$flake_root" -c sh -c \
        'cd "$TEX_PDF_SOURCE_DIR" && tectonic --keep-logs "$TEX_PDF_SOURCE_NAME"'
      return
    fi
  fi

  if command -v nix >/dev/null 2>&1; then
    # The variables intentionally expand in the child shell.
    # shellcheck disable=SC2016
    TEX_PDF_SOURCE_DIR=$source_dir TEX_PDF_SOURCE_NAME=$source_name \
      nix shell nixpkgs#tectonic -c sh -c \
      'cd "$TEX_PDF_SOURCE_DIR" && tectonic --keep-logs "$TEX_PDF_SOURCE_NAME"'
    return
  fi

  echo "Neither tectonic nor nix is available." >&2
  exit 127
}

run_tectonic

[ -s "$pdf_path" ] || {
  echo "Tectonic did not produce a nonempty PDF: $pdf_path" >&2
  exit 1
}

[ -f "$log_path" ] || {
  echo "Tectonic did not retain its log: $log_path" >&2
  exit 1
}

warning_pattern='Overfull|Underfull|out of page|LaTeX Warning|Package .* Warning|Missing character|undefined references|undefined citations'
if grep -Ein "$warning_pattern" "$log_path"; then
  echo "TeX or layout warnings remain in $log_path" >&2
  exit 1
fi

if command -v pdfinfo >/dev/null 2>&1; then
  pdfinfo "$pdf_path" | awk -F ': *' '/^(Pages|Page size|PDF version):/ { print }'
else
  echo "note: pdfinfo unavailable; metadata check skipped" >&2
fi

if command -v pdftotext >/dev/null 2>&1; then
  text_tmp=$(mktemp)
  trap 'rm -f "$text_tmp"' EXIT HUP INT TERM
  pdftotext "$pdf_path" "$text_tmp"
  text_bytes=$(wc -c < "$text_tmp" | tr -d ' ')
  if [ "$text_bytes" -lt 20 ]; then
    echo "Extracted PDF text is unexpectedly short ($text_bytes bytes)." >&2
    exit 1
  fi
  echo "Extracted text: $text_bytes bytes"
else
  echo "note: pdftotext unavailable; text extraction check skipped" >&2
fi

pdf_bytes=$(wc -c < "$pdf_path" | tr -d ' ')
echo "Verified PDF build: $pdf_path ($pdf_bytes bytes)"
echo "Visual audit still required: a clean build cannot detect wrong arrows, overlaps, clipping, or unreadable diagrams."
