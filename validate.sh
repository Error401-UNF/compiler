#!/usr/bin/env bash
# Autograder for ./compiler filename
# - Positive tests: compare preamble exactly; compare Symbol Table IDs as a set (order-insensitive).
#   Exit code must be 0.
# - Negative tests (fail*.txt): exit code must be 1.

set -u
shopt -s nullglob

DIR="TestSuites2"

COMPILER="./target/x86_64-unknown-linux-musl/debug/Assignment_2" # This if you run linux (me i run linux :3)
#COMPILER="./target/x86_64-pc-windows-msvc/debug/Assignment_2" # This if you run windows

if [[ ! -x "$COMPILER" ]]; then
  echo "Cannot find executable $COMPILER" >&2
  exit 2
fi
if [[ ! -d "$DIR" ]]; then
  echo "Cannot find test directory $DIR" >&2
  exit 2
fi

total=0
score=0

# Colors
if [[ -t 1 ]]; then
  RED="$(printf '\033[31m')"
  GREEN="$(printf '\033[32m')"
  YELLOW="$(printf '\033[33m')"
  BOLD="$(printf '\033[1m')"
  RESET="$(printf '\033[0m')"
else
  RED=""; GREEN=""; YELLOW=""; BOLD=""; RESET=""
fi

TMP_OUT="$(mktemp)"; TMP_ERR="$(mktemp)"
cleanup() { rm -f "$TMP_OUT" "$TMP_ERR"; }
trap cleanup EXIT

# ---- Helper: normalize line endings (remove CR) ----
strip_cr() { sed 's/\r$//' "$1"; }

# ---- Helper: compare with unordered Symbol Table IDs ----
check_output() {
  local ans="$1" out="$2"

  # Prepare temp normalized copies (no CRLF issues)
  local A_PRE O_PRE A_IDS O_IDS
  A_PRE="$(mktemp)"; O_PRE="$(mktemp)"
  A_IDS="$(mktemp)"; O_IDS="$(mktemp)"
  trap 'rm -f "$A_PRE" "$O_PRE" "$A_IDS" "$O_IDS"' RETURN

  # 1) Extract preamble (everything BEFORE "Symbol Table:")
  strip_cr "$ans" | sed '/^Symbol Table:/,$d' >"$A_PRE"
  strip_cr "$out" | sed '/^Symbol Table:/,$d' >"$O_PRE"

  # Compare preamble strictly
  if ! diff -u --label "ans(pre)" --label "out(pre)" "$A_PRE" "$O_PRE" >/dev/null; then
    echo "Mismatch in preamble (before 'Symbol Table:')"
    diff -u --label "ans(pre)" --label "out(pre)" "$A_PRE" "$O_PRE" || true
    return 1
  fi

  # 2) Extract IDs from Symbol Table sections (lines starting with 'ID:'), sort to allow any order
  strip_cr "$ans" | sed -n '/^Symbol Table:/,$p' | grep -E '^ID:' | sort >"$A_IDS" || true
  strip_cr "$out" | sed -n '/^Symbol Table:/,$p' | grep -E '^ID:' | sort >"$O_IDS" || true

  # Compare ID multiset (order-insensitive; duplicates preserved because no 'uniq')
  if ! diff -u --label "ans(Symbol IDs)" --label "out(Symbol IDs)" "$A_IDS" "$O_IDS" >/dev/null; then
    echo "Mismatch in Symbol Table IDs (order-insensitive compare)"
    diff -u --label "ans(Symbol IDs)" --label "out(Symbol IDs)" "$A_IDS" "$O_IDS" || true
    return 1
  fi

  return 0
}

echo "${BOLD}== Positive tests (preamble exact; Symbol IDs order-insensitive; exit code=0) ==${RESET}"

for txt in "$DIR"/*.txt; do
  base="$(basename "$txt" .txt)"
  [[ "$base" == fail* ]] && continue
  ans="$DIR/$base.ans"
  [[ -f "$ans" ]] || { echo "${YELLOW}[Skipped]${RESET} $base (missing $base.ans)"; continue; }

  ((total++))

  "$COMPILER" "$txt" >"$TMP_OUT" 2>"$TMP_ERR"
  rc=$?

  if check_output "$ans" "$TMP_OUT"; then
    if [[ $rc -eq 0 ]]; then
      ((score++))
    else
      echo "${RED}[Failed]${RESET} $base (output matched but exit code=$rc, expected 0)"
      if [[ -s "$TMP_ERR" ]]; then
        echo "--- stderr ---"; cat "$TMP_ERR"; echo "--------------"
      fi
    fi
  else
    echo "${RED}[Failed]${RESET} $base (content mismatch)"
    echo "Exit code: $rc"
    if [[ -s "$TMP_ERR" ]]; then
      echo "--- stderr ---"; cat "$TMP_ERR"; echo "--------------"
    fi
  fi
done

echo " == Positive Score: ${GREEN}$score${RESET}/$total == "

echo
echo "${BOLD}== Negative tests (fail*.txt must exit with code 1) ==${RESET}"

fail_found=0
for txt in "$DIR"/fail*.txt; do
  [[ -e "$txt" ]] || continue
  ((fail_found++))
  ((total++))
  base="$(basename "$txt")"

  "$COMPILER" "$txt" >"$TMP_OUT" 2>"$TMP_ERR"
  rc=$?

  if [[ $rc -eq 1 ]]; then
    ((score++))
  else
    echo "${RED}[Failed]${RESET} $base (expected exit code 1, got $rc)"
    if [[ -s "$TMP_OUT" ]]; then
      echo "--- stdout ---"; cat "$TMP_OUT"; echo "--------------"
    fi
    if [[ -s "$TMP_ERR" ]]; then
      echo "--- stderr ---"; cat "$TMP_ERR"; echo "--------------"
    fi
  fi
done

if [[ $fail_found -eq 0 ]]; then
  echo "${YELLOW}[Note] No $DIR/fail*.txt negative test files found${RESET}"
fi

echo " == Negative Score: ${GREEN}$((score - (total - fail_found)))${RESET}/$fail_found == "

echo
echo "${BOLD}== Final Score ==${RESET} ${GREEN}$score${RESET}/$total"
