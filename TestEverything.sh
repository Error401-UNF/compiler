#!/usr/bin/env bash

COMPILER_PATH="./target/debug/COMPILER"
TESTS_DIR="./Tests"
FAILURES_LOG="failed_tests_output.log" # File to store the output of failing tests
TIMEOUT_SECONDS=1 # Set the maximum runtime in seconds

# --- Setup and Validation ---
if [ ! -f "$COMPILER_PATH" ]; then
    echo "Error: Compiler not found at $COMPILER_PATH" >&2
    exit 1
fi

if [ ! -d "$TESTS_DIR" ]; then
    echo "Error: Test directory not found at $TESTS_DIR" >&2
    exit 1
fi

# Check for the 'timeout' utility
if ! command -v timeout &> /dev/null
then
    echo "Error: 'timeout' command not found." >&2
    echo "Please install coreutils (e.g., 'sudo apt install coreutils' on Debian/Ubuntu)." >&2
    exit 1
fi

# Clean up the previous log file if it exists
> "$FAILURES_LOG"

echo "--- Starting Compiler Tests (Smart Mode with Timeout) ---"
echo "Rule: Test files with 'fail' in the filename expect non-zero exit code."
echo "Timeout: $TIMEOUT_SECONDS seconds per test."
echo "--------------------------------------------"

# Initialize counters
SUCCESS_COUNT=0
FAIL_COUNT=0
TOTAL_COUNT=0

# Loop through every .txt file in the Tests directory
for TEST_FILE in $(find "$TESTS_DIR" -type f -name "*.txt" | sort); do
    
    TOTAL_COUNT=$((TOTAL_COUNT + 1))
    TEST_NAME=$(basename "$TEST_FILE")
    
    # --- 1. Check Filename for Forced Failure ---
    EXPECT_TO_FAIL=false
    
    # Check if the filename (converted to lowercase) contains "fail"
    if [[ $(echo "$TEST_NAME" | tr '[:upper:]' '[:lower:]') == *fail* ]]; then
        EXPECT_TO_FAIL=true
    fi
    
    EXPECTATION=$(if $EXPECT_TO_FAIL; then echo "FAIL"; else echo "PASS"; fi)
    echo -n "Running test: $TEST_NAME (Expect: $EXPECTATION)... "

    # --- 2. Run Compiler with TIMEOUT ---
    # The timeout command executes the compiler.
    # It redirects all compiler output (stdout and stderr) to the $OUTPUT variable.
    OUTPUT=$(timeout --foreground $TIMEOUT_SECONDS "$COMPILER_PATH" "$TEST_FILE" 2>&1) 
    EXIT_CODE=$?

    # Check for the specific exit code of a timeout (usually 124)
    if [ $EXIT_CODE -eq 124 ]; then
        # Handle Timeout Case
        ACTUAL_RESULT="TIMEOUT"
    else
        # Handle Normal/Error Exit Case
        ACTUAL_RESULT="Exit $EXIT_CODE"
    fi

    # --- 3. Determine Result (PASS/FAIL) ---
    PASS=false
    REASON=""
    
    if [ $EXIT_CODE -eq 124 ]; then
        # Timeout always counts as a failure against expectations
        PASS=false
        REASON="Test TIMED OUT after $TIMEOUT_SECONDS seconds."
    elif $EXPECT_TO_FAIL; then
        # Expected to FAIL: passes if exit code is NOT 0
        if [ $EXIT_CODE -ne 0 ]; then
            PASS=true 
        else
            PASS=false
            REASON="Expected FAIL, but compiler PASSED (Exit 0)."
        fi
    else
        # Expected to PASS: passes if exit code IS 0
        if [ $EXIT_CODE -eq 0 ]; then
            PASS=true 
        else
            PASS=false
            REASON="Expected PASS, but compiler FAILED (Exit $EXIT_CODE)."
        fi
    fi
    
    # --- 4. Report and Log ---
    if $PASS; then
        echo -e "\e[32mPASS\e[0m" 
        SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
    else
        # This is a true failure
        echo -e "\e[31mFAIL ($ACTUAL_RESULT)\e[0m" 
        FAIL_COUNT=$((FAIL_COUNT + 1))
        
        # --- Log the Output ---
        echo "==============================================" >> "$FAILURES_LOG"
        echo "FAIL: Test $TEST_NAME" >> "$FAILURES_LOG"
        echo "Reason: $REASON" >> "$FAILURES_LOG"
        echo "--- Compiler Output (Input: $TEST_NAME) ---" >> "$FAILURES_LOG"
        # If it was a timeout, the output might be empty, so log a clear message
        if [ $EXIT_CODE -eq 124 ]; then
            echo "(Program killed by timeout after $TIMEOUT_SECONDS seconds. Output may be incomplete or empty.)" >> "$FAILURES_LOG"
        fi
        echo "$OUTPUT" >> "$FAILURES_LOG"
        echo "==============================================" >> "$FAILURES_LOG"
    fi

done

# --- Summary ---
echo "--------------------------------------------"
echo "Testing complete."
echo -e "Total Tests: $TOTAL_COUNT"
echo -e "Passed: \e[32m$SUCCESS_COUNT\e[0m"
echo -e "Failed: \e[31m$FAIL_COUNT\e[0m"
echo "--------------------------------------------"

if [ $FAIL_COUNT -gt 0 ]; then
    echo "Check the file '$FAILURES_LOG' for details on all test discrepancies."
    exit 1
else
    echo "All tests passed according to defined expectations! 🎉"
    exit 0
fi