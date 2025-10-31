#!/bin/bash

# Define paths
COMPILER_PATH="./target/debug/COMPILER"
TESTS_DIR="./Tests"
FAILURES_LOG="failed_tests_output.log" # File to store the output of failing tests

# --- Setup and Validation ---
if [ ! -f "$COMPILER_PATH" ]; then
    echo "Error: Compiler not found at $COMPILER_PATH" >&2
    exit 1
fi

if [ ! -d "$TESTS_DIR" ]; then
    echo "Error: Test directory not found at $TESTS_DIR" >&2
    exit 1
fi

# Clean up the previous log file if it exists
> "$FAILURES_LOG"

echo "--- Starting Compiler Tests ---"
echo "Compiler: $COMPILER_PATH"
echo "Test Directory: $TESTS_DIR"
echo "-------------------------------"

# Initialize counters
SUCCESS_COUNT=0
FAIL_COUNT=0
TOTAL_COUNT=0

# Loop through every .txt file in the Tests directory
for TEST_FILE in $(find "$TESTS_DIR" -type f -name "*.txt" | sort); do
    
    TOTAL_COUNT=$((TOTAL_COUNT + 1))
    TEST_NAME=$(basename "$TEST_FILE")

    echo -n "Running test: $TEST_NAME... "

    # Execute the compiler and CAPTURE ITS OUTPUT to a temporary variable
    # We use 'tee' with a pipe to capture output AND check the exit code later
    
    # Run the compiler, piping its output to the terminal AND a temporary variable
    # To capture output accurately, we must redirect both stdout and stderr
    OUTPUT=$("$COMPILER_PATH" "$TEST_FILE" 2>&1 >/dev/null) # Run silently first to check exit code
    EXIT_CODE=$?
    
    if [ $EXIT_CODE -eq 0 ]; then
        # Success
        echo -e "\e[32mPASS\e[0m" 
        SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
    else
        # Failure: Rerun the command, capture output, and log it
        echo -e "\e[31mFAIL (Exit Code: $EXIT_CODE)\e[0m" 
        FAIL_COUNT=$((FAIL_COUNT + 1))
        
        # --- Log the Output ---
        echo "==============================================" >> "$FAILURES_LOG"
        echo "FAIL: Test $TEST_NAME (Exit Code: $EXIT_CODE)" >> "$FAILURES_LOG"
        echo "--- Compiler Output ---" >> "$FAILURES_LOG"
        
        # Rerun the command and append the actual output to the log file
        "$COMPILER_PATH" "$TEST_FILE" 2>&1 >> "$FAILURES_LOG"
        echo "==============================================" >> "$FAILURES_LOG"
        # --- End Logging ---
    fi

done

# --- Summary ---
echo "-------------------------------"
echo "Testing complete."
echo "Total Tests: $TOTAL_COUNT"
echo "Passed: \e[32m$SUCCESS_COUNT\e[0m"
echo "Failed: \e[31m$FAIL_COUNT\e[0m"
echo "-------------------------------"

if [ $FAIL_COUNT -gt 0 ]; then
    echo "Check the file '$FAILURES_LOG' for the output of all failing tests."
    exit 1
else
    echo "All tests passed successfully!"
    exit 0
fi