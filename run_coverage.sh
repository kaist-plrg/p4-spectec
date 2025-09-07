#!/bin/bash

print_usage() {
    echo "Usage: $0 -o output_dir directory1 [directory2 ...]"
    echo "Options:"
    echo "  -o output_dir    (Required) Specify output directory for coverage data"
    echo "  -n, --dry-run    Print list of .p4 files to be tested and exit"
    exit 1
}

cleanup() {
    echo
    echo "⚠️  Caught interrupt signal. Exiting gracefully..."
    exit 130  # 128 + SIGINT
}
trap cleanup SIGINT SIGTERM

OUTPUT_DIR=""
DRY_RUN=false

while getopts "o:hn" opt; do
    case $opt in
        o) OUTPUT_DIR="$OPTARG" ;;
        h) print_usage ;;
        n) DRY_RUN=true ;;
        \?) print_usage ;;
    esac
done


shift $((OPTIND-1))

# Ensure -o was provided
if [ -z "$OUTPUT_DIR" ]; then
    echo "Error: -o output_dir is required."
    print_usage
fi


# Ensure at least one positional directory is provided
if [ $# -eq 0 ]; then
    echo "Error: At least one test directory must be specified."
    print_usage
fi

# Clean up existing coverage data
echo "Initializing coverage..."
find ./p4c -name "*.gcda" -delete
mkdir -p "$OUTPUT_DIR"

# Load exclusion list
EXCLUDES=()
if [ -d "./excludes" ]; then
    while IFS= read -r line; do
        [[ -n "$line" ]] && EXCLUDES+=("$line")
    done < <(find ./excludes -type f -exec cat {} \;)
fi

is_excluded() {
    local file="$1"
    for exclude in "${EXCLUDES[@]}"; do
        if [[ "$file" == *"$exclude"* ]]; then
            return 0
        fi
    done
    return 1
}

run_p4tests() {
    local dir="$1"
    echo "Processing directory: $dir"

    # Step 1: Find .p4 files, exclude "/include/", sort by alias name (not realpath)
    mapfile -t found_files < <(find -L "$dir" -name "*.p4" -type f | grep -v "/include/" | sort)

    filtered_files=()
    for file in "${found_files[@]}"; do
        if ! is_excluded "$file"; then
            # Resolve only after passing exclusion check
            real_file=$(realpath "$file")
            filtered_files+=("$real_file")
        fi
    done

    echo "Found ${#filtered_files[@]} .p4 files to test."

    if $DRY_RUN; then
        echo "---- Dry Run Mode ----"
        printf "%s\n" "${filtered_files[@]}"
        echo "Total: ${#filtered_files[@]} .p4 files would be tested."
        echo "----------------------"
        return
    fi

    for p4_file in "${filtered_files[@]}"; do
        echo "Running p4test on: $p4_file"
        if p4test "$p4_file" > /dev/null 2>&1; then
	    echo "✅ Passed"
	else 
	    echo "❌ Failed"
        fi
    done
}
# Process each directory provided
for dir in "$@"; do
    if [ ! -d "$dir" ]; then
        echo "Warning: $dir is not a directory, skipping..."
        continue
    fi
    run_p4tests "$dir"
done

if $DRY_RUN; then
    echo "Dry run completed."
    exit 0
fi

# Cleanup
# Copy .gcda files
echo "Copying coverage data files..."
gcda_count=$(find ./p4c -name "*.gcda" | wc -l)
echo "Found $gcda_count .gcda files to copy"
find ./p4c -name "*.gcda" -exec cp --parents {} "$OUTPUT_DIR" \;

# Copy .gcno files
gcno_count=$(find ./p4c -name "*.gcno" | wc -l)
echo "Found $gcno_count .gcno files to copy"
find ./p4c -name "*.gcno" -exec cp --parents {} "$OUTPUT_DIR" \;

# Generate coverage report
echo "Generating coverage report..."
gcovr --root "$OUTPUT_DIR" \
  --filter '.*frontends/common/' \
  --filter '.*frontends/p4/' \
  --filter '.*midend/' \
  --txt-metric=branch \
  --gcov-ignore-errors=no_working_dir_found \
  --gcov-ignore-parse-errors=suspicious_hits.warn_once_per_file \
  -o "$OUTPUT_DIR/coverage_report.gcovr" 2> /dev/null

echo "Coverage report generated at $OUTPUT_DIR/coverage_report.gcovr"

