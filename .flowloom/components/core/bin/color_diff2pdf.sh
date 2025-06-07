#!/bin/bash

# Function to check dependencies
check_deps() {
    # Use the python in the PATH, which should be the ASDF one
    PYTHON_PATH=$(which python)
    echo "Using Python: $PYTHON_PATH"
    
    # Check if required Python packages are installed
    $PYTHON_PATH -c "import pdfkit" 2>/dev/null
    if [ $? -ne 0 ]; then
        echo "Error: Python pdfkit module is required but not found. Install it with pip:"
        echo "  pip install pdfkit"
        return 1
    fi
    
    # Check if wkhtmltopdf is installed (required by pdfkit)
    if ! command -v wkhtmltopdf &> /dev/null; then
        echo "Error: wkhtmltopdf is required but not found."
        echo "  macOS: brew install wkhtmltopdf"
        echo "  Linux: apt-get install wkhtmltopdf"
        return 1
    fi
    
    echo "All dependencies found."
    return 0
}

# Function to print usage
print_usage() {
    echo "Usage: $0 [options] [git-diff-args]"
    echo ""
    echo "Generate a color-highlighted PDF of git diff output."
    echo ""
    echo "Options:"
    echo "  -o, --output FILE    Output PDF file (default: color-diff-TIMESTAMP.pdf)"
    echo "  -h, --help           Show this help message"
    echo "  --exclude PATTERN    Exclude files matching pattern (can be used multiple times)"
    echo "  --exclude-xml        Exclude XML files (*.xml)"
    echo "  --exclude-csv        Exclude CSV files (*.csv)"
    echo ""
    echo "Examples:"
    echo "  $0                   # Compare working directory to HEAD"
    echo "  $0 HEAD~3..HEAD      # Compare 3 commits ago to HEAD"
    echo "  $0 -- file1.txt      # Show changes for specific file"
    echo "  $0 -o mydiff.pdf     # Save output to specified file"
    echo "  $0 --exclude-xml     # Exclude all XML files from diff"
    echo "  $0 --exclude .csv    # Exclude all CSV files from diff"
    echo ""
}

# Parse script options
OUTPUT_FILE=""
GIT_ARGS=""
EXCLUDE_ARGS=""
EXCLUDE_XML=""
EXCLUDE_CSV=""

while [[ $# -gt 0 ]]; do
  case $1 in
    -o|--output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    -h|--help)
      print_usage
      exit 0
      ;;
    --exclude)
      EXCLUDE_ARGS="$EXCLUDE_ARGS --exclude $2"
      shift 2
      ;;
    --exclude-xml)
      EXCLUDE_XML="--exclude-xml"
      shift
      ;;
    --exclude-csv)
      EXCLUDE_CSV="--exclude-csv"
      shift
      ;;
    *)
      GIT_ARGS="$GIT_ARGS $1"
      shift
      ;;
  esac
done

# Check dependencies
check_deps
if [ $? -ne 0 ]; then
    exit 1
fi

# Make the script executable if it isn't already
script_path="$(dirname "$0")/generate_color_diff_pdf.py"
if [ ! -x "$script_path" ]; then
    chmod +x "$script_path"
fi

# Execute the python script with the ASDF Python
PYTHON_PATH=$(which python)
cmd="$PYTHON_PATH \"$script_path\" $GIT_ARGS"
if [ -n "$OUTPUT_FILE" ]; then
    cmd="$cmd -o \"$OUTPUT_FILE\""
fi
if [ -n "$EXCLUDE_ARGS" ]; then
    cmd="$cmd $EXCLUDE_ARGS"
fi
if [ -n "$EXCLUDE_XML" ]; then
    cmd="$cmd $EXCLUDE_XML"
fi
if [ -n "$EXCLUDE_CSV" ]; then
    cmd="$cmd $EXCLUDE_CSV"
fi

eval $cmd