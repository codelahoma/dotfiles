#!/bin/bash

# Function to print usage
print_usage() {
    echo "Usage: $0 [options] [git-diff-args]"
    echo ""
    echo "Generate an e-ink optimized PDF of git diff output."
    echo ""
    echo "Options:"
    echo "  -o, --output FILE    Output PDF file (default: diff-review-TIMESTAMP.pdf)"
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

# Function to check dependencies
check_deps() {
    # Check if required Python packages are installed
    python3 -c "import pdfkit" 2>/dev/null
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
    
    # Check if ansi2html is available, but don't fail if it's not
    if ! command -v ansi2html &> /dev/null; then
        echo "Note: ansi2html is not found. Using fallback conversion method."
        echo "For better output formatting, install ansi2html:"
        echo "  pip install ansi2html"
        export ANSI2HTML_AVAILABLE="false"
    else
        export ANSI2HTML_AVAILABLE="true"
        # Test if ansi2html supports --inline parameter
        echo "Test" | ansi2html --inline &>/dev/null
        if [ $? -eq 0 ]; then
            echo "Using ansi2html with --inline parameter"
            export ANSI2HTML_PARAMS="--inline"
        else 
            echo "Using ansi2html with basic parameters"
            export ANSI2HTML_PARAMS=""
        fi
    fi
    
    return 0
}

# Parse arguments - if just help is requested, show usage without running Python
if [ "$1" == "-h" ] || [ "$1" == "--help" ]; then
    print_usage
    echo "Additional options available from Python script:"
    python3 "$(dirname "$0")/generate_eink_diff_pdf.py" --help | tail -n +4
    exit 0
fi

# Check dependencies
check_deps
if [ $? -ne 0 ]; then
    exit 1
fi

# Execute the python script with all arguments passed through
python3 "$(dirname "$0")/generate_eink_diff_pdf.py" "$@"