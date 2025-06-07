# Utility Scripts

This directory contains utility scripts for FlowLoom project development and workflow management.

## PDF Generation Scripts

### color_diff2pdf.sh
A shell script that generates color-highlighted PDFs of git diff output. This is useful for reviewing code changes in a more readable format.

**Features:**
- Color syntax highlighting of diffs
- Configurable output file naming
- Ability to exclude specific file patterns
- Built-in dependency checking

**Usage:**
```bash
./color_diff2pdf.sh [options] [git-diff-args]
```

**Options:**
- `-o, --output FILE` - Output PDF file (default: color-diff-TIMESTAMP.pdf)
- `--exclude PATTERN` - Exclude files matching pattern
- `--exclude-xml` - Exclude all XML files
- `--exclude-csv` - Exclude all CSV files

### diff2pdf.sh
A shell script that generates e-ink optimized PDFs of git diff output, suitable for e-readers or printing.

**Features:**
- E-ink friendly formatting
- Similar options to color_diff2pdf.sh
- Compatible with e-readers

**Usage:**
```bash
./diff2pdf.sh [options] [git-diff-args]
```

### generate_color_diff_pdf.py
The Python implementation used by color_diff2pdf.sh. This script handles the actual conversion of git diff output to a color-highlighted PDF.

### generate_eink_diff_pdf.py
The Python implementation used by diff2pdf.sh. This script handles the conversion of git diff output to an e-ink friendly PDF format.

## File Processing Scripts

### grepn.py
A Python utility for searching within Jupyter notebook files (*.ipynb).

**Features:**
- Search for text in notebook cell contents
- Show matching lines with customizable context
- Option to list only notebooks containing matches

**Usage:**
```bash
./grepn.py [--context N] [-l/--list] notebooks... search_text
```

**Options:**
- `--context N` - Number of context lines to show (default: 2)
- `-l, --list` - List only filenames of notebooks containing a match


## FlowLoom-Specific Scripts

### Plan Management
- `new_plan` - Create new plan files with proper structure
- `find_plan` - Search for plans containing specific keywords  
- `recent_plans` - List recently modified plan files
- `review_plans` - Review plans and show branch information

### Documentation Management
- `recent_docs` - List recently modified documentation files

### Command Management
- `sync_claude_commands.sh` - Sync Claude commands and configuration
- `check_command_updates.sh` - Check for command file updates

## Execution Notes

Most scripts can be executed directly from this directory. Ensure they have executable permissions:

```bash
chmod +x bin/<script_name>
```

Or call them explicitly with their interpreter:

```bash
bash bin/color_diff2pdf.sh
python bin/grepn.py
```