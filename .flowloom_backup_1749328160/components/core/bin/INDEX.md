# Utility Scripts Index

This index provides an overview of all utility scripts available in the `${FLOWLOOM_WORK_DIR:-.meta-claude}/bin` directory.

## Project Management Scripts

### `find_plan`
- Searches for specific plan files in the project.
- Useful for quickly finding plan documents.

### `new_plan`
- Creates a new plan document with appropriate structure.
- Helps maintain consistent planning documentation.

### `recent_docs`
- Lists recently modified documentation files.
- Helps track recent documentation changes.

### `recent_plans`
- Lists recently modified plan files.
- Useful for reviewing current planning status.

### `review_plans`
- Tool for systematically reviewing existing plans.
- Helps with plan assessment and updates.

## Code Visualization Scripts

### `color_diff2pdf.sh`
- Generates color-highlighted PDFs of git diff output.
- Useful for reviewing code changes in a readable format.

### `diff2pdf.sh`
- Creates e-ink optimized PDFs of git diff output.
- Suitable for e-readers or printing.

### `generate_color_diff_pdf.py`
- Python implementation used by color_diff2pdf.sh.
- Handles conversion of git diff output to color-highlighted PDF.

### `generate_eink_diff_pdf.py`
- Python implementation used by diff2pdf.sh.
- Converts git diff output to e-ink friendly PDF format.

## Documentation Scripts

### `build_2fa_docs`
- Builds documentation related to two-factor authentication features.
- Generates structured documentation from source files.

### `finalize_2fa_docs`
- Finalizes and formats 2FA documentation for publishing.
- Ensures documentation quality and consistency.

### `verify_2fa_docs_links`
- Verifies the integrity of links within 2FA documentation.
- Helps maintain documentation quality.

## File Processing Scripts

### `grepn.py`
- Utility for searching within Jupyter notebook files.
- Provides context-aware search results.

### `convert_daily_logs.sh`
- Processes daily log files, converting them to a consistent format.
- Helps with log management and analysis.

### `copy_files.sh`
- Utility for copying files with specific patterns or configurations.
- Streamlines file management tasks.

## Usage Notes

Most scripts can be executed directly from this directory. Ensure they have executable permissions:

```bash
chmod +x ${FLOWLOOM_WORK_DIR:-.meta-claude}/bin/<script_name>
```

Or call them explicitly with their interpreter:

```bash
bash ${FLOWLOOM_WORK_DIR:-.meta-claude}/bin/color_diff2pdf.sh
python ${FLOWLOOM_WORK_DIR:-.meta-claude}/bin/grepn.py
```

For more detailed information on specific scripts, refer to the README.md in this directory
or check the script files themselves for documentation and usage examples.