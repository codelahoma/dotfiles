#!/usr/bin/env python3

import pdfkit
import subprocess
import os
import argparse
import re
import fnmatch
from datetime import datetime
from html import escape

def get_git_diff(diff_args):
    """
    Get plain diff output from git.
    
    Args:
        diff_args (str): Arguments to pass to git diff command
        
    Returns:
        str: Plain diff output from git
    """
    try:
        cmd = ['git', 'diff']
        if diff_args:
            cmd.extend(diff_args.split())
        
        result = subprocess.run(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        
        if result.returncode != 0:
            print(f"Warning: git diff returned non-zero exit code: {result.returncode}")
            print(f"Error message: {result.stderr}")
        
        return result.stdout
    except Exception as e:
        print(f"Error running git diff: {e}")
        return ""

def is_excluded_file(file_path, exclude_patterns):
    """
    Check if a file should be excluded based on patterns.
    
    Args:
        file_path (str): File path to check
        exclude_patterns (list): List of patterns to exclude
        
    Returns:
        bool: True if file should be excluded, False otherwise
    """
    if not exclude_patterns:
        return False
        
    for pattern in exclude_patterns:
        if pattern.endswith('/'):
            # Directory pattern
            if pattern[:-1] in file_path:
                return True
        elif '*' in pattern:
            # Wildcard pattern
            if fnmatch.fnmatch(file_path, pattern):
                return True
        else:
            # Exact match or extension
            if pattern in file_path or file_path.endswith(pattern):
                return True
    
    return False

def filter_diff_output(diff_text, exclude_patterns):
    """
    Filter diff output to exclude certain files.
    
    Args:
        diff_text (str): Raw git diff output
        exclude_patterns (list): List of patterns to exclude
        
    Returns:
        str: Filtered diff output
    """
    if not exclude_patterns:
        return diff_text
        
    lines = diff_text.split('\n')
    filtered_lines = []
    
    skip_current_file = False
    current_file = None
    
    for line in lines:
        if line.startswith('diff --git'):
            # New file - check if it should be excluded
            parts = line.split()
            if len(parts) >= 3:
                current_file = parts[2].lstrip('a/')
                skip_current_file = is_excluded_file(current_file, exclude_patterns)
            else:
                skip_current_file = False
        
        if not skip_current_file:
            filtered_lines.append(line)
    
    return '\n'.join(filtered_lines)

def diff_to_html(diff_text):
    """
    Convert git diff output to HTML with colored indicators and line numbers.
    
    This function parses the raw git diff output and converts it to HTML format
    with colored indicators for added/removed lines and proper line numbering.
    
    Args:
        diff_text (str): Raw git diff output from git command
        
    Returns:
        tuple: (HTML formatted diff, list of files for TOC)
    """
    lines = diff_text.split('\n')
    html = []
    toc_entries = []
    
    # Track current file
    current_file = None
    in_file_section = False
    file_count = 0
    
    # For line numbering
    line_number = 0
    source_line = 0
    
    for line in lines:
        # Escape HTML special characters
        escaped_line = escape(line)
        
        # New file section starts with "diff --git"
        if line.startswith('diff --git'):
            # Close previous file section if it exists
            if in_file_section:
                html.append('</div>')
            
            # Extract file name
            match = re.search(r'diff --git a/(.*) b/(.*)', line)
            if match:
                current_file = match.group(2)
                
                # Create a shorter display name for TOC
                if '/' in current_file:
                    path_parts = current_file.split('/')
                    if len(path_parts) > 2:
                        display_name = '/'.join(path_parts[-2:])
                    else:
                        display_name = current_file
                else:
                    display_name = current_file
                
                file_count += 1
                file_id = f"file-{file_count}"
                toc_entries.append((file_id, display_name))
                
                # Start new file section
                html.append(f'<div id="{file_id}" class="file-section">')
                html.append(f'<div class="file-header">{display_name}</div>')
                html.append('<div class="file-content">')
                in_file_section = True
                
                # Reset line counters
                line_number = 0
                source_line = 0
                
                # Add metadata line
                html.append(f'<div class="meta-line">{escaped_line}</div>')
            else:
                # Fallback if regex doesn't match
                html.append(f'<div class="meta-line">{escaped_line}</div>')
        
        # File header metadata lines
        elif line.startswith('---') or line.startswith('+++') or line.startswith('index '):
            html.append(f'<div class="meta-line">{escaped_line}</div>')
        
        # Chunk header (e.g., @@ -1,5 +1,5 @@)
        elif line.startswith('@@'):
            # Extract line numbers
            match = re.match(r'^@@ -(\d+),(\d+) \+(\d+),(\d+) @@', line)
            if match:
                source_line = int(match.group(3))
                line_number = source_line
            
            html.append(f'<div class="chunk-header">{escaped_line}</div>')
        
        # Removed line
        elif line.startswith('-'):
            html.append(f'<div class="line-removed"><span class="line-num-removed">{source_line}</span>{escaped_line}</div>')
            source_line += 1
        
        # Added line
        elif line.startswith('+'):
            html.append(f'<div class="line-added"><span class="line-num-added">{line_number}</span>{escaped_line}</div>')
            line_number += 1
        
        # Context line
        else:
            html.append(f'<div class="line-context"><span class="line-num">{line_number}</span>{escaped_line}</div>')
            if line.strip():  # Only increment for non-empty lines
                line_number += 1
                source_line += 1
    
    # Close the last file section if it exists
    if in_file_section:
        html.append('</div>')
        html.append('</div>')
    
    return '\n'.join(html), toc_entries

def generate_color_diff_pdf(diff_args=None, output_file=None, diff_text=None, exclude_patterns=None):
    """
    Generate a color PDF from git diff output.
    
    Args:
        diff_args (str, optional): Arguments to pass to git diff command (not used if diff_text provided)
        output_file (str, optional): Output PDF filename, defaults to timestamp-based name
        diff_text (str, optional): Raw diff text to use instead of running git diff
        exclude_patterns (list, optional): List of patterns to exclude from diff
    """
    timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
    if not output_file:
        output_file = f"color-diff-{timestamp}.pdf"
    
    # Generate git diff output if not provided
    if diff_text is None:
        print(f"Generating diff for: {diff_args}...")
        diff_text = get_git_diff(diff_args)
    
    # Apply exclusion filters if specified
    if exclude_patterns:
        print(f"Excluding files matching patterns: {', '.join(exclude_patterns)}")
        diff_text = filter_diff_output(diff_text, exclude_patterns)
    
    if not diff_text.strip():
        print("No changes found in diff output.")
        return None
    
    # Convert diff to HTML
    print("Converting diff to HTML...")
    diff_html, toc_entries = diff_to_html(diff_text)
    
    # Generate the table of contents HTML
    toc_html = []
    if toc_entries:
        toc_html.append('<div class="toc">')
        toc_html.append('<h2>Table of Contents</h2>')
        toc_html.append('<ul class="toc-list">')
        for file_id, file_name in toc_entries:
            toc_html.append(f'<li><a href="#{file_id}">{file_name}</a></li>')
        toc_html.append('</ul>')
        toc_html.append('</div>')
    
    # Create a complete HTML document with our custom styles
    html_content = f"""<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Git Diff Review (Color): {timestamp}</title>
    <style>
        /* Page setup */
        @page {{
            size: letter landscape;
            margin: 10mm 10mm 10mm 8mm;
            orphans: 3;
            widows: 3;
        }}
        body {{
            font-family: 'Courier New', monospace;
            line-height: 1.2;
            background-color: #ffffff;
            padding: 0;
            margin: 0;
            font-size: 12pt;
        }}
        
        /* Headers */
        h1, h2 {{
            font-family: Arial, sans-serif;
        }}
        
        /* File sections */
        .file-section {{
            margin-bottom: 20px;
            padding-bottom: 10px;
            border-bottom: 1px dashed #ccc;
            page-break-inside: avoid;
            page-break-after: auto;
        }}
        .file-header {{
            font-family: Arial, sans-serif;
            font-weight: bold;
            font-size: 14pt;
            padding: 5px 0;
            margin: 15px 0 5px 0;
            border-bottom: 1px solid #000000;
            page-break-after: avoid;
            color: #0066cc;
        }}
        .file-content {{
            font-family: 'Courier New', monospace;
            white-space: pre-wrap;
            margin: 0;
            padding: 0;
        }}
        
        /* Line styles */
        .meta-line {{
            color: #666666;
            font-style: italic;
            padding: 2px 0;
        }}
        .chunk-header {{
            color: #8a2be2;  /* blueviolet */
            font-weight: bold;
            padding: 2px 0;
            margin: 5px 0 2px 0;
            border-top: 1px dotted #999;
        }}
        .line-context, .line-added, .line-removed {{
            padding: 1px 0;
            white-space: pre-wrap;
        }}
        .line-added {{
            border-left: 5px solid #00aa00;
            padding-left: 5px;
            background-color: rgba(0, 170, 0, 0.05);  /* Very light green background */
        }}
        .line-removed {{
            text-decoration: line-through;
            border-left: 5px solid #cc0000;
            padding-left: 5px;
            background-color: rgba(204, 0, 0, 0.05);  /* Very light red background */
        }}
        .line-context {{
            border-left: 5px solid transparent;
            padding-left: 5px;
        }}
        
        /* Line numbers */
        .line-num, .line-num-added, .line-num-removed {{
            display: inline-block;
            width: 3em;
            text-align: right;
            padding-right: 1em;
            margin-right: 0.5em;
            border-right: 1px solid #ccc;
            color: #888;
            user-select: none;
        }}
        .line-num-added {{
            color: #00aa00;
            font-weight: bold;
        }}
        .line-num-removed {{
            color: #cc0000;
            font-weight: bold;
        }}
        
        /* Table of Contents */
        .toc {{
            margin-bottom: 30px;
            page-break-after: always;
        }}
        .toc h2 {{
            font-size: 18pt;
            margin: 5px 0;
            border-bottom: 2px solid #000;
            padding-bottom: 5px;
        }}
        .toc-list {{
            list-style-type: none;
            padding: 0;
            margin: 0;
            columns: 2;
            column-gap: 30px;
        }}
        .toc-list li {{
            margin: 5px 0;
            padding: 2px 0;
            line-height: 1.3;
            page-break-inside: avoid;
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
        }}
        .toc-list a {{
            text-decoration: none;
            color: #0066cc;
            font-family: Arial, sans-serif;
        }}
    </style>
</head>
<body>
    <h1 style="margin: 0 0 2px 0; font-size: 18pt;">Git Diff Review (Color)</h1>
    <p style="margin: 0 0 15px 0; font-size: 12pt; color: #666;">
        Generated: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")} | 
        Command: <code>git diff {diff_args}</code>
    </p>
    
    {''.join(toc_html)}
    
    <div class="diff-content">
        {diff_html}
    </div>
</body>
</html>"""

    # Write HTML to temp file
    temp_html = f"temp_color_diff_{timestamp}.html"
    with open(temp_html, 'w') as f:
        f.write(html_content)
    
    # Also save the HTML for debugging
    debug_html = f"color-diff-{timestamp}.html"
    with open(debug_html, 'w') as f:
        f.write(html_content)
    print(f"HTML saved to: {debug_html} (for debugging)")
    
    # Configure PDFKit options
    options = {
        'page-size': 'Letter',
        'orientation': 'Landscape',
        'margin-top': '10mm',
        'margin-right': '10mm',
        'margin-bottom': '10mm',
        'margin-left': '8mm',
        'encoding': 'UTF-8',
        'no-outline': None,
        'enable-local-file-access': None,
        'dpi': 300,
        'quiet': ''
    }
    
    # Create the PDF
    print(f"Generating PDF: {output_file}...")
    try:
        pdfkit.from_file(temp_html, output_file, options=options)
        print(f"PDF generated successfully! Output file: {output_file}")
    except Exception as e:
        print(f"Error generating PDF: {e}")
    
    # Clean up temporary files
    if os.path.exists(temp_html):
        os.remove(temp_html)
    
    return output_file

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Generate a color PDF from git diff output')
    parser.add_argument('diff_args', nargs='?', default='HEAD', 
                        help='Arguments to pass to git diff command (default: HEAD)')
    parser.add_argument('-o', '--output', help='Output PDF filename')
    parser.add_argument('--exclude', nargs='+', default=[],
                        help='Patterns to exclude (e.g. .xml .csv prompts/)')
    parser.add_argument('--exclude-xml', action='store_true',
                        help='Exclude XML files (*.xml)')
    parser.add_argument('--exclude-csv', action='store_true',
                        help='Exclude CSV files (*.csv)')
    args = parser.parse_args()
    
    # Build exclusion patterns
    exclude_patterns = list(args.exclude)
    if args.exclude_xml:
        exclude_patterns.append('.xml')
    if args.exclude_csv:
        exclude_patterns.append('.csv')
    
    # Generate diff
    try:
        raw_diff = subprocess.check_output(f"git diff {args.diff_args}", shell=True, text=True)
        
        # Apply exclusion filters
        if exclude_patterns:
            print(f"Excluding files matching patterns: {', '.join(exclude_patterns)}")
            filtered_diff = filter_diff_output(raw_diff, exclude_patterns)
            
            if not filtered_diff.strip():
                print("Warning: No content left after filtering!")
                if input("Continue anyway? (y/n): ").lower() != 'y':
                    exit(0)
            
            # If differences remain after filtering, proceed
            output_file = generate_color_diff_pdf(diff_args=args.diff_args, diff_text=filtered_diff, output_file=args.output)
        else:
            # No filtering needed
            output_file = generate_color_diff_pdf(diff_args=args.diff_args, output_file=args.output)
    except subprocess.CalledProcessError as e:
        print(f"Error running git diff: {e}")
        exit(1)