#!/usr/bin/env python3

import pdfkit
import subprocess
import os
import argparse
import re
from datetime import datetime
from html import escape

def manual_diff_to_html(diff_text, repo_root=None):
    """
    Convert git diff output to HTML manually, optimized for e-ink display.
    
    Args:
        diff_text (str): Raw git diff output
        repo_root (str, optional): Git repository root path
        
    Returns:
        tuple: (HTML formatted diff, list of files for TOC)
    """
    # If repo_root wasn't provided, try to detect it
    if repo_root is None:
        repo_root = get_git_root()
    lines = diff_text.split('\n')
    html = []
    
    # Keep track of current file and line numbers
    current_file = None
    in_file_section = False
    
    # For tracking line numbers (reset for each file)
    line_number = 0
    source_line_nums = {}  # Mapping of hunk starting points
    current_src_line = 0
    
    # For table of contents
    toc_entries = []
    
    # Start a preformatted block for monospace display
    html.append('<div class="diff-content">')
    
    for line in lines:
        escaped_line = escape(line)
        
        if line.startswith('diff --git'):
            # Close previous file section if exists
            if in_file_section:
                html.append('</pre>')
                html.append('</div>')
            
            # Extract file name for header
            parts = line.split()
            if len(parts) >= 3:
                full_path = parts[2].lstrip('a/')
                
                # Make relative path by removing repo root if it's in the path
                if repo_root and full_path.startswith(repo_root):
                    current_file = full_path[len(repo_root):].lstrip('/')
                elif '/' in full_path:
                    # Try to extract just the filename and one directory level
                    path_parts = full_path.split('/')
                    if len(path_parts) > 2:
                        current_file = '/'.join(path_parts[-2:])  # Last two parts of path
                    else:
                        current_file = full_path
                else:
                    current_file = full_path
            else:
                current_file = "Unknown file"
                
            # Reset line counters for new file
            line_number = 0
            source_line_nums = {}
            current_src_line = 0
                
            # Add to TOC (use file ID based on position for anchor links)
            file_id = f"file-{len(toc_entries) + 1}"
            toc_entries.append((file_id, current_file))
            
            # Start new file section with ID for TOC linking
            html.append(f'<div id="{file_id}" class="file-section">')
            
            # Add file header and start content
            html.append(f'<div class="diff-header">{current_file}</div>')
            html.append(f'<pre class="file-content">')
            in_file_section = True
            
            # Add the diff --git line (no line number)
            html.append(f'<span class="full-line index-line">{escaped_line}</span>')
            
        elif line.startswith('@@'):
            # Extract line numbers from hunk header
            # Format: @@ -l,s +l,s @@ optional section heading
            match = re.match(r'^@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@', line)
            if match:
                src_start = int(match.group(1))
                dest_start = int(match.group(2))
                current_src_line = dest_start
            
            html.append(f'<span class="full-line hunk-header">{escaped_line}</span>')
            
        elif line.startswith('---') or line.startswith('+++') or line.startswith('index '):
            # Metadata lines - no line numbers
            html.append(f'<span class="full-line file-path">{escaped_line}</span>')
            
        elif line.startswith('-'):
            # This is a removed line - show source line number only
            html.append(f'<span class="line-num line-num-removed">{current_src_line}</span><span class="line-content line-removed">{escaped_line}</span>')
            current_src_line += 1  # Increment source line
            
        elif line.startswith('+'):
            # This is an added line - show destination line number
            line_number += 1
            html.append(f'<span class="line-num">{line_number}</span><span class="line-content line-added">{escaped_line}</span>')
            
        else:
            # Context line - show line number
            if not line.strip() and len(lines) > 0:
                # Empty lines still get line numbers
                line_number += 1
                current_src_line += 1
                html.append(f'<span class="line-num">{line_number}</span><span class="line-content context-line">{escaped_line}</span>')
            elif line.strip():
                # Non-empty context line
                line_number += 1
                current_src_line += 1
                html.append(f'<span class="line-num">{line_number}</span><span class="line-content context-line">{escaped_line}</span>')
    
    # Close last file section if exists
    if in_file_section:
        html.append('</pre>')
        html.append('</div>')
    
    html.append('</div>')
    
    return '\n'.join(html), toc_entries

def get_git_root():
    """
    Get the git repository root directory.
    
    Returns:
        str: The absolute path to the git repository root
    """
    try:
        root = subprocess.check_output(
            "git rev-parse --show-toplevel", 
            shell=True, 
            text=True
        ).strip()
        return root
    except subprocess.CalledProcessError:
        # Fallback if git command fails
        return os.getcwd()

def generate_eink_diff_pdf(diff_args=None, output_file=None, diff_text=None):
    """
    Generate an e-ink friendly PDF from git diff output.
    
    Args:
        diff_args (str, optional): Arguments to pass to git diff command
        output_file (str, optional): Output PDF filename, defaults to timestamp-based name
        diff_text (str, optional): Raw diff text to use instead of running git diff
    """
    timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
    if not output_file:
        output_file = f"diff-review-{timestamp}.pdf"
    
    # Generate git diff output as HTML
    if diff_text is None:
        print(f"Generating diff for: {diff_args}...")
        
        # Get the plain diff without color formatting
        try:
            raw_diff = subprocess.check_output(f"git diff {diff_args}", shell=True, text=True)
        except subprocess.CalledProcessError as e:
            print(f"Error generating diff: {e}")
            return None
    else:
        # Use provided diff text
        raw_diff = diff_text
    
    # Get the git repository root
    repo_root = get_git_root()
    
    # Always use our custom HTML generator - more reliable for e-ink formatting
    print("Using custom e-ink optimized diff conversion...")
    diff_html, toc_entries = manual_diff_to_html(raw_diff, repo_root)
    
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
    <title>Git Diff Review: {timestamp}</title>
    <style>
        @page {{
            size: letter landscape;
            margin: 10mm 10mm 10mm 8mm;
            orphans: 3;   /* Minimum number of lines to keep at bottom of a page */
            widows: 3;    /* Minimum number of lines to keep at top of a page */
        }}
        body {{
            font-family: 'Courier New', monospace;
            font-weight: 600;  /* Increased for better e-ink visibility */
            line-height: 1.05;
            color: #000000;
            background-color: #ffffff;
            padding: 0;
            margin: 0;
            font-size: 16pt; /* Increased from base 11pt */
        }}
        pre, code, tt {{
            margin: 0;
            padding: 2px 0 2px 0;  /* Further reduced vertical padding */
            white-space: pre;  /* Don't wrap lines - preserve spacing */
            font-family: 'Courier New', monospace;
            font-weight: 600;  /* Increased for better e-ink visibility */
            font-size: 16pt; /* Increased from 11pt */
            line-height: 1.05;
            page-break-inside: avoid;  /* Prevent breaking within PRE elements */
        }}
        .ansi2html-content, .diff-content {{
            padding: 5px 0px 5px 0px;  /* Further reduced padding */
            font-family: 'Courier New', monospace;
            white-space: pre-wrap;  /* Allow wrapping while preserving spaces */
            word-wrap: break-word;   /* Break long words if necessary */
            overflow-x: visible;     /* No scrolling */
        }}
        
        /* Manual diff HTML styles */
        .diff-content {{
            font-family: 'Courier New', monospace;
        }}
        .file-section {{
            margin-bottom: 20px; /* Add more vertical space between files */
            padding-bottom: 10px; /* Add padding at bottom */
            border-bottom: 1px dashed #ccc; /* Add subtle separator */
            page-break-inside: avoid; /* Try to avoid breaking within a file section */
            page-break-after: auto;
        }}
        .diff-header {{
            font-family: 'Courier New', monospace;
            font-weight: 800; /* Increased for better e-ink visibility */
            font-size: 18pt; /* Increased from 12pt */
            padding: 5px 0 2px 0;
            margin-top: 15px; /* Slightly larger to separate files */
            border-bottom: 1px solid #000000;
            page-break-after: avoid; /* Keep header with content */
        }}
        .file-content {{
            white-space: pre-wrap;
            font-family: 'Courier New', monospace;
            font-weight: 600;  /* Increased for better e-ink visibility */
            font-size: 16pt; /* Increased from 11pt */
            line-height: 1.05;
            padding: 0;
            margin: 0;
            overflow-x: visible;
        }}
        .full-line {{
            display: block;
            width: 100%;
            line-height: 1.0;  /* Slightly reduced */
            margin: 0;  /* No margin */
            page-break-inside: avoid;
        }}
        .file-path {{
            font-weight: 600;  /* Increased for better e-ink visibility */
            padding: 0;
            font-size: 14pt;  /* Increased from 10pt */
            color: #555;
        }}
        .hunk-header {{
            font-weight: 700; /* Increased for better e-ink visibility */
            padding: 0;
            border-top: 1px dotted #888888; /* Changed to dotted for less visual weight */
            margin-top: 4px; /* Reduced from 8px */
            margin-bottom: 0px;
            page-break-before: auto;
            page-break-after: avoid;
            font-size: 14pt; /* Increased from 10pt */
        }}
        /* Line number styles */
        .line-num {{
            display: inline-block;
            width: 3em;  /* Reduced from 4em */
            text-align: right;
            padding-right: 0.3em;  /* Reduced from 1em */
            margin-right: 0.3em;  /* Reduced from 0.5em */
            border-right: 1px solid #888;
            color: #555;
            font-weight: 500;
            -webkit-user-select: none;
            user-select: none;
        }}
        .line-num-removed {{
            color: #777;
            font-style: italic;
        }}
        .line-content {{
            display: inline;
            white-space: pre-wrap;      /* Allow line wrapping */
            word-wrap: break-word;      /* Break long words if needed */
            page-break-inside: avoid;   /* Prevent breaking within lines */
            page-break-after: auto;     /* Allow breaks after lines */
        }}
        /* Line styles */
        .line-removed {{
            font-style: italic;
            font-weight: 600;  /* Increased for better e-ink visibility */
            text-decoration: line-through;
        }}
        .line-added {{
            font-style: normal;
            font-weight: 700;  /* Increased for better e-ink visibility */
        }}
        .context-line {{
            font-weight: 500;  /* Increased for better e-ink visibility */
        }}
        .index-line {{
            font-weight: 600;  /* Increased for better e-ink visibility */
            font-size: 14pt;   /* Increased from 10pt */
            color: #555;
        }}
        
        /* Table of Contents Styles */
        .toc {{
            margin-bottom: 30px;
            page-break-after: always;
        }}
        .toc h2 {{
            font-size: 22pt; /* Increased from 16pt */
            font-weight: 800; /* Increased for better e-ink visibility */
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
            font-family: 'Courier New', monospace;
            font-size: 16pt; /* Increased from 11pt */
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
        }}
        .toc-list a {{
            text-decoration: none;
            color: #000;
            font-weight: 600;  /* Increased for better e-ink visibility */
        }}
        
        /* ansi2html styles */
        /* Override any color-based spans from ansi2html with our eink-friendly styles */
        .ansi1, .ansi31, .ansi41 {{
            /* For deletions - use italic instead of red */
            font-style: italic;
            text-decoration: line-through;
            font-weight: 600;  /* Increased for better e-ink visibility */
        }}
        .ansi32, .ansi42 {{
            /* For additions - use normal font weight but bolded */
            font-style: normal;
            font-weight: 700;  /* Increased for better e-ink visibility */
        }}
        /* Hide any colors with black text on light background for maximum contrast */
        .ansi30, .ansi31, .ansi32, .ansi33, .ansi34, .ansi35, .ansi36, .ansi37,
        span[style*="color"] {{
            color: #000000 !important;
        }}
        .ansi40, .ansi41, .ansi42, .ansi43, .ansi44, .ansi45, .ansi46, .ansi47,
        span[style*="background"] {{
            background-color: #ffffff !important;
        }}
        .ansi-default-background-color {{
            background-color: #ffffff !important;
        }}
        .ansi-default-text-color {{
            color: #000000 !important;
        }}
        /* Add file headers styling */
        .file-header {{
            font-weight: 700;
            font-size: 13pt;
            padding: 10px 0;
            margin-top: 20px;
            border-bottom: 2px solid #000000;
        }}
        /* These styles are already defined above, removing redundant definition */
    </style>
</head>
<body>
    <h1 style="margin: 0 0 2px 0; font-size: 20pt;">Git Diff Review</h1>
    <p style="margin: 0; font-size: 14pt;">Generated: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")} | Command: <code>git diff {diff_args}</code></p>
    
    {''.join(toc_html)}
    
    {diff_html}
</body>
</html>"""

    # We no longer need this post-processing as our manual HTML generation
    # already has proper structure for file sections
    
    # Write HTML to temp file
    temp_html = f"temp_diff_{timestamp}.html"
    with open(temp_html, 'w') as f:
        f.write(html_content)
    
    # Configure PDFKit options - simple set to avoid errors
    options = {
        'page-size': 'Letter',
        'orientation': 'Landscape',
        'margin-top': '10mm',
        'margin-right': '10mm',
        'margin-bottom': '10mm',
        'margin-left': '8mm',  # Reduced left margin
        'encoding': 'UTF-8',
        'no-outline': None,
        'enable-local-file-access': None,
        'dpi': 300,  # Higher DPI for better quality on e-ink
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
            import fnmatch
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

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Generate an e-ink friendly PDF from git diff output')
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
    
    # Generate diff first
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
            
            # If differences remain after filtering, use the filtered diff directly
            diff_html, toc_entries = manual_diff_to_html(filtered_diff, get_git_root())
            output_file = generate_eink_diff_pdf(args.diff_args, args.output, filtered_diff)
        else:
            # No filtering needed
            output_file = generate_eink_diff_pdf(args.diff_args, args.output)
    except subprocess.CalledProcessError as e:
        print(f"Error running git diff: {e}")
        exit(1)