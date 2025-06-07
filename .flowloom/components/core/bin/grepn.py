#!/usr/bin/env python3

import argparse
import json
from pathlib import Path


def search_notebook(notebook_path, search_text, context_lines=2, list_only=False):
    """
    Search for a text string within the cells of a Jupyter Notebook (.ipynb) file,
    optionally listing only filenames that contain matches.

    Parameters
    ----------
    notebook_path : Path
        The path to the .ipynb file.
    search_text : str
        The text to search for within the notebook cells.
    context_lines : int
        Number of context lines to show before and after a match.
    list_only : bool
        If True, only list the filename of notebooks that contain matches.

    Returns
    -------
    bool
        True if a match is found, False otherwise.
    """
    with open(notebook_path, encoding="utf-8") as f:
        notebook = json.load(f)

    match_found = False

    for i, cell in enumerate(notebook.get("cells", [])):
        if "source" in cell:
            cell_content = cell["source"]
            full_content = "".join(cell_content)

            if search_text in full_content:
                match_found = True
                if list_only:
                    # If list-only, we can stop searching once we find a match
                    return True

                # Print the match with context
                for j, line in enumerate(cell_content):
                    if search_text in line:
                        start = max(0, j - context_lines)
                        end = min(len(cell_content), j + context_lines + 1)
                        context = "".join(cell_content[start:end])

                        print(f"Match found in {notebook_path.name}, cell {i}, line {j}:\n")
                        print(context)
                        print("-" * 80)

    return match_found


def main():
    parser = argparse.ArgumentParser(
        description="Search for text in Jupyter Notebook cell contents with context."
    )
    parser.add_argument("notebooks", type=Path, nargs="+", help="Paths to the .ipynb files")
    parser.add_argument("search_text", type=str, help="Text to search for in the notebooks")
    parser.add_argument(
        "--context", type=int, default=2, help="Number of context lines to show (default: 2)"
    )
    parser.add_argument(
        "-l",
        "--list",
        action="store_true",
        help="List only filenames of notebooks containing a match",
    )

    args = parser.parse_args()

    for notebook in args.notebooks:
        if not notebook.is_file():
            print(f"The file {notebook} does not exist.")
        else:
            # Perform the search and return matches accordingly
            match_found = search_notebook(notebook, args.search_text, args.context, args.list)

            if match_found and args.list:
                print(notebook.name)


if __name__ == "__main__":
    main()
