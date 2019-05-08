#!/usr/bin/env python

import sh
import os
from os import listdir
from os.path import isfile, join

path_to_wsdls = "."
wsdl_files = [f for f in listdir(path_to_wsdls) if isfile(join(path_to_wsdls, f))]
output_dir = "docs"

if not os.path.exists(output_dir):
    os.mkdir(output_dir)

master_html = """
<html>
<body>
<ul>
"""
for file in wsdl_files:
    file_parts = file.split(".")
    if len(file_parts) == 2:
        if file_parts[1] == 'wsdl':
            # Found WSDL file, convert it now
            html_file_name = "{}.html".format(file_parts[0])
            with open("{}/{}".format(output_dir, html_file_name), "w") as html_file:
                sh.xsltproc('../wsdl-viewer.xsl', file, _out=html_file)
            master_html += '<li><a href="{0}">{0}</a></li>\n'.format(html_file_name)

master_html += """
</ul>
</body>
</html>
"""

with open("{}/index.html".format(output_dir), "w") as index_file:
    index_file.write(master_html)
