---
title: PPA Security Transformation with find -exec and sed
type: note
permalink: technical-insights/ppa-security-transformation-with-find-exec-and-sed
tags:
- '#ppa'
- '#security'
- '#sed'
- '#find'
- '#command-patterns'
---

# PPA Security Transformation with find -exec and sed

## Issue Discovered
When using `find -exec` with sed commands containing pipe delimiters (`|`), PPA security system transforms the command in a way that breaks sed syntax.

## Failed Command Pattern
```bash
find _docs -name "*.md" -exec sed -i '' 's|\.\./getting-started/||g; s|\.\./user-guide/||g; s|\.\./developer-guide/||g; s|\.\./reference/||g' {} \;
```

## Error Manifestation
```
sed: 1: "s < /dev/null | \.\./ge ...": bad flag in substitute command: '|'
```

## What Happened
1. PPA detected the combination of:
   - `find -exec` (can execute commands on multiple files)
   - Pipe characters `|` within the sed expression

2. PPA attempted to inject `< /dev/null` into the command for security
3. This injection occurred *within* the sed pattern, breaking the syntax
4. The sed command became malformed: `s < /dev/null | \.\./ge ...`

## Working Alternative
```bash
# Use a for loop instead
for file in _docs/*.md; do 
    sed -i '' -e 's|\.\./getting-started/||g' -e 's|\.\./user-guide/||g' -e 's|\.\./developer-guide/||g' -e 's|\.\./reference/||g' "$file"
done
```

## Key Insights

1. **Risk Assessment**: PPA treats `find -exec` as higher risk than simple loops
2. **Context Matters**: Same sed pattern works in for loop but not in find -exec
3. **Security Transformation**: PPA modifies commands it deems risky, potentially breaking them
4. **Delimiter Choice**: While pipe `|` is valid sed delimiter, it triggers extra scrutiny

## Best Practices

1. **Avoid find -exec with complex sed**: Use for loops for better compatibility
2. **Alternative delimiters**: Consider using `#` or `/` instead of `|` in sed
3. **Break up complex commands**: Multiple simple commands over one complex command
4. **Python alternative**: For complex transformations, Python scripts avoid these issues

## Example Python Alternative
```python
import os
import re

for filename in os.listdir('_docs'):
    if filename.endswith('.md'):
        filepath = os.path.join('_docs', filename)
        with open(filepath, 'r') as f:
            content = f.read()
        
        # Replace relative paths
        content = re.sub(r'\.\./getting-started/', '', content)
        content = re.sub(r'\.\./user-guide/', '', content)
        content = re.sub(r'\.\./developer-guide/', '', content)
        content = re.sub(r'\.\./reference/', '', content)
        
        with open(filepath, 'w') as f:
            f.write(content)
```

## Related Patterns
- Similar issues may occur with other commands that combine find -exec with special characters
- PPA's security transformations can break valid syntax when injecting safety measures
- Understanding these patterns helps choose appropriate command structures