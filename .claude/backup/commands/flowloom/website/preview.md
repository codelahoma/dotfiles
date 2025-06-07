#!/bin/bash
# Local Jekyll preview server

set -e

REPO_ROOT=$(git rev-parse --show-toplevel)
CURRENT_BRANCH=$(git branch --show-current)

echo "🌐 Starting FlowLoom website preview..."

# Check if we're on gh-pages or need to create preview
if [[ "$CURRENT_BRANCH" == "gh-pages" ]]; then
  echo "📖 Using existing gh-pages content..."
  SERVE_DIR="$REPO_ROOT"
  NEEDS_SYNC=false
else
  echo "📝 Creating preview from current documentation..."
  SERVE_DIR="$REPO_ROOT/_preview"
  NEEDS_SYNC=true
fi

# Create preview content if needed
if [[ "$NEEDS_SYNC" == "true" ]]; then
  echo "🔄 Syncing documentation for preview..."
  
  # Check dependencies
  if [[ ! -f "$REPO_ROOT/.github/scripts/sync-documentation.js" ]]; then
    echo "❌ Sync script not found. Cannot create preview."
    exit 1
  fi
  
  if ! command -v node &> /dev/null; then
    echo "❌ Node.js not found. Please install Node.js to create preview."
    exit 1
  fi
  
  # Install sync dependencies
  cd "$REPO_ROOT/.github/scripts"
  if [[ ! -d "node_modules" ]]; then
    echo "📦 Installing sync dependencies..."
    npm install --silent
  fi
  
  # Run sync to create preview
  echo "🏗️  Building preview content..."
  rm -rf "$SERVE_DIR"
  mkdir -p "$SERVE_DIR"
  
  # Create basic Jekyll structure for preview
  cat > "$SERVE_DIR/_config.yml" << EOF
title: FlowLoom (Preview)
description: Open-source AI workflow automation for Claude Code
url: http://localhost:4000
baseurl: ""

markdown: kramdown
highlighter: rouge

collections:
  docs:
    output: true
    permalink: /docs/:path/
    layout: documentation

defaults:
  - scope:
      path: "_docs"
      type: "docs"
    values:
      layout: "documentation"
      toc: true

plugins:
  - jekyll-feed
  - jekyll-sitemap

exclude:
  - .github/
  - node_modules/
  - package*.json
  - "*.config.js"
  - .env*
  - sync-report.json
EOF

  # Create basic layout
  mkdir -p "$SERVE_DIR/_layouts"
  cat > "$SERVE_DIR/_layouts/default.html" << 'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{{ page.title | default: site.title }}</title>
  <style>
    body { font-family: -apple-system, sans-serif; margin: 0; padding: 20px; max-width: 1200px; margin: 0 auto; }
    .header { border-bottom: 1px solid #eee; margin-bottom: 20px; padding-bottom: 20px; }
    .content { display: flex; gap: 20px; }
    .sidebar { width: 250px; flex-shrink: 0; }
    .main { flex: 1; }
    .nav-section h4 { margin: 20px 0 10px 0; color: #666; }
    .nav-section ul { list-style: none; padding: 0; margin: 0; }
    .nav-section li { margin: 5px 0; }
    .nav-section a { text-decoration: none; color: #0066cc; }
    .nav-section a:hover { text-decoration: underline; }
    .nav-section a.current { font-weight: bold; color: #333; }
    pre { background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto; }
    code { background: #f0f0f0; padding: 2px 4px; border-radius: 3px; }
    pre code { background: none; padding: 0; }
    .doc-meta { color: #666; font-size: 0.9em; margin-bottom: 20px; }
    .preview-banner { background: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; border-radius: 5px; margin-bottom: 20px; }
  </style>
</head>
<body>
  <div class="header">
    <h1><a href="/" style="text-decoration: none; color: #333;">{{ site.title }}</a></h1>
    <div class="preview-banner">
      🔍 <strong>Preview Mode</strong> - This is a local preview of the FlowLoom documentation
    </div>
  </div>
  <div class="content">
    <div class="sidebar">
      <nav>
        {% if site.data.navigation.docs %}
          {% for section in site.data.navigation.docs %}
            <div class="nav-section">
              <h4>{{ section.title }}</h4>
              <ul>
                {% for item in section.children %}
                  <li>
                    <a href="{{ item.url | relative_url }}"
                       {% if page.url == item.url %}class="current"{% endif %}>
                      {{ item.title }}
                    </a>
                  </li>
                {% endfor %}
              </ul>
            </div>
          {% endfor %}
        {% endif %}
      </nav>
    </div>
    <div class="main">
      {{ content }}
    </div>
  </div>
</body>
</html>
EOF

  cat > "$SERVE_DIR/_layouts/documentation.html" << 'EOF'
---
layout: default
---

<article>
  <header>
    <h1>{{ page.title }}</h1>
    {% if page.description %}
      <p><em>{{ page.description }}</em></p>
    {% endif %}
    {% if page.source_file %}
      <div class="doc-meta">
        Source: <code>{{ page.source_file }}</code>
        {% if page.last_modified %}
          | Updated: {{ page.last_modified | date: "%B %d, %Y" }}
        {% endif %}
      </div>
    {% endif %}
  </header>
  
  {{ content }}
</article>
EOF

  # Create homepage
  cat > "$SERVE_DIR/index.md" << 'EOF'
---
layout: default
title: FlowLoom Documentation Preview
---

# FlowLoom Documentation Preview

Welcome to the local preview of FlowLoom documentation!

## Navigation

Use the sidebar to browse through the documentation sections:

{% if site.data.navigation.docs %}
{% for section in site.data.navigation.docs %}
- **{{ section.title }}**
  {% for item in section.children -%}
  - [{{ item.title }}]({{ item.url }})
  {% endfor %}
{% endfor %}
{% endif %}

## About This Preview

This preview is generated from your local documentation files and shows how they will appear on the website after synchronization.

**Note:** This preview may not include all styling and features of the live website.
EOF

  # Sync documentation
  node sync-documentation.js "$REPO_ROOT" "$SERVE_DIR" --verbose
  
  cd "$REPO_ROOT"
fi

# Check Jekyll dependencies
cd "$SERVE_DIR"

if [[ ! -f "Gemfile" ]]; then
  echo "📦 Creating Gemfile for Jekyll..."
  cat > Gemfile << 'EOF'
source "https://rubygems.org"

gem "jekyll", "~> 4.3"
gem "jekyll-feed"
gem "jekyll-sitemap"

group :jekyll_plugins do
  gem "jekyll-feed"
  gem "jekyll-sitemap"
end
EOF
fi

# Check if bundler is available
if ! command -v bundle &> /dev/null; then
  echo "❌ Bundler not found. Please install Ruby and Bundler:"
  echo "   For macOS: brew install ruby && gem install bundler"
  echo "   For Ubuntu: sudo apt install ruby-dev && gem install bundler"
  exit 1
fi

# Install Jekyll dependencies
echo "📦 Installing Jekyll dependencies..."
bundle config set --local path 'vendor/bundle'
bundle install --quiet

# Start Jekyll server
echo ""
echo "🚀 Starting Jekyll preview server..."
echo "📱 Preview will be available at: http://localhost:4000"
echo "🔄 Jekyll will automatically reload when files change"
echo "⌨️  Press Ctrl+C to stop the server"
echo ""

# Check if port 4000 is available
if command -v lsof &> /dev/null && lsof -Pi :4000 -sTCP:LISTEN -t >/dev/null; then
  echo "⚠️  Port 4000 is already in use. Trying port 4001..."
  PORT=4001
else
  PORT=4000
fi

# Start server with appropriate options
if [[ "$CURRENT_BRANCH" == "gh-pages" ]]; then
  bundle exec jekyll serve --host 0.0.0.0 --port $PORT --livereload
else
  echo "💡 Tip: Save this preview with 'cp -r $SERVE_DIR /tmp/flowloom-preview'"
  bundle exec jekyll serve --host 0.0.0.0 --port $PORT --livereload --source "$SERVE_DIR"
fi