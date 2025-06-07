---
title: Single-File HTML App Prompts Library
type: note
permalink: prompts/single-file-html-app-prompts-library
---

# Single-File HTML App Prompts Library

## Overview
Created a comprehensive library of 40 prompts for generating self-contained HTML applications. Each prompt is designed to create a complete, functional application in a single HTML file with inline CSS and JavaScript, requiring no external dependencies.

## Prompt Categories

### 1. Productivity Applications (`single_file_html_apps.prompt`)
- **Task Manager**: Drag & drop, categories, local storage persistence
- **Markdown Editor**: Split-pane with live preview, syntax highlighting
- **Personal Finance Tracker**: Charts, budgets, multi-account support
- **Pomodoro Timer**: Analytics, Web Audio notifications, task tracking
- **Code Snippet Manager**: Multi-language syntax highlighting, templates
- **Habit Tracker**: Streak visualization, calendar heatmap
- **Mind Map Creator**: Auto-layout, SVG export, presentation mode
- **Password Vault**: Web Crypto API encryption, TOTP support
- **Resume Builder**: Multiple templates, ATS-friendly, PDF export
- **Beat Maker**: 16-step sequencer, Web Audio synthesis

### 2. Games (`single_file_html_games.prompt`)
- **Space Shooter**: Procedural graphics, boss battles, power-ups
- **Puzzle Platformer**: Physics puzzles, level editor, speedrun timer
- **Tower Defense**: Multiple tower types, pathfinding, upgrade trees
- **Match-3 RPG**: Combat system, equipment, skill trees
- **Endless Runner**: Procedural generation, missions, vehicle sections
- **RTS Game**: Resource management, tech tree, AI opponents
- **Rhythm Game**: Web Audio music generation, note charts
- **Roguelike**: Procedural dungeons, permadeath, meta progression
- **Physics Sandbox**: Custom physics engine, level sharing
- **Card Battler**: Deck building, AI opponents, tournaments

### 3. Creative Tools (`single_file_html_creative_tools.prompt`)
- **Vector Editor**: Bezier curves, boolean operations, SVG export
- **Digital Audio Workstation**: Multi-track, effects, MIDI editor
- **3D Modeler**: Mesh editing, sculpting, WebGL rendering
- **Photo Editor**: Layers, filters, non-destructive editing
- **Video Editor**: Timeline, transitions, chroma key
- **Animation Studio**: Onion skinning, rigging, tweening
- **Diagram Designer**: Smart connectors, auto-layout, templates
- **Font Designer**: Glyph editor, kerning, OpenType features
- **Circuit Designer**: Schematic capture, simulation, PCB layout
- **Architecture Tool**: 2D/3D views, material library, walkthrough

### 4. Data Tools (`single_file_html_data_tools.prompt`)
- **Spreadsheet**: 100+ functions, pivot tables, virtual scrolling
- **SQL Database**: Query editor, visual builder, schema management
- **Data Visualization**: 20+ chart types, dashboards, animations
- **Log Analyzer**: Pattern detection, timeline viz, anomaly detection
- **JSON/XML/YAML Editor**: Validation, conversion, diff tools
- **Network Analyzer**: HAR analysis, performance metrics
- **Scientific Calculator**: 3D plotting, symbolic math, LaTeX
- **Project Management**: Gantt charts, Kanban, resource planning
- **Form Builder**: Conditional logic, analytics, accessibility
- **BI Dashboard**: KPIs, drill-down, predictive analytics

## Technical Implementation Highlights

### Core Technologies Used
- **Canvas API**: For games, graphics editors, visualizations
- **WebGL**: For 3D modeling, advanced graphics
- **Web Audio API**: For music creation, sound synthesis
- **Web Crypto API**: For encryption, security features
- **Web Workers**: For background processing, performance
- **IndexedDB**: For large dataset handling
- **Service Workers**: For offline functionality

### Design Principles
1. **Zero Dependencies**: Everything inline, no CDNs or external files
2. **Performance First**: Virtual rendering, lazy loading, efficient algorithms
3. **Professional Quality**: Features comparable to commercial software
4. **Responsive Design**: Works on desktop, tablet, and mobile
5. **Privacy Focused**: All processing happens locally in browser
6. **Accessibility**: Keyboard navigation, ARIA labels, screen reader support

### Use Cases
- **Rapid Prototyping**: Quick proof of concepts without setup
- **Offline Tools**: Fully functional without internet
- **Education**: Teaching web technologies capabilities
- **Portable Apps**: Email or USB stick distribution
- **Privacy Tools**: No server-side data processing
- **Emergency Tools**: When online services are unavailable

## Usage Instructions
Each prompt can be used with Claude or other LLMs to generate complete applications. Simply copy the prompt and ask for implementation. The resulting HTML file can be:
1. Saved locally and opened in any browser
2. Shared via email or messaging
3. Hosted on any static web server
4. Embedded in other applications

## Future Enhancements
- Additional app categories (education, communication, utilities)
- Framework for combining multiple apps
- Progressive enhancement patterns
- WebAssembly integration examples
- PWA conversion templates

These prompts demonstrate the incredible capabilities of modern browsers and serve as a resource for rapid application development without traditional toolchains.