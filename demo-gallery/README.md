# FlowLoom HTML App Gallery

This gallery showcases self-contained HTML applications created in a single turn using AI prompts. Each app is a complete, functional application in a single HTML file with no external dependencies.

## 📊 Current Status

- **Total App Prompts**: 40
- **Apps Implemented**: 0
- **Categories**: 4 (Productivity, Games, Creative Tools, Data Tools)

## 🎯 Purpose

The demo gallery serves as:
1. **Showcase**: Demonstrating the power of single-file HTML applications
2. **Resource**: Ready-to-use applications for various purposes
3. **Learning**: Examples of advanced web technologies in action
4. **Testing**: Validating the quality of AI-generated code

## 📁 Directory Structure

```
demo-gallery/
├── index.html           # Gallery homepage
├── README.md           # This file
├── productivity/       # Productivity applications
├── games/             # HTML5 games
├── creative-tools/    # Creative and design tools
└── data-tools/        # Data analysis and visualization tools
```

## 🚀 How to Add a New App

### 1. Generate the App

Use one of the prompts from the `/prompts/` directory:

```bash
# Example: Generate a task manager app
# Copy the prompt from prompts/single_file_html_apps.prompt
# Use it with Claude or another AI to generate the app
```

### 2. Save the Generated App

Place the generated HTML file in the appropriate category folder:

```bash
# Example: Save task manager
demo-gallery/productivity/task-manager.html
```

### 3. Update the Gallery Index

The index.html will need to be updated to replace the placeholder with the actual app card. Here's the structure:

```html
<!-- Replace the placeholder -->
<div class="app-card placeholder">
    <div class="icon">✅</div>
    <h3>Task Manager</h3>
    <p>Coming Soon</p>
</div>

<!-- With an actual app card -->
<div class="app-card">
    <span class="badge new">NEW</span>
    <div class="app-header">
        <h3>Task Manager Pro</h3>
        <p class="app-description">
            Full-featured task management with categories, 
            due dates, and local storage persistence.
        </p>
    </div>
    <div class="app-footer">
        <a href="productivity/task-manager.html" class="app-link">
            ▶️ Launch App
        </a>
        <a href="../prompts/single_file_html_apps.prompt#task-manager" class="app-link secondary">
            📋 View Prompt
        </a>
    </div>
</div>
```

## 🎨 App Requirements

Each app should meet these criteria:

1. **Self-Contained**: Everything in a single HTML file
2. **No Dependencies**: No external libraries, CDNs, or assets
3. **Responsive**: Works on desktop, tablet, and mobile
4. **Functional**: Fully working, not just a demo
5. **Persistent**: Uses localStorage where appropriate
6. **Polished**: Professional UI/UX

## 📝 Naming Convention

Use descriptive, kebab-case filenames:
- `task-manager.html`
- `markdown-editor.html`
- `space-shooter.html`
- `vector-graphics-editor.html`

## 🏷️ Badges

Apps can have badges to indicate status:
- `NEW` - Recently added (red)
- `POPULAR` - Frequently used (yellow)
- `UPDATED` - Recently improved (blue)

## 🧪 Testing Checklist

Before adding an app, verify:
- [ ] Opens directly in browser without errors
- [ ] All features work as described
- [ ] Mobile responsive
- [ ] No external requests (check Network tab)
- [ ] Data persists after refresh (if applicable)
- [ ] Keyboard shortcuts work
- [ ] Accessibility basics (can navigate with keyboard)

## 🛠️ Development Tips

1. **Use the Prompts**: Start with the detailed prompts in `/prompts/`
2. **Iterate**: If the first generation isn't perfect, refine the prompt
3. **Test Thoroughly**: Each app should be production-ready
4. **Document Features**: Update the description to highlight key features
5. **Share Feedback**: Note what worked well or needed adjustment

## 🤝 Contributing

When contributing apps:
1. Follow the single-file constraint strictly
2. Ensure high quality and full functionality
3. Test on multiple devices/browsers
4. Update both the file and index.html
5. Add a note about which AI model/prompt was used

## 📊 Tracking Progress

The gallery automatically counts implemented apps. As we add apps, we'll track:
- Which prompts produce the best results
- Which apps are most popular/useful
- Areas for prompt improvement
- Common implementation patterns

## 🎯 Goals

- **Short Term**: Implement 5 apps from each category
- **Medium Term**: Have all 40 apps implemented
- **Long Term**: Expand with community contributions

## 🔗 Resources

- [FlowLoom Project](https://github.com/codelahoma/flowloom)
- [Prompts Directory](/prompts/)
- [HTML App Best Practices](/docs/single-file-html-best-practices.md)

---

*This gallery is part of FlowLoom's mission to demonstrate the power of AI-assisted development and the capabilities of modern web technologies.*