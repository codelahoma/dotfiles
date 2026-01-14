# Streamlined Spacemacs Configuration Plan

## Executive Summary

This plan will reduce the Spacemacs configuration from 1,182 lines to approximately 600-800 lines by:
- Removing 9,566 lines of CodeLahoma GTD code (25 files)
- Removing 8 unused languages/tools layers
- Removing 10+ niche packages
- Integrating org-gtd-zettelkasten from ~/github
- Adding mu4e email layer
- Streamlining themes from 10 to 5
- Reducing font fallbacks from 11 to 4
- Fixing org-roam autosync hang

---

## Part 1: Files to DELETE

### CodeLahoma GTD System (9,566 lines total)

**Main Directory:** `/Users/rodk/.homesick/repos/dotfiles/home/.spacemacs.d/codelahoma-gtd/`

Delete entire directory containing:
- codelahoma-bridge-metrics.el (615 lines)
- codelahoma-bridge-projects.el (531 lines)
- codelahoma-bridge-suggestions.el (446 lines)
- codelahoma-bridge-workflows.el (472 lines)
- codelahoma-bridge.el (305 lines)
- codelahoma-command-palette.el (511 lines)
- codelahoma-dashboard.el (512 lines)
- codelahoma-gtd-agenda.el (235 lines)
- codelahoma-gtd-analytics.el (455 lines)
- codelahoma-gtd-autosave.el (338 lines)
- codelahoma-gtd-capture.el (152 lines)
- codelahoma-gtd-config.el (123 lines)
- codelahoma-gtd-contexts.el (217 lines)
- codelahoma-gtd-core.el (768 lines)
- codelahoma-gtd-daily-review.el (339 lines)
- codelahoma-gtd-monthly-review.el (493 lines)
- codelahoma-gtd-process.el (247 lines)
- codelahoma-gtd-quick.el (315 lines)
- codelahoma-gtd-reminders.el (348 lines)
- codelahoma-gtd-review.el (66 lines)
- codelahoma-gtd-roam.el (120 lines)
- codelahoma-gtd-weekly-review.el (506 lines)
- codelahoma-status-bar.el (363 lines)
- codelahoma-unified-search.el (535 lines)
- codelahoma-ux-polish.el (554 lines)
- tests/ directory with 49 test files

**Loader Files (in .spacemacs.d root):**
- `/Users/rodk/.homesick/repos/dotfiles/home/.spacemacs.d/codelahoma-gtd-loader.el` (164 lines)
- `/Users/rodk/.homesick/repos/dotfiles/home/.spacemacs.d/codelahoma-gtd-minimal.el` (59 lines)
- `/Users/rodk/.homesick/repos/dotfiles/home/.spacemacs.d/codelahoma-gtd-reload.el` (~35 lines)
- `/Users/rodk/.homesick/repos/dotfiles/home/.spacemacs.d/codelahoma-ui.el` (466 lines)

**Total to delete:** Entire `codelahoma-gtd/` directory + 4 root files = ~10,255 lines

---

## Part 2: Layers Configuration

### REMOVE These Layers (11 total)

**Languages:**
- `rust` - Not used by user
- `lua` - Hammerspoon config edited via org-mode tangling, not in Emacs
- `ansible` - Niche, not mentioned as priority
- `cmake` - Niche, not mentioned as priority

**Tools:**
- `wakatime` - Includes hardcoded API key, user requested removal
- `pass` - User requested removal
- `chrome` (atomic-chrome) - User requested removal
- `themes-megapack` - Replace with curated themes
- `bm` (visual bookmarks) - Niche utility
- `command-log` - Debug tool, not daily use
- `copy-as-format` - Niche utility

### KEEP These Layers (28 total)

**Core Spacemacs:**
- `auto-completion` (with Company, posframe, company-box)
- `colors`
- `emoji`
- `evil-better-jumper`
- `helm`
- `spacemacs-modeline`
- `spacemacs-org`
- `spacemacs-navigation`
- `syntax-checking`
- `theming`
- `treemacs` (with git mode, file watching, perspectives)
- `osx`

**Programming:**
- `javascript` (with Node REPL, Prettier)
- `typescript` (with TIDE backend, ESLint)
- `python` (with Pyright LSP, Black formatter, Pytest)
- `emacs-lisp`
- `react`
- `lsp` (with high file watch threshold, peek navigation, breadcrumbs)
- `tree-sitter`

**Writing & Knowledge:**
- `org` (with roam, roam-ui, sticky-header, transclusion)
- `markdown`
- `html`
- `mermaid`

**Data & Formats:**
- `yaml` (with LSP)
- `csv`
- `pdf`

**DevOps & Tools:**
- `restclient`
- `docker`
- `git`
- `shell` (vterm with zsh, 50px height, right position)

**Utilities:**
- `elfeed` (RSS reader)
- `eww` (Emacs web browser)
- `helpful` (better help)
- `ibuffer` (buffer management)
- `search-engine`
- `spell-checking` (disabled by default)
- `version-control`

**Private Layers:**
- `rk-layout` (workspace management)
- `gtd-zettelkasten` (NEW - from ~/github/org-gtd-zettelkasten)

### ADD This Layer

**Email:**
```elisp
(mu4e :variables
      mu4e-installation-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
      mu4e-enable-notifications t
      mu4e-enable-mode-line t
      mu4e-org-compose-support t)
```

---

## Part 3: Additional Packages Configuration

### REMOVE These Packages (7 total)

- `hyperbole` - Hypertext system (niche, rarely used)
- `pinboard` - Pinboard.in client (user requested removal)
- `atomic-chrome` - Edit Chrome text areas (user requested removal)
- `jira-markup-mode` - JIRA markup (niche)
- `sicp` - SICP textbook support (niche)
- `wsd-mode` - WebSequenceDiagrams (niche)
- Duplicate `fold-this` entry

### KEEP These Packages (18 total)

**Essential Tools:**
- `gptel` - AI integration (user priority)
- `gptel-extensions` (from ~/.emacs.d/private/)
- `evil-easymotion` - Vim-style motion
- `direnv` - Environment management
- `editorconfig` - EditorConfig support
- `keychain-environment` - SSH keychain
- `yasnippet-snippets` - Snippet collection

**Org/Writing:**
- `org-roam-bibtex` - Bibliography
- `org-noter` - Note-taking on PDFs
- `org-noter-pdftools` - PDF tools integration
- `fold-this` - Code folding

**UI/Theme:**
- `fira-code-mode` - Font ligatures
- `highlight-indent-guides` - Indentation guides
- `ef-themes` - Elegant themes (Protesilaos Stavrou)
- `all-the-icons` - Icon support

**Data:**
- `sqlite3` - Database (for org-roam)
- `mermaid-mode` - Diagram support

---

## Part 4: Theme Configuration

### Current Themes (10):
```elisp
ef-autumn, ef-winter, farmhouse-light, farmhouse-dark,
dakrone, hc-zenburn, leuven, cyberpunk,
gruvbox-light-hard, gruvbox-dark-hard
```

### Recommended Streamlined Themes (5):

**Light themes (2):**
- `ef-winter` - Clean, elegant light (Protesilaos Stavrou)
- `leuven` - Classic org-mode light theme

**Dark themes (3):**
- `ef-autumn` - Warm, muted dark
- `hc-zenburn` - Low-contrast dark (easy on eyes)
- `gruvbox-dark-hard` - Popular retro dark

**Rationale:**
- Keep ef-themes (modern, maintained, excellent for long reading/writing)
- Keep leuven (org-mode standard)
- Keep one gruvbox variant
- Keep hc-zenburn (low-contrast for late-night coding)
- Remove: farmhouse (redundant), dakrone (niche), cyberpunk (harsh), gruvbox-light-hard (redundant with leuven)

---

## Part 5: Font Configuration

### Current Fonts (11 fallbacks - excessive!)

FiraCode, MesloLGS, Inconsolata, Cascadia, JetBrainsMono, Monoid, VictorMono, Iosevka, Cousine, IMWritingMonoS, Hack, SauceCodePro Nerd Fonts

### Recommended Streamlined Fonts (4):

```elisp
dotspacemacs-default-font '(
  ("FiraCode Nerd Font"
   :size 18.0
   :weight normal
   :width normal)
  ("JetBrainsMono Nerd Font"
   :size 18.0
   :weight normal
   :width normal)
  ("Cascadia Mono NF"
   :size 18.0
   :weight normal
   :width normal)
  ("Hack Nerd Font"
   :size 18.0
   :weight normal
   :width normal)
)
```

**Rationale:**
- FiraCode: Excellent ligatures, current primary
- JetBrains Mono: Modern, professional fallback
- Cascadia: Microsoft's modern font
- Hack: Reliable fallback

---

## Part 6: mu4e Email Configuration

### Prerequisites

**System packages to install:**
```bash
brew install mu
brew install isync  # for mbsync (IMAP sync)
# or
brew install offlineimap  # alternative IMAP sync
```

### Configuration Structure

**In dotspacemacs/layers:**
```elisp
(mu4e :variables
      mu4e-installation-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
      mu4e-enable-notifications t
      mu4e-enable-mode-line t
      mu4e-org-compose-support t)  ; Enable org-mode in emails
```

**In dotspacemacs/user-config:**
```elisp
(with-eval-after-load 'mu4e
  ;; Basic paths
  (setq mu4e-maildir "~/Mail"
        mu4e-attachment-dir "~/Downloads"
        mu4e-get-mail-command "mbsync -a"  ; or "offlineimap"
        mu4e-update-interval 300)  ; Update every 5 minutes

  ;; Folders (will vary by email provider)
  (setq mu4e-sent-folder   "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder  "/Trash"
        mu4e-refile-folder "/Archive")

  ;; SMTP (example - user needs to customize)
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.example.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls)

  ;; View settings
  (setq mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-html2text-command "w3m -T text/html"
        mu4e-compose-format-flowed t)

  ;; Keybindings
  (spacemacs/set-leader-keys
    "am" 'mu4e))
```

**Setup steps for user:**
1. Create `~/.mbsyncrc` (or `~/.offlineimaprc`) with email account details
2. Run initial sync: `mbsync -a` (or `offlineimap`)
3. Index emails: `mu index --maildir=~/Mail`
4. Test in Emacs: `SPC a m`

---

## Part 7: org-gtd-zettelkasten Integration

### Current State
- Located at: `/Users/rodk/github/org-gtd-zettelkasten`
- Already symlinked: `~/.emacs.d/private/gtd-zettelkasten -> ~/github/org-gtd-zettelkasten/layers/gtd-zettelkasten`
- Not currently loaded in init.el

### Configuration to Add

**In dotspacemacs/layers:**
```elisp
;; Add after rk-layout
gtd-zettelkasten
```

**In dotspacemacs/user-config (REPLACE old GTD section):**

```elisp
;; org-gtd-zettelkasten Configuration
(with-eval-after-load 'org
  ;; Set base directory for GTD and Zettelkasten
  (setq gtd-zettel-org-directory "~/personal/org-files/")

  ;; The layer will automatically set up:
  ;; - gtd-zettel-gtd-directory      → ~/personal/org-files/gtd/
  ;; - gtd-zettel-notes-directory    → ~/personal/org-files/notes/
  ;; - gtd-zettel-inbox-file         → ~/personal/org-files/gtd/inbox.org
  ;; - gtd-zettel-projects-file      → ~/personal/org-files/gtd/projects.org

  ;; Org-roam integration (for Zettelkasten)
  (setq org-roam-directory (expand-file-name "knowledge/" gtd-zettel-org-directory)
        org-roam-db-location (expand-file-name ".org-roam.db" gtd-zettel-org-directory))

  ;; IMPORTANT: Manual sync to avoid hang
  ;; Run M-x org-roam-db-sync after adding/modifying notes
  ;; Do NOT enable org-roam-db-autosync-mode (causes 100% CPU hang)

  (message "org-gtd-zettelkasten system loaded"))
```

**Keybindings (layer provides):**
- `SPC o g i` - Capture to inbox
- `SPC o g p` - Process inbox
- `SPC o g a` - Agenda view
- `SPC o g n` - New Zettel note
- `SPC o g f` - Find note

---

## Part 8: org-roam Autosync Fix

### The Problem
- User reports: "org-roam DB autosync disabled (causes hangs)"
- Symptom: Emacs process at 100% CPU, minibuffer shows "Processing modified files..."

### Root Causes
1. **URL link expansion bug** - Files with http:// links slow processing
2. **Large note collection** - DB sync on every file change is expensive
3. **Multiple Emacs instances** - Concurrent DB access causes corruption/hangs
4. **Outdated org-roam version** - Many bugs fixed in recent versions

### Solution Strategy

**Recommended approach (current config is CORRECT):**

```elisp
;; In org-roam config:
(use-package org-roam
  :custom
  (org-roam-directory "~/personal/org-files/knowledge/")
  (org-roam-db-location "~/personal/org-files/.org-roam.db")
  :config
  ;; DO NOT enable autosync - causes hangs
  ;; (org-roam-db-autosync-mode)  ;; ← Keep this commented!

  ;; Instead: Manual sync workflow
  ;; Run M-x org-roam-db-sync periodically or after batch note changes
)
```

**User workflow:**
1. Take notes normally
2. When ready (end of session, after imports): `M-x org-roam-db-sync`
3. For automation, add to Emacs exit hook:

```elisp
;; Optional: Sync on Emacs exit
(add-hook 'kill-emacs-hook 'org-roam-db-sync)
```

---

## Part 9: Custom Functions to Keep

### Essential Functions (KEEP)

```elisp
codelahoma/set-font-size-based-on-screen  ; Dynamic font sizing
my-info-mode-hook                         ; Info mode navigation
renumber-region                           ; Utility for numbering
insert-current-date-time                  ; Timestamp insertion
rk/insert-spacemacs-config-block          ; Literate config helper
rk/get-spacemacs-config-targets           ; Config targets helper
codelahoma/insert-random-uid              ; UUID generation
elfeed-save-to-org-roam-dailies          ; RSS → org integration
rk/clip-for-claude                        ; Copy file reference with line numbers
rk/reset-hammerspoon                      ; Reload Hammerspoon
```

### Remove Functions
- All `codelahoma-gtd-*` functions (replaced by gtd-zettelkasten layer)
- All `codelahoma-status-bar-*` functions
- All `codelahoma-bridge-*` functions

### Keybinding Organization

```elisp
;; User-defined prefix
(spacemacs/declare-prefix "o" "user-defined")

;; Subgroups
(spacemacs/declare-prefix "ob" "buffer")
(spacemacs/set-leader-keys "obn" 'spacemacs/new-empty-buffer)

(spacemacs/declare-prefix "oc" "copy")
(spacemacs/set-leader-keys
  "ocl" 'avy-copy-line
  "ocp" 'forge-copy-url-at-point-as-kill)

(spacemacs/declare-prefix "ox" "text")
(spacemacs/set-leader-keys "oxw" 'white-space-cleanup)

(spacemacs/declare-prefix "oh" "Hammerspoon")
(spacemacs/set-leader-keys "ohr" 'rk/reset-hammerspoon)

(spacemacs/declare-prefix "oC" "Claude")
(spacemacs/set-leader-keys "oCC" 'rk/clip-for-claude)

(spacemacs/declare-prefix "of" "folding")
(spacemacs/set-leader-keys
  "off" 'fold-this
  "ofm" 'fold-this-all
  "ofr" 'fold-this-unfold-all)

(spacemacs/declare-prefix "og" "GTD-Zettelkasten")
;; gtd-zettelkasten layer will populate this

(spacemacs/set-leader-keys "otd" 'insert-current-date-time)

;; REMOVE this (pinboard):
;; (spacemacs/set-leader-keys "oap" 'pinboard)
```

---

## Part 10: Expected Results

### Quantified Improvements

| Metric | Before | After | Change |
|--------|--------|-------|--------|
| init.el lines | 1,182 | ~700 | -40% |
| Total .el lines | 11,437 | ~1,200 | -90% |
| Configuration layers | 39+ | ~28 | -28% |
| Additional packages | 30+ | 18 | -40% |
| Themes loaded | 10 | 5 | -50% |
| Font fallbacks | 11 | 4 | -64% |
| Files in .spacemacs.d | 32 | 8 | -75% |

### Functional Improvements

1. **Cleaner architecture**: Single GTD system (org-gtd-zettelkasten) vs. custom 9,500-line implementation
2. **Email capability**: mu4e layer for email management (user requirement)
3. **Faster startup**: Fewer layers and packages to load
4. **Maintainability**: Literate config in .org file with clear sections
5. **Security**: Removed hardcoded wakatime API key
6. **Stability**: org-roam autosync issue addressed with manual sync approach
7. **Focus**: Only tools user actually uses (Python, JS/TS, writing, GTD)

---

## Part 11: Implementation Approach

### Phase 1: Backup and Preparation

```bash
# 1. Backup current configuration
cd ~/.spacemacs.d
tar -czf ~/spacemacs-backup-$(date +%Y%m%d).tar.gz .

# 2. Backup current Emacs packages
cd ~/.emacs.d
tar -czf ~/emacs-packages-backup-$(date +%Y%m%d).tar.gz elpa/

# 3. Create git branch for changes
cd ~/.homesick/repos/dotfiles
git checkout -b streamline-spacemacs
```

### Phase 2: Delete CodeLahoma GTD Files

```bash
cd ~/.spacemacs.d

# Delete entire GTD directory
rm -rf codelahoma-gtd/

# Delete loader files
rm codelahoma-gtd-loader.el
rm codelahoma-gtd-minimal.el
rm codelahoma-gtd-reload.el
rm codelahoma-ui.el

# Verify deletion
ls -la | grep codelahoma
# Should show no results
```

### Phase 3: Work with dotspacemacs.org

**Current situation:** The init.el says it's auto-generated from dotspacemacs.org, but I found the actual source file at:
- `/Users/rodk/.homesick/repos/dotfiles/home/dotspacemacs.org`

This is a literate programming file that needs to be edited. After editing, tangle with `C-c C-v t` to regenerate init.el.

### Phase 4: Update Configuration

Edit `/Users/rodk/.homesick/repos/dotfiles/home/dotspacemacs.org`:

1. **Remove** layers: rust, lua, ansible, cmake, wakatime, pass, chrome, themes-megapack, bm, command-log, copy-as-format
2. **Remove** packages: hyperbole, pinboard, atomic-chrome, jira-markup-mode, sicp, wsd-mode
3. **Add** layer: mu4e (with configuration)
4. **Add** layer: gtd-zettelkasten
5. **Update** themes list to 5 themes
6. **Update** font list to 4 fonts
7. **Replace** old GTD configuration with org-gtd-zettelkasten config
8. **Keep** essential custom functions
9. **Update** keybindings (remove pinboard, keep others)

### Phase 5: Tangle and Test

```bash
# 1. Open dotspacemacs.org in Emacs
emacs ~/dotspacemacs.org

# 2. Tangle to generate init.el
# In Emacs: C-c C-v t

# 3. Review generated init.el
wc -l ~/.spacemacs.d/init.el
# Should be ~600-800 lines (down from 1,182)

# 4. Restart Emacs and watch for errors
emacs --debug-init

# 5. Check loaded layers
# In Emacs: SPC h d s
```

### Phase 6: Install mu4e

```bash
# Install prerequisites
brew install mu isync

# Create mbsync configuration
# Edit ~/.mbsyncrc with email settings

# Initial sync
mbsync -a

# Index emails
mu init --maildir=~/Mail
mu index

# Test in Emacs
# SPC a m
```

### Phase 7: Verify org-gtd-zettelkasten

```bash
# In Emacs:
# SPC o g i  # Capture to inbox
# SPC o g n  # New note
# SPC o g a  # Agenda

# Manual sync:
# M-x org-roam-db-sync
```

### Phase 8: Verification Checklist

- [ ] Configuration loads without errors
- [ ] Layer count reduced to ~28
- [ ] Package count reduced to ~18
- [ ] Only 5 themes cycle with `SPC T n`
- [ ] Only 4 fonts available with `SPC T F`
- [ ] GTD-Zettelkasten keybindings work (`SPC o g`)
- [ ] mu4e works (`SPC a m`)
- [ ] Python LSP and formatting work
- [ ] TypeScript/JavaScript with Tide and Prettier work
- [ ] Org-roam manual sync works without hanging
- [ ] Hammerspoon integration works (`SPC o h r`)
- [ ] gptel (AI) works

---

## Part 12: Rollback Plan

If migration fails:

```bash
# 1. Restore backup
cd ~/.spacemacs.d
rm -rf *
tar -xzf ~/spacemacs-backup-YYYYMMDD.tar.gz

# 2. Restore packages if needed
cd ~/.emacs.d
rm -rf elpa/
tar -xzf ~/emacs-packages-backup-YYYYMMDD.tar.gz

# 3. Restart Emacs
emacs

# 4. Revert git changes
cd ~/.homesick/repos/dotfiles
git checkout master
git branch -D streamline-spacemacs
```

---

## Summary

This plan streamlines your Spacemacs configuration by:

1. **Removing bloat**: 10,000+ lines of old GTD code, unused language layers, niche tools
2. **Focusing on your workflow**: GTD/Zettelkasten, Python/JS development, writing, email
3. **Improving maintainability**: Clear structure, fewer dependencies, faster startup
4. **Adding missing features**: mu4e for email management
5. **Fixing issues**: org-roam autosync (use manual sync), hardcoded API keys removed
6. **Preserving functionality**: All your actual use cases remain supported

**Next steps after review:**
1. Answer any clarifying questions you have
2. Create backups
3. Execute the migration phase by phase
4. Test thoroughly at each step
5. Commit changes to git branch
