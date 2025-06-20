# GTD System Verification Guide

## Quick Test in Spacemacs

After restarting Spacemacs (or running `SPC f e R` to reload config), test these commands:

### 1. Basic Navigation (SPC o o n)
- `SPC o o n i` - Should open inbox.org
- `SPC o o n p` - Should open projects.org
- `SPC o o n n` - Should open next-actions.org

### 2. Development Tools (SPC o o d)
- `SPC o o d i` - Initialize GTD system (creates directories/files)
- `SPC o o d r` - Reload GTD configuration
- `SPC o o d b` - Benchmark capture performance

### 3. Save Command
- `SPC o o s` - Save all org buffers

### 4. Verify Keybindings
- `SPC o o` - Should show GTD/Zettelkasten menu in which-key

### 5. Check Org-Roam (if installed)
- `SPC o o z n` - Find/create knowledge note
- `SPC o o z c` - Capture new knowledge

## Expected Results

✅ All GTD files should open without errors
✅ Keybindings should appear in which-key
✅ Initialize command should report "GTD system initialized successfully"
✅ Benchmark should complete in under 1 second

## Troubleshooting

If keybindings don't work:
1. Run `SPC o o d r` to reload GTD system
2. Check *Messages* buffer for errors
3. Ensure org-mode is loaded: `M-x org-version`

If files are missing:
1. Run `SPC o o d i` to initialize system
2. Check directory permissions on ~/personal/org-files/