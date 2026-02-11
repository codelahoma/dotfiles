# Recreating iTerm2's Autocomplete in tmux with fzf

> Script: `~/.local/bin/tmux-autocomplete` | Keybinding: `M-;` (Alt-semicolon)

One of iTerm2's best features is its autocomplete popup. You press `Cmd-;` and it scans your terminal scrollback for words matching whatever you've partially typed, presenting them in a filterable list. Pick one, and it fills in the rest. It's fast, contextual, and once you've used it, you miss it everywhere else.

The problem: if you live inside tmux, iTerm2's autocomplete can't see your scrollback. tmux manages its own buffer, and iTerm2 only sees the outer terminal. So that `Cmd-;` shortcut becomes useless the moment you attach a session.

The good news is that tmux gives us all the primitives we need to rebuild this from scratch. We can capture the scrollback, figure out what word the cursor is on, search for completions, and present them in a popup — all with a bash script and fzf.

## The approach

The script breaks down into three phases:

1. **Find the prefix** — figure out the partial word to the left of the cursor
2. **Search the scrollback** — capture the full tmux scrollback and find matching words
3. **Present and insert** — show matches in an fzf popup, then send the completion back to the pane

Each phase leans on a different piece of tmux's API.

## Phase 1: Finding the prefix

Before we can complete anything, we need to know what the user has partially typed. tmux can tell us exactly where the cursor is and what's on the current line.

```bash
PANE_ID="${1:-}"
if [[ -z "$PANE_ID" ]]; then
    PANE_ID="$(tmux display-message -p '#{pane_id}')"
fi

# Get cursor position (0-indexed)
CURSOR_X="$(tmux display-message -t "$PANE_ID" -p '#{cursor_x}')"
CURSOR_Y="$(tmux display-message -t "$PANE_ID" -p '#{cursor_y}')"

# Capture just the cursor's line
CURRENT_LINE="$(tmux capture-pane -t "$PANE_ID" -p -S "$CURSOR_Y" -E "$CURSOR_Y")"

# Extract text to the left of the cursor
LEFT_OF_CURSOR="${CURRENT_LINE:0:$CURSOR_X}"

# Pull out the partial word (non-whitespace chars at end of string)
if [[ "$LEFT_OF_CURSOR" =~ ([^[:space:]]+)$ ]]; then
    PREFIX="${BASH_REMATCH[1]}"
else
    exit 0  # nothing to complete
fi
```

A few things worth noting:

- `tmux display-message -p` with format strings like `#{cursor_x}` is how you query tmux's internal state. It's the equivalent of an API call — you're asking tmux "where is the cursor right now?"
- `tmux capture-pane -p` dumps pane content to stdout. The `-S` and `-E` flags set the start and end lines. By passing the cursor's Y position for both, we grab just the one line we care about.
- The bash regex match `([^[:space:]]+)$` grabs the last contiguous non-whitespace chunk. That's our partial word.

We also bail early if the prefix is shorter than 2 characters, since single-character matches would flood the list:

```bash
if [[ "${#PREFIX}" -lt 2 ]]; then
    tmux display-message "Prefix too short for autocomplete (need >= 2 chars)"
    exit 0
fi
```

## Phase 2: Searching the scrollback

Now we grab everything tmux has stored — the full scrollback history plus the visible area — and extract matching words.

```bash
# Capture the entire scrollback + visible area
SCROLLBACK="$(tmux capture-pane -t "$PANE_ID" -p -S - -E -)"

# Extract unique words that start with the prefix
COMPLETIONS="$(
    echo "$SCROLLBACK" \
    | grep -oE '[^[:space:]]+' \
    | grep -F "$PREFIX" \
    | grep "^$(printf '%s' "$PREFIX" | sed 's/[][\\.^$*+?(){}|]/\\&/g')" \
    | sort -u \
    | grep -vxF "$PREFIX" \
)"
```

The `-S -` and `-E -` flags on `capture-pane` are the key trick. The dash means "the beginning" and "the end" of the scrollback history respectively, so we get everything tmux has buffered.

The grep pipeline does the filtering:

1. `grep -oE '[^[:space:]]+'` splits the captured text into individual words
2. `grep -F "$PREFIX"` does a fast fixed-string prefilter (this is an optimization — it narrows the set quickly before the regex match)
3. The anchored `grep "^..."` ensures the word actually *starts* with our prefix, not just contains it
4. `sort -u` deduplicates
5. `grep -vxF "$PREFIX"` removes the prefix itself — completing a word to itself isn't useful

The `sed` escaping on the prefix handles special regex characters. If you're completing a word like `foo.bar`, you don't want the dot interpreted as "any character."

## Phase 3: The fzf popup

This is where it gets fun. tmux has had `popup` support since version 3.2, and it's perfect for this — a floating window that appears, does its thing, and disappears.

The tricky part is that the popup runs in its own process. It can't directly access variables from our script. So we write the completions to a temp file and generate a small helper script:

```bash
TMPFILE="$(mktemp /tmp/tmux-autocomplete.XXXXXX)"
echo "$COMPLETIONS" > "$TMPFILE"

HELPER="$(mktemp /tmp/tmux-ac-helper.XXXXXX.sh)"
cat > "$HELPER" <<'INNER'
#!/usr/bin/env bash
set -euo pipefail
TMPFILE="$1"
PREFIX="$2"
PANE_ID="$3"
PREFIX_LEN="${#PREFIX}"

SELECTED="$(cat "$TMPFILE" | fzf --height=100% --layout=reverse \
    --prompt="complete: " --query="" \
    --bind="enter:accept" --no-multi \
    --header="Completing: ${PREFIX}…")"

if [[ -n "$SELECTED" ]]; then
    SUFFIX="${SELECTED:$PREFIX_LEN}"
    if [[ -n "$SUFFIX" ]]; then
        tmux send-keys -t "$PANE_ID" -l -- "$SUFFIX"
    fi
fi

rm -f "$TMPFILE" "$0"
INNER
chmod +x "$HELPER"

tmux popup -E -w 60 -h 16 -T " Autocomplete " \
    bash "$HELPER" "$TMPFILE" "$PREFIX" "$PANE_ID"
```

The important detail is in the `send-keys` call. We don't send the whole selected word — we send only the **suffix**, the part after what's already typed. The `-l` flag means "literal," so special characters in the completion get sent as-is rather than interpreted as tmux key names. The `--` prevents any leading dashes in the suffix from being parsed as flags.

The popup itself is configured with `-E` (close on exit), a fixed width and height, and a title bar. When you press Escape or the process exits, the popup vanishes cleanly.

## tmux APIs used

To recap the tmux features this script leans on:

- **`display-message -p`** — query tmux format variables (cursor position, pane ID)
- **`capture-pane -p`** — dump pane content to stdout, with line range control
- **`popup -E`** — create a floating overlay window that closes on process exit
- **`send-keys -l`** — inject literal text into a pane as if the user typed it

These four commands are remarkably powerful building blocks. If you haven't explored `capture-pane` and `popup` before, they open up a whole category of tmux tooling.

## Installation

Save the script to `~/.local/bin/tmux-autocomplete` and make it executable:

```bash
chmod +x ~/.local/bin/tmux-autocomplete
```

Then add the keybinding to your `~/.tmux.conf`:

```bash
bind-key -T root "M-;" run-shell ~/.local/bin/tmux-autocomplete
```

Reload your tmux config:

```bash
tmux source-file ~/.tmux.conf
```

Now type a partial word in any tmux pane and press `M-;` (Alt-semicolon). If there are matches in your scrollback, you'll get an fzf popup. Select one, press Enter, and the rest of the word gets filled in.

## Requirements

- **tmux 3.2+** (for popup support)
- **fzf** (for the fuzzy finder interface)
- **bash 4+** (for the regex matching with `BASH_REMATCH`)

On macOS, the default `/bin/bash` is version 3. The `#!/usr/bin/env bash` shebang will pick up a Homebrew-installed bash if you have one. If not, `brew install bash` will sort you out.

## Wrapping up

The whole thing is about 80 lines of bash. No compiled dependencies, no plugins to install, no configuration framework — just tmux's own APIs, fzf, and some text processing. It's not a perfect replica of iTerm2's autocomplete (that one also handles mid-word completion and has tighter UI integration), but for the common case of "I typed half a path/command/variable and want the rest," it works well.

The broader pattern here is worth internalizing: tmux's `capture-pane`, `display-message`, and `popup` give you enough to build surprisingly capable tools. Your scrollback is a rich source of context, and fzf makes it searchable. This autocomplete script is one application, but the same approach works for extracting URLs, file paths, git hashes, or anything else that shows up in your terminal.
