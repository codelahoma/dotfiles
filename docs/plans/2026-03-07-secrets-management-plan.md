# Secrets Management Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace local-only `pass` secrets with Ansible Vault so secrets work in both local dev (via mise) and CI/CD pipelines.

**Architecture:** Encrypted vault files committed to git, decrypted locally by `get-secret` wrapper and inline in CI/CD. Spacemacs ansible layer provides transparent edit. 1Password backup via `op` CLI.

**Tech Stack:** Ansible Vault, mise, bash, Spacemacs ansible layer, 1Password CLI (`op`)

---

### Task 1: Create Vault Password File

**Files:**
- Create: `~/.vault_pass` (local only, not in dotfiles repo)

**Step 1: Generate a strong vault password**

```bash
openssl rand -base64 32 > ~/.vault_pass
chmod 600 ~/.vault_pass
```

**Step 2: Verify permissions**

Run: `ls -la ~/.vault_pass`
Expected: `-rw-------` permissions, file contains a single line of base64

**Step 3: Store vault password in pass (backup)**

```bash
pass insert ansible/vault-password < ~/.vault_pass
```

---

### Task 2: Set ANSIBLE_VAULT_PASSWORD_FILE in Shell

**Files:**
- Modify: `home/.zshrc`

**Step 1: Add env var export to .zshrc**

Add after the mise activation block (around line 112):

```bash
# Ansible vault password file for secret decryption
export ANSIBLE_VAULT_PASSWORD_FILE="$HOME/.vault_pass"
```

**Step 2: Verify**

Run: `source ~/.zshrc && echo $ANSIBLE_VAULT_PASSWORD_FILE`
Expected: `/Users/rodk/.vault_pass`

**Step 3: Commit**

```bash
git add home/.zshrc
git commit -m "feat(shell): add ANSIBLE_VAULT_PASSWORD_FILE env var"
```

---

### Task 3: Create get-secret Wrapper Script

**Files:**
- Create: `home/bin/get-secret`

**Step 1: Write the script**

```bash
#!/usr/bin/env bash
set -euo pipefail

# Usage: get-secret <key> [vault-file]
# Decrypts an Ansible vault file and extracts a single YAML key value.
#
# Vault password resolution:
#   1. ANSIBLE_VAULT_PASSWORD_FILE env var
#   2. ~/.vault_pass
#
# Vault file resolution:
#   1. Second argument if provided
#   2. secrets.vault.yml in current directory
#   3. secrets.vault.yml in repo root (git rev-parse --show-toplevel)

KEY="${1:?Usage: get-secret <key> [vault-file]}"
VAULT_FILE="${2:-}"

# Find vault password file
PASS_FILE="${ANSIBLE_VAULT_PASSWORD_FILE:-$HOME/.vault_pass}"
if [[ ! -f "$PASS_FILE" ]]; then
  echo "Error: vault password file not found: $PASS_FILE" >&2
  exit 1
fi

# Find vault file
if [[ -z "$VAULT_FILE" ]]; then
  if [[ -f "secrets.vault.yml" ]]; then
    VAULT_FILE="secrets.vault.yml"
  else
    REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || true)"
    if [[ -n "$REPO_ROOT" && -f "$REPO_ROOT/secrets.vault.yml" ]]; then
      VAULT_FILE="$REPO_ROOT/secrets.vault.yml"
    else
      echo "Error: no secrets.vault.yml found" >&2
      exit 1
    fi
  fi
fi

# Decrypt and extract key
ansible-vault decrypt --vault-password-file "$PASS_FILE" --output - "$VAULT_FILE" 2>/dev/null \
  | python3 -c "import yaml,sys; d=yaml.safe_load(sys.stdin); v=d.get('$KEY',''); print(v if v else '')" \
  | tr -d '\n'
```

**Step 2: Make executable**

```bash
chmod +x home/bin/get-secret
```

**Step 3: Link dotfiles**

```bash
homeshick link dotfiles
```

**Step 4: Test the script shows usage when called without args**

Run: `get-secret 2>&1; echo "exit: $?"`
Expected: `Usage: get-secret <key> [vault-file]` and exit code 1

**Step 5: Commit**

```bash
git add home/bin/get-secret
git commit -m "feat: add get-secret wrapper for Ansible Vault"
```

---

### Task 4: Create Test Vault File and Verify get-secret

**Files:**
- Create: `/tmp/test-vault.yml` (temporary, for testing only)

**Step 1: Create a test vault file**

```bash
cat > /tmp/test-secrets.yml <<'EOF'
test_key: "hello-world"
another_key: "secret-value"
EOF
ansible-vault encrypt --vault-password-file ~/.vault_pass /tmp/test-secrets.yml
```

**Step 2: Test get-secret extracts a value**

Run: `get-secret test_key /tmp/test-secrets.yml`
Expected: `hello-world` (no trailing newline)

**Step 3: Test get-secret with missing key returns empty**

Run: `get-secret nonexistent /tmp/test-secrets.yml`
Expected: empty output

**Step 4: Clean up**

```bash
rm /tmp/test-secrets.yml
```

---

### Task 5: Create Initial Vault File from Pass Secrets

**Files:**
- Create: `secrets.vault.yml` (in dotfiles repo root, encrypted)

**Step 1: Export current pass secrets to a YAML file**

```bash
cat > /tmp/dotfiles-secrets.yml <<EOF
anthropic_api_key: "$(pass show anthropic/api-key)"
gitlab_host: "$(pass show gitlab/host)"
openai_api_key: "$(pass show openai/api-key)"
openai_base_url: "$(pass show openai/base-url)"
openai_model_name: "$(pass show openai/model-name)"
openai_model: "$(pass show openai/model)"
openai_transcribe_model: "$(pass show openai/transcribe-model)"
org_inbox_api_key: "$(pass show org-inbox/api-key)"
portainer_api_key: "$(pass show portainer/api-key)"
portainer_url: "$(pass show portainer/url)"
proxmox_host: "$(pass show proxmox/host)"
proxmox_node: "$(pass show proxmox/node)"
proxmox_token_name: "$(pass show proxmox/token-name)"
proxmox_token_value: "$(pass show proxmox/token-value)"
proxmox_user: "$(pass show proxmox/user)"
truenas_api_key: "$(pass show truenas/api-key)"
EOF
```

**Step 2: Encrypt the vault file**

```bash
ansible-vault encrypt --vault-password-file ~/.vault_pass \
  --output secrets.vault.yml /tmp/dotfiles-secrets.yml
rm /tmp/dotfiles-secrets.yml
```

**Step 3: Verify round-trip**

Run: `get-secret anthropic_api_key`
Expected: matches output of `pass show anthropic/api-key`

**Step 4: Commit**

```bash
git add secrets.vault.yml
git commit -m "feat: add encrypted vault file with secrets from pass"
```

---

### Task 6: Migrate Global Mise Config to Use get-secret

**Files:**
- Modify: `~/.mise.toml`

**Step 1: Read current config**

```bash
cat ~/.mise.toml
```

**Step 2: Replace pass show calls with get-secret**

```toml
[tools]
node = "latest"
python = "3.13.12"

[env]
ModelName = "{{ exec(command='get-secret openai_model_name') }}"
OpenAIKey = "{{ exec(command='get-secret openai_api_key') }}"
OPENAI_API_KEY = "{{ exec(command='get-secret openai_api_key') }}"
```

**Step 3: Verify**

Run: `mise env | grep OPENAI_API_KEY`
Expected: same value as `pass show openai/api-key`

**Step 4: Commit (if ~/.mise.toml is tracked)**

Note: `~/.mise.toml` is not currently tracked in the dotfiles repo. User decides whether to add it.

---

### Task 7: Migrate Homelab Mise Config

**Files:**
- Modify: `~/gitlab/homelab/.mise.toml`

**Step 1: Read current config**

```bash
cat ~/gitlab/homelab/.mise.toml
```

**Step 2: Replace pass show calls with get-secret**

All `pass show X/Y` calls become `get-secret X_Y` using the vault file at the dotfiles repo root (since homelab doesn't have its own vault file yet).

```toml
[env]
GITLAB_HOST = "{{ exec(command='get-secret gitlab_host ~/dotfiles/secrets.vault.yml') }}"
```

Or create a project-specific vault file if homelab should be self-contained. User decides.

**Step 3: Verify**

Run: `cd ~/gitlab/homelab && mise env | grep PORTAINER`
Expected: same values as before

---

### Task 8: Create save-vault-to-1p Script

**Files:**
- Create: `home/bin/save-vault-to-1p`

**Step 1: Write the script**

```bash
#!/usr/bin/env bash
set -euo pipefail

# Usage: save-vault-to-1p [vault-file]
# Pushes encrypted vault file to 1Password as a document.
# Requires biometric unlock configured for op CLI.

VAULT_FILE="${1:-secrets.vault.yml}"
ITEM_TITLE="ansible-vault-$(basename "$(pwd)")"

if [[ ! -f "$VAULT_FILE" ]]; then
  echo "Error: vault file not found: $VAULT_FILE" >&2
  exit 1
fi

# Check if item already exists
if op document get "$ITEM_TITLE" >/dev/null 2>&1; then
  op document edit "$ITEM_TITLE" "$VAULT_FILE"
  echo "Updated '$ITEM_TITLE' in 1Password"
else
  op document create "$VAULT_FILE" --title "$ITEM_TITLE"
  echo "Created '$ITEM_TITLE' in 1Password"
fi
```

**Step 2: Make executable**

```bash
chmod +x home/bin/save-vault-to-1p
```

**Step 3: Link and test**

```bash
homeshick link dotfiles
save-vault-to-1p secrets.vault.yml
```

Expected: Touch ID prompt, then confirmation message.

**Step 4: Commit**

```bash
git add home/bin/save-vault-to-1p
git commit -m "feat: add save-vault-to-1p backup script"
```

---

### Task 9: Add Ansible Layer to Spacemacs

**Files:**
- Modify: `home/dotspacemacs.org`

**Step 1: Read the Development Tools layer section**

```bash
grep -n 'Development Tools' home/dotspacemacs.org
```

**Step 2: Add ansible layer after the Development Tools section**

Add a new subsection after "Development Tools" (after line 228):

```org
*** Configuration Management

#+begin_src emacs-lisp :noweb-ref config-layers
  (ansible :variables
           ansible-vault-password-file "~/.vault_pass")
#+end_src
```

**Step 3: Tangle**

```bash
emacs --batch -l org --eval '(org-babel-tangle-file "home/dotspacemacs.org")'
```

**Step 4: Verify**

Run: `grep -A2 'ansible' home/.spacemacs.d/init.el`
Expected: `(ansible :variables ansible-vault-password-file "~/.vault_pass")`

**Step 5: Commit**

```bash
git add home/dotspacemacs.org home/.spacemacs.d/init.el
git commit -m "feat(spacemacs): add ansible layer with vault password file"
```

---

### Task 10: Final Verification and Cleanup

**Step 1: Verify get-secret works end-to-end**

```bash
get-secret anthropic_api_key
get-secret portainer_url
get-secret proxmox_host
```

Each should return the correct value.

**Step 2: Verify mise picks up secrets**

```bash
mise env | grep -E 'OPENAI|ANTHROPIC'
```

**Step 3: Verify 1Password backup exists**

```bash
op document get "ansible-vault-dotfiles" --out-file /tmp/verify-vault.yml
diff secrets.vault.yml /tmp/verify-vault.yml
rm /tmp/verify-vault.yml
```

Expected: files are identical.

**Step 4: Push branch**

```bash
git push
```
