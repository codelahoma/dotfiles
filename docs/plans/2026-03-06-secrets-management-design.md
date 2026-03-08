# Secrets Management: Ansible Vault for Local + CI/CD

## Problem

Secrets are stored in `pass` and consumed via mise `{{ exec(command='pass show ...') }}`. This works locally but not in CI/CD — pipelines have no access to the GPG-based password store. Secrets that need to change must be updated in multiple places (pass locally, platform secrets in GitHub/GitLab).

## Design

### Single Source of Truth

An Ansible vault file per project (or shared), committed encrypted to git. One vault password unlocks everything.

### Three Contexts, One File

| Context | Vault password source | Secret consumption |
|---|---|---|
| **Local dev** | `~/.vault_pass` (file, chmod 600) | mise `[env]` calls `~/bin/get-secret` |
| **CI/CD** | `ANSIBLE_VAULT_PASSWORD` env var (from GitHub/GitLab secrets) | Inline vault decrypt in pipeline steps |
| **Backup** | 1Password stores encrypted vault file | Pushed via `op` + Touch ID after edits |

### Components

#### 1. Vault File (`secrets.vault.yml`)

Standard Ansible vault file, one per project or shared:

```yaml
# Before encryption:
anthropic_api_key: "sk-ant-..."
openai_api_key: "sk-..."
gitlab_host: "gitlab.ecksp.com"
portainer_api_key: "ptr_..."
portainer_url: "https://..."
```

Encrypted with `ansible-vault encrypt secrets.vault.yml` and committed to git.

#### 2. Vault Password File (`~/.vault_pass`)

Single file, chmod 600, not committed anywhere. Contains the vault password.

Referenced by:
- `ANSIBLE_VAULT_PASSWORD_FILE=~/.vault_pass` in shell env
- Spacemacs ansible layer for transparent edit
- `~/bin/get-secret` wrapper script

#### 3. `get-secret` Wrapper Script (`~/bin/get-secret`)

Installed via dotfiles (`home/bin/get-secret`), available globally on all local projects:

```bash
#!/usr/bin/env bash
# Usage: get-secret <key> [vault-file]
# Decrypts vault and extracts a single key value.
#
# Vault password resolution:
# ANSIBLE_VAULT_PASSWORD_FILE env var or ~/.vault_pass
```

Not used in CI/CD — pipelines decrypt the vault inline instead.

#### 4. Mise Integration

Replace `pass show` calls in `.mise.toml`:

```toml
# Before:
[env]
ANTHROPIC_API_KEY = "{{ exec(command='pass show anthropic/api-key') }}"

# After:
[env]
ANTHROPIC_API_KEY = "{{ exec(command='get-secret anthropic_api_key') }}"
```

#### 5. 1Password Backup Script (`~/bin/save-vault-to-1p`)

Run manually or hooked to post-encrypt:

```bash
#!/usr/bin/env bash
# Pushes encrypted vault file to 1Password as a document.
# Requires Touch ID via `op` CLI with biometric unlock.
```

#### 6. Spacemacs Ansible Layer

Add the ansible layer to `dotspacemacs.org` with vault password file config:

```elisp
(ansible :variables
         ansible-vault-password-file "~/.vault_pass")
```

This enables `ansible-vault-mode` for transparent decrypt-on-open / encrypt-on-save of vault files.

### Workflow

#### Changing a secret

1. `ansible-vault edit secrets.vault.yml` (or just open in Emacs — auto-decrypts)
2. Edit the value, save (auto-encrypts)
3. `save-vault-to-1p secrets.vault.yml` (Touch ID tap → backed up to 1Password)
4. `git commit -m "chore: update vault"` — encrypted file committed
5. CI/CD pipelines pick up the change on next run — no platform secrets to update

#### Setting up a new project

1. Create `secrets.vault.yml` with needed keys
2. `ansible-vault encrypt secrets.vault.yml`
3. Update `.mise.toml` to use `get-secret`
4. Add `ANSIBLE_VAULT_PASSWORD` to GitHub/GitLab project secrets (one-time)
5. Commit vault file and mise config

#### CI/CD Pipeline Usage

Pipelines decrypt the vault inline — no external scripts needed:

```yaml
# GitLab CI example:
variables:
  ANSIBLE_VAULT_PASSWORD: $VAULT_PASSWORD  # from project CI/CD variables

before_script:
  - echo "$ANSIBLE_VAULT_PASSWORD" > /tmp/.vault_pass
  - ansible-vault decrypt --vault-password-file /tmp/.vault_pass
      --output /tmp/secrets.yml secrets.vault.yml
  - eval $(python3 -c "
      import yaml;
      [print(f'export {k.upper()}={v}')
       for k,v in yaml.safe_load(open('/tmp/secrets.yml')).items()]")
  - rm /tmp/.vault_pass /tmp/secrets.yml
```

### Migration

Incremental — migrate one project at a time:

1. Create vault file with secrets currently in `pass`
2. Update `.mise.toml` to use `get-secret`
3. Verify locally
4. Add vault password to CI/CD platform
5. Verify in pipeline

`pass` remains installed — it holds the vault password and any non-migrated secrets. Fully removing `pass` is optional and not a goal.

### Security Notes

- Vault file is AES-256 encrypted — safe to commit to git
- Vault password file (`~/.vault_pass`) is local-only, chmod 600, gitignored
- CI/CD platforms store only the vault password, not individual secrets
- 1Password backup is the encrypted vault file (still needs password to decrypt)
- Changing the vault password requires re-encrypting and updating all CI/CD platform secrets
- CI/CD inline decrypt writes to `/tmp` and cleans up immediately

### Relation to Mise Agent/Skills

The `get-secret` wrapper and vault file pattern will be encoded in the mise skills (currently paused — see `memory/mise-agent-brainstorm.md`). The "project setup" skill will guide creating vault files and configuring mise to use them.
