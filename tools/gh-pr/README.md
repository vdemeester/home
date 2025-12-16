# gh-pr

A comprehensive GitHub Pull Request management tool written in Go, consolidating PR creation with template support, workflow management, and conflict resolution.

## Features

- **PR Creation with Templates**: Create pull requests with automatic template discovery and caching
- **Template Management**: List and preview templates from local or remote repositories
- **Remote Template Discovery**: Browse templates from any GitHub repository
- **Batch Commenting**: Comment on multiple pull requests at once using fzf multi-select
- **Workflow Restart**: Automatically restart failed GitHub Actions workflows
- **Conflict Resolution**: Interactive merge conflict resolution with worktree support
- **Template Caching**: Templates are cached for one week to speed up operations

## Installation

```bash
# Build with Nix
nix build .#gh-pr

# Or install to your profile
nix profile install .#gh-pr
```

## Commands

### `gh-pr create`

Create a pull request with optional template support.

```bash
# Create PR interactively (uses gh defaults)
gh-pr create

# Create PR with a specific template
gh-pr create --template bug-fix

# Create draft PR with title and body
gh-pr create --title "Fix bug" --body "Description" --draft

# Refresh template cache
gh-pr create --refresh

# Full example with all options
gh-pr create \
  --title "Add new feature" \
  --template feature \
  --draft \
  --reviewer user1,user2 \
  --assignee user3 \
  --label enhancement,feature \
  --base main \
  --head feature-branch
```

**Options:**
- `-t, --title`: Pull request title
- `-b, --body`: Pull request body (overridden by template if both are provided)
- `--template`: Template name or path
- `-d, --draft`: Create as draft PR
- `--base`: Base branch (default: repository default)
- `--head`: Head branch (default: current branch)
- `-w, --web`: Open in web browser
- `-r, --reviewer`: Request reviewers (comma-separated)
- `-a, --assignee`: Assign users (comma-separated)
- `-l, --label`: Add labels (comma-separated)
- `--refresh`: Bypass template cache and search again

**Template Discovery:**

Templates are automatically discovered from:
- `.github/PULL_REQUEST_TEMPLATE.md`
- `.github/PULL_REQUEST_TEMPLATE/`
- `docs/PULL_REQUEST_TEMPLATE.md`

### `gh-pr comment`

Comment on multiple pull requests at once using interactive selection.

```bash
# Interactive mode - select PRs and enter comment
gh-pr comment

# Pre-specify the comment body
gh-pr comment --body "LGTM! Approving this change."

# Filter by label before selecting
gh-pr comment --label bug --label urgent

# Work with a specific repository
gh-pr comment --repo owner/repo

# Include closed PRs in the selection
gh-pr comment --state all

# Filter by author
gh-pr comment --author username
```

**Options:**
- `-b, --body`: Comment body (will prompt if not provided)
- `-R, --repo`: Repository in "owner/repo" format
- `-l, --label`: Filter PRs by label (can be used multiple times)
- `-a, --author`: Filter PRs by author
- `-s, --state`: Filter by state: open, closed, merged, all (default: open)

**How It Works:**

1. **List PRs**: Fetches pull requests matching your filters using `gh pr list`
2. **Select**: Uses fzf for multi-select with preview pane showing:
   - PR title and author
   - CI check status with visual indicators (✓/✗/●/○)
   - Tab to select, Enter to confirm
3. **Comment**: Prompts for comment body if not provided via `--body`
4. **Post**: Posts the same comment to all selected PRs

**Use Cases:**
- Notify multiple PRs about a related change
- Request updates across multiple related PRs
- Add acknowledgments to a batch of PRs
- Communicate breaking changes to affected PRs

### `gh-pr approve`

Approve and optionally merge pull requests interactively.

```bash
# Approve PRs from default projects (Tekton)
gh-pr approve

# Approve PRs from specific projects
gh-pr approve tektoncd/pipeline tektoncd/cli

# Approve with Prow comment (/lgtm)
gh-pr approve -p tektoncd/pipeline

# Approve and merge
gh-pr approve -m tektoncd/pipeline

# Approve with custom comment
gh-pr approve -c "LGTM! Great work" tektoncd/pipeline

# Approve with editor for multi-line comment
gh-pr approve -C tektoncd/pipeline

# Force merge (requires admin rights)
gh-pr approve -m -f tektoncd/pipeline

# Interactive comment prompt
gh-pr approve -i tektoncd/pipeline
```

**Options:**
- `-p, --prow`: Add Prow `/lgtm` comment (for Prow-based repos)
- `-m, --merge`: Merge PR after approval (uses `--rebase` and `--delete-branch`)
- `-f, --force`: Force merge with `--admin` flag (requires admin rights)
- `-c, --comment`: Custom approval comment
- `-C, --editor`: Open editor for multi-line comment
- `-i, --interactive`: Prompt for comment interactively

**Positional Arguments:**
- `projects`: GitHub repositories in `owner/repo` format (optional)
  - If not provided, uses default Tekton projects:
    - `tektoncd/pipeline`
    - `tektoncd/plumbing`
    - `tektoncd/cli`
    - `tektoncd/mcp-server`

**How It Works:**

1. **List PRs**: Fetches pull requests from each specified project
2. **Select**: Uses fzf for multi-select with preview pane showing:
   - PR number, author, and title
   - CI check status (Tab to select, Enter to confirm)
3. **Approve**: Approves all selected PRs with optional comment
4. **Merge** (optional): If `-m` is set, merges approved PRs
   - Uses `--rebase` to maintain linear history
   - Uses `--delete-branch` to clean up after merge
   - Uses `--admin` if `-f` is set (force merge)

**Comment Handling:**

The tool provides three ways to add approval comments:

1. **Inline comment** (`-c`): Quick one-line comment
   ```bash
   gh-pr approve -c "LGTM! Nice work" tektoncd/pipeline
   ```

2. **Editor comment** (`-C`): Multi-line comment using your `$EDITOR`
   ```bash
   gh-pr approve -C tektoncd/pipeline
   ```

3. **Interactive prompt** (`-i`): Prompt for comment at runtime
   ```bash
   gh-pr approve -i tektoncd/pipeline
   ```

**Prow Integration:**

For repositories using Prow (like Tekton projects), use the `-p` flag to automatically add the `/lgtm` command:

```bash
# This will approve with comment: "/lgtm\n\nLooks good!"
gh-pr approve -p -c "Looks good!" tektoncd/pipeline
```

**Use Cases:**
- Batch approve multiple related PRs
- Approve and merge PRs in one workflow
- Add consistent comments across multiple PRs
- Work with multiple projects efficiently

### `gh-pr list-templates`

List all available PR templates in the current or a remote repository.

```bash
# List templates in current repository
gh-pr list-templates

# List templates from a remote repository
gh-pr list-templates tektoncd/pipeline

# Show template content preview
gh-pr list-templates --verbose

# Refresh cache and list templates
gh-pr list-templates --refresh

# Browse templates from any repo
gh-pr list-templates kubernetes/kubernetes --verbose
```

**Options:**
- `[REPOSITORY]`: Optional repository in "owner/repo" format to search
- `--refresh`: Refresh template cache
- `-v, --verbose`: Show template content preview

**Remote Repository Support:**

You can now browse templates from any GitHub repository without cloning it first! The tool will:
1. Shallow clone the repository to a temporary directory
2. Search for PR templates
3. Cache the results for one week
4. Clean up the temporary clone

This is especially useful for:
- Exploring templates from organizations you contribute to
- Finding good template examples from popular projects
- Quickly checking if a repository uses PR templates

### `gh-pr restart-failed`

Restart failed workflow runs on pull requests with interactive selection.

```bash
# Interactive mode - select PRs with failed checks using fzf
gh-pr restart-failed

# Restart workflows for a specific PR (no selection needed)
gh-pr restart-failed owner/repo#123

# Filter by label before selecting
gh-pr restart-failed --label bug

# Ignore specific workflows
gh-pr restart-failed --ignore "build" --ignore "test"

# Work with a specific repository
gh-pr restart-failed owner/repo
```

**Options:**
- `-i, --ignore`: Ignore workflows matching pattern (can be used multiple times)
- `-l, --label`: Filter PRs by label (can be used multiple times)

**How It Works:**

1. **Find Failed PRs**: Fetches all PRs and filters those with failed checks
2. **Select**: Uses fzf for multi-select with preview pane showing:
   - All CI check statuses with visual indicators (✓/✗/●/○)
   - Author information
   - Tab to select, Enter to confirm
3. **Restart**: Restarts failed workflows for all selected PRs

**Default Behavior:**
- "Label Checker" workflows are ignored by default
- Only restarts workflows that failed due to:
  - `failure`
  - `timed_out`
  - `startup_failure`
  - `action_required`
- Shows failed count for each PR in the selection interface

### `gh-pr resolve-conflicts`

Resolve merge conflicts in pull requests interactively with full worktree support and fzf selection.

```bash
# Search for your conflicting PRs and select with fzf
gh-pr resolve-conflicts

# Resolve a specific PR (no selection)
gh-pr resolve-conflicts owner/repo#123

# Filter by organization before selecting
gh-pr resolve-conflicts -o tektoncd

# Use existing repo instead of creating worktree
gh-pr resolve-conflicts -n

# Don't auto-push after resolving
gh-pr resolve-conflicts -N

# Specify custom worktree directory
gh-pr resolve-conflicts -w /tmp/my-worktrees
```

**Options:**
- `-w, --worktree DIR`: Create worktrees in specified directory (default: `/tmp/gh-resolve-conflicts-worktrees`)
- `-n, --no-worktree`: Use existing repo instead of creating worktrees
- `-N, --no-push`: Don't automatically force-push after resolution
- `-o, --org ORG`: Filter PRs by organization
- `-a, --author USER`: Filter PRs by author (default: `@me`)

**How It Works:**

1. **Find Conflicting PRs**: Searches for open PRs with merge conflicts
2. **Select**: Uses fzf for multi-select with preview pane showing:
   - Branch information (head → base)
   - Merge status
   - CI check status with visual indicators (✓/✗/●/○)
   - Tab to select, Enter to confirm
3. **Setup Worktree**: Creates an isolated worktree for each PR (or uses existing repo)
4. **Fetch Branches**: Fetches both the PR branch and upstream base branch
5. **Rebase**: Attempts to rebase the PR onto the base branch
6. **Resolve Conflicts**: Launches conflict resolution tool:
   - Tries `emacs` with ediff mode first
   - Falls back to `git mergetool` if emacs is unavailable
7. **Force Push**: Optionally force-pushes the resolved changes

**Fork Support:**

The tool automatically handles forked repositories:
- Detects cross-repository PRs
- Adds upstream remote when needed
- Fetches from both fork and upstream
- Pushes to the correct fork after resolution

**Worktree Benefits:**

Using worktrees (default behavior) allows you to:
- Resolve conflicts in isolated environments
- Work on multiple PRs simultaneously
- Keep your main repository clean
- Easily discard worktrees after resolution

## Template Caching

Templates are cached for **7 days** (one week) by default. This significantly speeds up operations when working with the same repository.

**Cache Location:** `~/.cache/gh-pr/`

**Cache Invalidation:**
- Use `--refresh` flag on any command that uses templates
- Cache automatically expires after 7 days
- Manual deletion: `rm -rf ~/.cache/gh-pr/`

## Architecture

The tool is organized into several packages:

```
tools/gh-pr/
├── cmd/gh-pr/              # Main command and subcommands
│   ├── main.go            # Entry point and root command
│   ├── create.go          # PR creation
│   ├── list_templates.go  # Template listing
│   ├── restart_failed.go  # Workflow restart
│   └── resolve_conflicts.go # Conflict resolution (stub)
├── internal/
│   ├── cache/             # Caching with TTL support
│   ├── output/            # Colored terminal output
│   └── templates/         # Template discovery and management
├── go.mod
├── default.nix            # Nix package definition
└── README.md
```

## Examples

### Creating a PR from Claude Code

When Claude suggests creating a PR, the workflow is streamlined:

```bash
# List available templates
gh-pr list-templates

# Create PR with a template
gh-pr create --template feature --title "Add user authentication"

# Create PR with custom content
gh-pr create \
  --title "Implement OAuth login" \
  --body "Adds OAuth 2.0 support for Google and GitHub" \
  --draft \
  --reviewer team-lead
```

### Commenting on Multiple PRs

```bash
# Select and comment on multiple PRs interactively
gh-pr comment

# Comment on all PRs with a specific label
gh-pr comment --label needs-rebase --body "Please rebase on main"

# Notify all your open PRs about a breaking change
gh-pr comment --author @me --body "Note: This depends on #1234"
```

### Restarting Failed Workflows

```bash
# Select PRs with failures and restart workflows
gh-pr restart-failed

# Restart specific PR in another repo (no selection)
gh-pr restart-failed tektoncd/pipeline#1234

# Filter and select PRs with specific label
gh-pr restart-failed --label bug

# Ignore flaky tests
gh-pr restart-failed --ignore "e2e-tests"
```

## Integration with Existing Tools

This tool consolidates and replaces:

- `gh-approve`: Now integrated as `gh-pr approve`
- `gh-restart-failed`: Now integrated as `gh-pr restart-failed`
- `gh-resolve-conflicts`: Now integrated as `gh-pr resolve-conflicts`

The old shell scripts are now deprecated in favor of this unified Go tool.

## Development

```bash
# Run tests
go test ./...

# Format code
go fmt ./...

# Build locally
go build -o gh-pr ./cmd/gh-pr

# Build with Nix
nix build .#gh-pr
```

## Dependencies

- `gh` (GitHub CLI) - Required for all GitHub operations
- `fzf` - Required for interactive PR selection with preview
- `jq` - Required for formatting preview pane and JSON parsing
- Go 1.23+ - For building from source

## License

MIT

## Future Enhancements

- [x] Full implementation of `resolve-conflicts` command
- [x] Remote repository template discovery
- [x] Batch operations on multiple PRs (comment command)
- [ ] Interactive PR selection with `fzf` integration for conflict resolution
- [ ] Support for PR templates in multiple formats (YAML, JSON)
- [ ] Custom cache TTL configuration
- [ ] Integration with review tools
- [ ] Template validation and linting
