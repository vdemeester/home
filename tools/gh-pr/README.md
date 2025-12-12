# gh-pr

A comprehensive GitHub Pull Request management tool written in Go, consolidating PR creation with template support, workflow management, and conflict resolution.

## Features

- **PR Creation with Templates**: Create pull requests with automatic template discovery and caching
- **Template Management**: List and preview available PR templates
- **Workflow Restart**: Automatically restart failed GitHub Actions workflows
- **Conflict Resolution**: (Placeholder) Will support interactive merge conflict resolution
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

### `gh-pr list-templates`

List all available PR templates in the repository.

```bash
# List templates
gh-pr list-templates

# Show template content preview
gh-pr list-templates --verbose

# Refresh cache and list templates
gh-pr list-templates --refresh
```

**Options:**
- `--refresh`: Refresh template cache
- `-v, --verbose`: Show template content preview

### `gh-pr restart-failed`

Restart failed workflow runs on pull requests.

```bash
# Interactive mode - list all PRs with failed checks
gh-pr restart-failed

# Restart workflows for a specific PR
gh-pr restart-failed owner/repo#123

# Filter by label
gh-pr restart-failed --label bug

# Ignore specific workflows
gh-pr restart-failed --ignore "build" --ignore "test"

# Work with a specific repository
gh-pr restart-failed owner/repo
```

**Options:**
- `-i, --ignore`: Ignore workflows matching pattern (can be used multiple times)
- `-l, --label`: Filter PRs by label (can be used multiple times)

**Default Behavior:**
- "Label Checker" workflows are ignored by default
- Only restarts workflows that failed due to:
  - `failure`
  - `timed_out`
  - `startup_failure`
  - `action_required`

### `gh-pr resolve-conflicts`

Resolve merge conflicts in pull requests (placeholder - not yet implemented).

```bash
# This command is not yet fully implemented
gh-pr resolve-conflicts

# For now, use the existing shell script:
gh-resolve-conflicts
```

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

### Restarting Failed Workflows

```bash
# See all PRs with failures and restart them
gh-pr restart-failed

# Restart specific PR in another repo
gh-pr restart-failed tektoncd/pipeline#1234

# Ignore flaky tests
gh-pr restart-failed --ignore "e2e-tests"
```

## Integration with Existing Tools

This tool is designed to consolidate and replace:

- `gh-restart-failed`: Shell script for restarting failed workflows
- `gh-resolve-conflicts`: Shell script for resolving merge conflicts (not yet migrated)

The old tools remain available during the transition period.

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
- `jq` - Used for JSON parsing in workflow operations
- Go 1.23+ - For building from source

## License

MIT

## Future Enhancements

- [ ] Full implementation of `resolve-conflicts` command
- [ ] Interactive PR selection with `fzf` integration
- [ ] Support for PR templates in multiple formats (YAML, JSON)
- [ ] Batch operations on multiple PRs
- [ ] Custom cache TTL configuration
- [ ] Integration with review tools
