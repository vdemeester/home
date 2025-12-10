# gh-resolve-conflicts

Interactive tool to scan, identify, and resolve merge conflicts in GitHub pull requests using emacs ediff.

## Features

- **Automatic Scanning**: Searches for all open PRs with merge conflicts across organizations
- **Interactive Selection**: Uses fzf for multi-select PR picking with preview
- **Fork-Aware**: Automatically detects and clones your fork, sets up upstream remote
- **Git Worktrees**: Isolates conflict resolution in separate worktrees (or uses existing repo)
- **Emacs Ediff Integration**: Launches emacs with ediff-merge for 3-way conflict resolution
- **Automated Rebase**: Handles git rebase workflow automatically against upstream
- **Auto-Push by Default**: Automatically force-pushes resolved changes to your fork (disable with `--no-push`)
- **Uses Your Emacs Config**: Launches ediff with your full emacs configuration

## Installation

### With Nix

Add to your Nix configuration:

```nix
let
  gh-resolve-conflicts = pkgs.callPackage ./tools/gh-resolve-conflicts { };
in {
  home.packages = [ gh-resolve-conflicts ];
}
```

### Manual

```bash
# Make executable
chmod +x gh-resolve-conflicts.sh

# Optionally link to PATH
ln -s "$(pwd)/gh-resolve-conflicts.sh" ~/.local/bin/gh-resolve-conflicts
```

## Dependencies

- `gh` (GitHub CLI)
- `fzf` (fuzzy finder, for interactive mode)
- `jq` (JSON processor)
- `git`
- `emacs` (for ediff conflict resolution, falls back to git mergetool if not available)

## Usage

### Interactive Mode

Scan all your PRs in the tektoncd organization:

```bash
gh-resolve-conflicts -o tektoncd
```

Scan all your PRs across all organizations:

```bash
gh-resolve-conflicts
```

### Direct PR Mode

Resolve a specific PR directly:

```bash
gh-resolve-conflicts tektoncd/mcp-server#94
```

### Options

```
-w, --worktree DIR      Create worktrees in DIR (default: /tmp/gh-resolve-conflicts-worktrees)
-n, --no-worktree       Use existing repo instead of creating worktrees
-N, --no-push           Do NOT automatically force-push after resolution (default: auto-push)
-o, --org ORG           Filter PRs by organization
-a, --author AUTHOR     Filter PRs by author (default: @me)
-h, --help              Show help message
```

### Examples

**Interactive selection (auto-pushes by default):**
```bash
gh-resolve-conflicts -o tektoncd
```

**Interactive selection without auto-push:**
```bash
gh-resolve-conflicts -o tektoncd -N
```

**Use existing repo (no worktree):**
```bash
cd ~/src/tektoncd/pipeline
gh-resolve-conflicts -n tektoncd/pipeline#9197
```

**Custom worktree directory:**
```bash
gh-resolve-conflicts -w ~/tmp/worktrees -o tektoncd
```

## Workflow

1. **Scan**: Tool searches for open PRs with merge conflicts
2. **Select**: Interactive fzf interface shows conflicting PRs with preview
3. **Fork Detection**: Automatically identifies your fork of the upstream repository
4. **Setup**:
   - Clones your fork (not upstream)
   - Adds upstream as a remote
   - Fetches from both fork and upstream
5. **Checkout**: Creates worktree and checks out PR branch from your fork
6. **Rebase**: Attempts rebase against `upstream/base-branch`
7. **Resolve**: When conflicts occur:
   - Lists all conflicted files
   - Launches emacs ediff for each file
   - Ediff provides 3-way merge interface
   - Automatically marks files as resolved after ediff
8. **Complete**: Continues rebase after all conflicts resolved
9. **Push**: Automatically force-pushes changes to your fork (with `--force-with-lease`, unless `--no-push`)

## Emacs Ediff

When conflicts are detected, the tool launches emacs with ediff-merge using your full emacs configuration:

- **Uses Your Config**: Loads your complete emacs setup (themes, packages, keybindings)
- **3-way merge interface**: See your changes, their changes, and the common ancestor
- **Visual conflict resolution**: Navigate conflicts with keyboard shortcuts
- **Automatic staging**: Resolved files are automatically git-added

### Ediff Controls

- `n` / `p`: Next/previous conflict
- `a`: Choose variant A (yours)
- `b`: Choose variant B (theirs)
- `ab` / `ba`: Combine both variants
- `q`: Quit ediff (saves and marks resolved)

## Worktree Isolation

By default, the tool creates git worktrees for each PR resolution:

**Benefits:**
- Isolates work from your main repository
- Multiple PRs can be resolved in parallel
- Original repo remains untouched
- Easy cleanup
- Automatically handles fork setup (origin = your fork, upstream = original repo)

**Location:**
```
/tmp/gh-resolve-conflicts-worktrees/
├── tektoncd-pipeline/
│   ├── main/              (bare clone of YOUR fork)
│   │   ├── origin  -> vdemeester/tektoncd-pipeline (your fork)
│   │   └── upstream -> tektoncd/pipeline (upstream)
│   └── pr-9197/           (worktree for PR #9197)
└── tektoncd-mcp-server/
    ├── main/              (bare clone of YOUR fork)
    │   ├── origin  -> vdemeester/tektoncd-mcp-server
    │   └── upstream -> tektoncd/mcp-server
    └── pr-94/             (worktree for PR #94)
```

**Cleanup:**
```bash
# Remove specific worktree
git worktree remove /tmp/gh-resolve-conflicts-worktrees/tektoncd-pipeline/pr-9197

# Or just delete the directory
rm -rf /tmp/gh-resolve-conflicts-worktrees/
```

## Use Cases

**Resolve conflicts across multiple repos:**
```bash
# Finds all conflicting PRs in tektoncd org and lets you pick which to fix
gh-resolve-conflicts -o tektoncd
```

**Quick fix for a single PR:**
```bash
# Directly resolve PR #94, auto-pushes when done
gh-resolve-conflicts tektoncd/mcp-server#94
```

**Resolve in existing checkout:**
```bash
# Use your current repo checkout instead of worktree
cd ~/src/tektoncd/chains
gh-resolve-conflicts -n tektoncd/chains#1487
```

## Troubleshooting

**"emacs not found" warning:**
- Tool falls back to `git mergetool`
- Install emacs for better conflict resolution experience

**Ediff doesn't show conflicts:**
- Some conflicts may need manual resolution
- Option to open file in $EDITOR is provided

**Rebase fails:**
- Tool offers options to skip file, abort, or open manually
- Worktree is preserved for manual intervention

**Can't push after resolution:**
- Check if you have write access to the repository
- May need to configure git credentials
- Use `--force-with-lease` manually if needed

## Related Tools

- `gh-restart-failed`: Restart failed GitHub workflow checks
- `gh pr checkout`: GitHub CLI built-in PR checkout
- `gh-pr-worktree`: GitHub CLI extension for PR worktrees

## Contributing

This tool follows the same structure as other tools in `~/src/home/tools/`:

```
gh-resolve-conflicts/
├── gh-resolve-conflicts.sh    Main script
├── default.nix                Nix package definition
└── README.md                  Documentation
```

## License

MIT
