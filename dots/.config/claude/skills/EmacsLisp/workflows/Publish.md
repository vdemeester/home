# Publish Workflow

Publish Emacs Lisp packages to MELPA, GNU ELPA, or NonGNU ELPA.

## Package Archives

| Archive | Requirements | Review | Target Audience |
|---------|--------------|--------|-----------------|
| **MELPA** | Public Git repo | Community review | Most packages |
| **MELPA Stable** | Git tags | Same as MELPA | Stable releases |
| **GNU ELPA** | Copyright assignment | FSF review | GNU project packages |
| **NonGNU ELPA** | Free license | FSF review | Non-GNU free software |

## MELPA (Recommended)

### Requirements

1. **Public Git repository**: GitHub, GitLab, SourceHut, etc.
2. **Package quality**:
   - Passes `package-lint`
   - Passes `checkdoc`
   - Byte-compiles without warnings
3. **Documentation**: README with usage instructions
4. **License**: Free software license (GPL recommended)
5. **Tests**: Recommended but not required

### Step 1: Prepare Package

```bash
# Lint package
eask lint package

# Check documentation
eask lint checkdoc

# Byte compile
eask compile

# Run tests
eask test ert

# All checks pass? Continue!
```

### Step 2: Create MELPA Recipe

Fork MELPA repository:
```bash
git clone https://github.com/melpa/melpa.git
cd melpa
git checkout -b add-my-package
```

Create recipe file `recipes/my-package`:

```elisp
(my-package :fetcher github
            :repo "username/my-package")
```

**Recipe Options**:

```elisp
;; GitHub
(my-package :fetcher github
            :repo "username/my-package"
            :files ("*.el" "dir/*.el"))

;; GitLab
(my-package :fetcher gitlab
            :repo "username/my-package")

;; SourceHut
(my-package :fetcher sourcehut
            :repo "username/my-package")

;; Git (generic)
(my-package :fetcher git
            :url "https://example.com/my-package.git"
            :branch "main")

;; With specific files
(my-package :fetcher github
            :repo "username/my-package"
            :files ("*.el" "lisp/*.el"
                    (:exclude "lisp/test-*.el")))

;; Multi-file package
(my-package :fetcher github
            :repo "username/my-package"
            :files (:defaults "extensions/*.el"))
```

### Step 3: Test Recipe Locally

```bash
# In MELPA directory
make recipes/my-package

# Test installation
make sandbox INSTALL=my-package

# Clean up
make clean
```

### Step 4: Submit Pull Request

```bash
git add recipes/my-package
git commit -m "Add my-package recipe"
git push origin add-my-package
```

Create PR at https://github.com/melpa/melpa/pulls

### Step 5: Address Review Feedback

MELPA maintainers will review:
- Recipe correctness
- Package quality
- License
- Documentation

Common feedback:
- Fix package-lint warnings
- Improve docstrings
- Add missing headers
- Simplify recipe

### Step 6: Approval and Publication

Once approved:
- PR is merged
- Package builds automatically
- Available in MELPA within 24 hours

## MELPA Stable

### Requirements

Same as MELPA, plus:
- Git tags for releases
- Semantic versioning

### Create Release

```bash
# Tag release
git tag -a v1.0.0 -m "Release version 1.0.0"
git push origin v1.0.0
```

### Recipe

```elisp
(my-package :fetcher github
            :repo "username/my-package"
            :version-regexp "v\\([0-9.]+\\)")  ; Optional
```

MELPA Stable automatically uses tags.

## GNU ELPA

### Requirements

1. **Copyright assignment**: For packages >300 lines
2. **License**: GPL-compatible
3. **Code quality**: High standards
4. **No external dependencies**: Prefer built-in features

### Process

1. **Email proposal**: Send to `emacs-devel@gnu.org`
   ```
   Subject: [ELPA] New package: my-package

   I would like to submit my-package to GNU ELPA.

   Description: [Brief description]
   Repository: [Git URL]
   License: GPL-3.0-or-later

   [Additional details]
   ```

2. **Copyright assignment**:
   - If >300 lines, complete FSF copyright assignment
   - Process takes 2-4 weeks
   - Forms available from FSF

3. **Code review**:
   - FSF maintainers review code
   - May request changes
   - Higher standards than MELPA

4. **Integration**:
   - Package added to GNU ELPA
   - Automatically built and published

## NonGNU ELPA

### Requirements

1. **Free license**: GPL, MIT, Apache, etc.
2. **No copyright assignment**: Unlike GNU ELPA
3. **Code quality**: Good standards
4. **No proprietary dependencies**

### Process

1. **Email proposal**: Send to `emacs-devel@gnu.org`
   ```
   Subject: [NonGNU ELPA] New package: my-package

   I would like to submit my-package to NonGNU ELPA.

   Description: [Brief description]
   Repository: [Git URL]
   License: MIT

   The package provides [features]...
   ```

2. **Review**: Less strict than GNU ELPA
3. **Integration**: Added to NonGNU ELPA repository

## Version Management

### Semantic Versioning

Follow [SemVer](https://semver.org/):

- **MAJOR**: Incompatible API changes (1.0.0 → 2.0.0)
- **MINOR**: New features, backwards-compatible (1.0.0 → 1.1.0)
- **PATCH**: Bug fixes, backwards-compatible (1.0.0 → 1.0.1)

### Update Version

```elisp
;; In package file header
;; Version: 1.2.3

;; In Eask
(package "my-package" "1.2.3" "Description")
```

### Create Release

```bash
# Update version in files
# Update CHANGELOG.md

git add .
git commit -m "Bump version to 1.2.3"
git tag -a v1.2.3 -m "Release version 1.2.3"
git push origin main
git push origin v1.2.3
```

## CHANGELOG

Maintain `CHANGELOG.md`:

```markdown
# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Feature in development

## [1.2.0] - 2025-01-15

### Added
- New command `my-package-export`
- Support for custom backends

### Changed
- Improved performance of parsing
- Updated documentation

### Fixed
- Bug in file handling

## [1.1.0] - 2025-01-01

### Added
- Initial public release
```

## Package Distribution

### Create Package Tarball

```bash
# With Eask
eask package

# Output: dist/my-package-1.0.0.tar

# Manual
tar -cf my-package-1.0.0.tar my-package.el my-package-utils.el README.md
```

### Test Package Installation

```elisp
;; Install local package
M-x package-install-file RET /path/to/my-package-1.0.0.tar RET

;; Or from directory
M-x package-install-file RET /path/to/package-directory RET
```

## Post-Publication

### Monitor Issues

- Watch GitHub issues
- Respond to bug reports
- Merge pull requests

### Update Package

```bash
# Fix bugs, add features
git commit -am "Fix issue #123"
git push

# MELPA rebuilds automatically
# No PR needed for updates
```

### Deprecation

If deprecating package:

```elisp
;; Add to package file
(make-obsolete 'my-package-old-function
               'my-package-new-function
               "1.5.0")

(define-obsolete-function-alias
  'my-old-function
  'my-new-function
  "1.5.0")

(define-obsolete-variable-alias
  'my-old-var
  'my-new-var
  "1.5.0")
```

## Quality Checklist

Before publishing:

### Code Quality
- [ ] All functions have docstrings
- [ ] All variables have docstrings
- [ ] Lexical binding enabled
- [ ] No byte-compile warnings
- [ ] package-lint passes
- [ ] checkdoc passes

### Documentation
- [ ] README with installation and usage
- [ ] CHANGELOG with version history
- [ ] License file (GPL-3.0-or-later recommended)
- [ ] Code comments where needed

### Testing
- [ ] Tests cover main functionality
- [ ] Tests pass
- [ ] CI configured (GitHub Actions)

### Package Metadata
- [ ] All required headers present
- [ ] Package-Requires correct
- [ ] Keywords appropriate
- [ ] URL points to repository

### Repository
- [ ] .gitignore includes build artifacts
- [ ] Clean git history
- [ ] Tagged releases
- [ ] Issues/PR templates (optional)

## Common Issues

### Recipe Rejected

**Wrong :files**:
```elisp
;; Bad - includes test files
(my-package :fetcher github
            :repo "user/my-package"
            :files ("*.el"))

;; Good - excludes tests
(my-package :fetcher github
            :repo "user/my-package"
            :files (:defaults (:exclude "test-*.el")))
```

### Package Fails to Build

Check MELPA build log:
- https://melpa.org/#/my-package

Common causes:
- Missing dependencies
- Byte-compile errors
- Wrong file paths

### Version Mismatch

```elisp
;; Version in package file
;; Version: 1.0.0

;; Must match git tag
git tag v1.0.0
```

## Best Practices

1. **Start with MELPA**: Easiest to publish
2. **Tag releases**: For MELPA Stable
3. **Semantic versioning**: Clear version scheme
4. **Maintain CHANGELOG**: Document changes
5. **Respond to issues**: Help users
6. **Keep dependencies minimal**: Easier maintenance
7. **Test thoroughly**: Before each release
8. **Document well**: README and docstrings
9. **Follow conventions**: MELPA guidelines
10. **CI/CD**: Automate testing

## Resources

- [MELPA Contributing Guide](https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org)
- [GNU ELPA](https://elpa.gnu.org/)
- [NonGNU ELPA](https://elpa.nongnu.org/)
- [Semantic Versioning](https://semver.org/)
- [Package Archives Wiki](https://www.emacswiki.org/emacs/ELPA)
