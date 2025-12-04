# ReviewPR Workflow

Review a GitHub pull request with comprehensive checks, diff analysis, and approval/feedback workflow.

## When to Use

- "review pr"
- "review pull request"
- "check pr changes"
- "approve pr"
- "request changes on pr"

## Quick Commands

### View PR

```bash
# View PR summary
gh pr view <number>

# View in browser
gh pr view <number> --web

# View specific fields
gh pr view <number> --json title,body,state,author,reviews
```

### Review PR

```bash
# Approve
gh pr review <number> --approve

# Request changes
gh pr review <number> --request-changes --body "Feedback here"

# Comment only
gh pr review <number> --comment --body "Looks good, minor suggestions"
```

### Checkout and Test

```bash
# Checkout PR branch locally
gh pr checkout <number>

# Run tests
npm test  # or your test command

# Return to previous branch
git checkout -
```

## Workflow Steps

### 1. Get PR Information

```bash
# Fetch comprehensive PR data
gh pr view <number> --json \
  number,title,body,state,author,headRefName,baseRefName,\
  mergeable,statusCheckRollup,reviews,commits

# Store for analysis
```

**Present summary:**
```
PR #123: Add user authentication
Author: @username
Branch: feature/auth → main
Status: OPEN
Mergeable: TRUE

Commits: 5
Files changed: 12
Reviews: 1 approval, 0 changes requested
```

### 2. Check CI/CD Status

```bash
# Get check status
gh pr checks <number>

# Detailed status
gh pr view <number> --json statusCheckRollup --jq '
  .statusCheckRollup |
  group_by(.conclusion) |
  map({status: .[0].conclusion, count: length})
'
```

**Present status:**
```
Checks: ✓ 8/8 passing
- ✓ tests (3m 24s)
- ✓ lint (1m 12s)
- ✓ build (2m 45s)
- ✓ security scan (2m 01s)
```

### 3. Review Changes

```bash
# View diff
gh pr diff <number>

# View diff stats
gh pr diff <number> --name-status

# Save diff for analysis
gh pr diff <number> > /tmp/pr_${number}.diff
```

**Analyze changes:**
- Identify file types (code, tests, docs, config)
- Look for security concerns
- Check test coverage
- Review code quality

### 4. Check for Common Issues

**Security concerns:**
```bash
# Look for potential security issues in diff
gh pr diff <number> | grep -E "(password|secret|api_key|token|private_key)"
```

**Test coverage:**
```bash
# Check if tests were added
gh pr diff <number> --name-status | grep -c "_test\|spec\|test_"
```

**Large changes:**
```bash
# Count lines changed
gh pr diff <number> --stat | tail -1
```

### 5. Request Feedback or Approve

**If issues found:**
```bash
gh pr review <number> --request-changes --body "$(cat <<'EOF'
Thanks for the PR! I have a few suggestions:

1. **Security**: Line 45 has a hardcoded API key
   - Move to environment variable or config

2. **Testing**: Missing tests for the new auth middleware
   - Add tests in `tests/auth.test.ts`

3. **Documentation**: Update README with auth setup instructions

Let me know if you have questions!
EOF
)"
```

**If approved:**
```bash
gh pr review <number> --approve --body "$(cat <<'EOF'
LGTM!

Changes look good:
✓ Tests added and passing
✓ Security best practices followed
✓ Code is well-documented

Approved to merge.
EOF
)"
```

### 6. Suggest Next Steps

Based on review outcome:

**If approved and checks passing:**
```
✓ PR approved
✓ All checks passing
✓ No merge conflicts

Ready to merge:
gh pr merge <number> --squash  # or --merge, --rebase
```

**If changes requested:**
```
Requested changes on PR #123

Waiting for author to address feedback.
Track updates: gh pr view <number>
```

**If checks failing:**
```
Cannot approve: checks are failing

Fix issues first:
gh pr checks <number>
```

## Advanced Usage

### Review Checklist

Create a review checklist template:

```bash
cat > review_checklist.md <<'EOF'
## Code Review Checklist

### Functionality
- [ ] Changes match the PR description
- [ ] No obvious bugs or logical errors
- [ ] Edge cases are handled

### Code Quality
- [ ] Code follows project style guidelines
- [ ] No code duplication
- [ ] Functions are small and focused
- [ ] Variable names are clear

### Testing
- [ ] Tests added for new functionality
- [ ] Tests cover edge cases
- [ ] All tests passing

### Security
- [ ] No hardcoded secrets or credentials
- [ ] Input validation present
- [ ] No obvious security vulnerabilities

### Documentation
- [ ] Code is commented where necessary
- [ ] README updated if needed
- [ ] API docs updated if applicable

### Performance
- [ ] No obvious performance issues
- [ ] Database queries are optimized
- [ ] No memory leaks
EOF
```

### Inline Comments

Add specific line comments:

```bash
# Comment on specific lines (requires GitHub API)
gh api repos/:owner/:repo/pulls/<number>/reviews \
  -f event=COMMENT \
  -f body="Review comments" \
  -F comments[][path]="file.ts" \
  -F comments[][line]=42 \
  -F comments[][body]="Consider extracting this to a function"
```

### Batch Review

Review multiple PRs:

```bash
# Get all open PRs
gh pr list --json number,title,author

# Review each
for pr in $(gh pr list --json number --jq '.[].number'); do
  echo "Reviewing PR #$pr"
  gh pr view "$pr"
  # ... review process
done
```

## Common Scenarios

### Scenario 1: Quick Approval

```bash
# View PR
gh pr view 123

# Check status
gh pr checks 123

# If all good, approve
gh pr checks 123 && \
  gh pr review 123 --approve --body "LGTM! Approving." && \
  gh pr merge 123 --squash
```

### Scenario 2: Detailed Review

```bash
# Checkout and test locally
gh pr checkout 123
npm test
npm run lint

# If passing, approve
if [ $? -eq 0 ]; then
  gh pr review 123 --approve --body "Tested locally. All passing."
else
  gh pr review 123 --request-changes --body "Tests failing locally. Please investigate."
fi

# Return to main
git checkout main
```

### Scenario 3: Collaborative Review

```bash
# Check existing reviews
gh pr view 123 --json reviews

# Add your review
gh pr review 123 --comment --body "Adding review comments. Will approve after @otherreviewer weighs in."

# Later, after discussion
gh pr review 123 --approve
```

## Custom Tool: Enhanced Review

For complex review workflows:

```bash
# In tools/ directory
./ReviewPR.ts --pr 123 --checklist --auto-test --report
```

**Tool capabilities:**
- Automatic checklist generation
- Clone and test PR in isolated environment
- Generate review report with metrics
- Compare against style guide automatically
- Integration with code quality tools (SonarQube, etc.)

## Review Templates

### Approval Template

```bash
gh pr review <number> --approve --body "$(cat <<'EOF'
✓ Code review complete

**What I reviewed:**
- Code changes and logic
- Test coverage
- Security considerations
- Documentation

**Summary:**
All looks good! Changes are well-implemented and tested.

Approved to merge.
EOF
)"
```

### Request Changes Template

```bash
gh pr review <number> --request-changes --body "$(cat <<'EOF'
Thanks for the PR! I have some feedback:

## Required Changes

1. **Issue 1**
   - Description
   - Suggested fix

2. **Issue 2**
   - Description
   - Suggested fix

## Optional Suggestions

- Suggestion 1
- Suggestion 2

Let me know if you have questions!
EOF
)"
```

### Comment Template

```bash
gh pr review <number> --comment --body "$(cat <<'EOF'
A few suggestions (non-blocking):

💡 **Performance**: Consider caching the result of `expensiveFunction()`
💡 **Testing**: Might be worth adding a test for the error case
💡 **Documentation**: Could add a code comment explaining why we use this approach

Feel free to address in this PR or a follow-up.
EOF
)"
```

## Best Practices

1. **Review promptly**: Don't let PRs sit unreviewed
2. **Be constructive**: Frame feedback positively
3. **Explain reasoning**: Don't just say "change this"
4. **Test locally when needed**: For complex changes, test yourself
5. **Check the context**: Read linked issues/discussions
6. **Review tests first**: Ensure tests validate the changes
7. **Look for security issues**: Always consider security implications
8. **Don't nitpick**: Focus on important issues
9. **Approve fast, merge slow**: Approve quickly but merge after all reviewers approve
10. **Follow up**: Check if your feedback was addressed

## Integration with Other Workflows

- **CheckStatus**: Check CI before reviewing
- **CreatePR**: Review immediately after creating your own PRs
- **ManageIssues**: Link issues in review comments

## Resources

- [gh pr review documentation](https://cli.github.com/manual/gh_pr_review)
- [GitHub PR review guide](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/reviewing-changes-in-pull-requests)
- [Code review best practices](https://google.github.io/eng-practices/review/)
