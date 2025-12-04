# Bisect Workflow

Find the commit that introduced a bug using binary search.

## When to Use

- "find bad commit"
- "git bisect"
- "when did this break"
- "regression debugging"

## Quick Commands

### Basic Bisect
```bash
# Start bisect
git bisect start

# Mark current commit as bad
git bisect bad

# Mark known good commit
git bisect good v1.0.0

# Or use commit hash
git bisect good abc1234

# Git will checkout middle commit
# Test and mark:
git bisect bad   # if broken
git bisect good  # if working

# Repeat until found
# Git will show: "abc1234 is the first bad commit"

# End bisect session
git bisect reset
```

### Automated Bisect
```bash
# Start bisect
git bisect start HEAD v1.0.0

# Run automated test
git bisect run ./test.sh

# Git will automatically find bad commit
# Bisect session ends automatically
```

## How Bisect Works

### Binary Search Process
```
Commits: A---B---C---D---E---F---G
         good               bad

# Round 1: Test D (middle)
A---B---C---D---E---F---G
         good   ?     bad
# D is bad

# Round 2: Test B
A---B---C---D
   ?     bad
# B is good

# Round 3: Test C
B---C---D
good ? bad
# Found! C is first bad commit
```

### Efficiency
```bash
# For 100 commits: ~7 tests (log2(100))
# For 1000 commits: ~10 tests (log2(1000))
# Much faster than linear search!
```

## Manual Bisect Process

### Step-by-Step Example
```bash
# 1. Start bisect
git bisect start

# 2. Mark current state as bad
git bisect bad

# 3. Find known good commit
git log --oneline
git bisect good v2.0.0  # or commit hash

# 4. Test current state
npm test  # or whatever test
# If it fails:
git bisect bad
# If it passes:
git bisect good

# 5. Repeat step 4 until done

# 6. Git will show:
# abc1234 is the first bad commit
# commit abc1234
# Author: ...
# Date: ...
#     feat: add user authentication

# 7. View the bad commit
git show abc1234

# 8. End bisect
git bisect reset
```

## Automated Bisect

### Using Test Script
```bash
# Create test script: test.sh
#!/bin/bash
npm test
exit $?  # Return test exit code

# Make executable
chmod +x test.sh

# Run automated bisect
git bisect start HEAD v1.0.0
git bisect run ./test.sh
```

### Test Script Requirements
```bash
# Script must:
# - Exit 0 if commit is GOOD
# - Exit 1-127 (except 125) if commit is BAD
# - Exit 125 if commit cannot be tested (will skip)

# Example test script
#!/bin/bash

# Build project
make build || exit 125  # Skip if build fails

# Run tests
make test
exit $?
```

### Complex Test Script
```bash
#!/bin/bash

# Example: Testing for specific bug
# Bug: API returns 500 on /users endpoint

# Start server
npm start &
SERVER_PID=$!
sleep 5  # Wait for startup

# Test endpoint
response=$(curl -s -o /dev/null -w "%{http_code}" http://localhost:3000/users)

# Cleanup
kill $SERVER_PID

# Check result
if [ "$response" = "200" ]; then
    exit 0  # Good commit
else
    exit 1  # Bad commit
fi
```

## Bisect with Specific Path

### Limit to Specific Files
```bash
# Only bisect commits that touched specific files
git bisect start -- src/auth.ts src/user.ts

# Or specific directory
git bisect start -- src/api/
```

## Handling Build Failures

### Skip Untestable Commits
```bash
# During manual bisect, if commit won't build:
git bisect skip

# Git will try a different commit nearby

# Skip multiple commits
git bisect skip v1.0..v1.5
```

### In Automated Script
```bash
#!/bin/bash

# Try to build
npm install && npm run build
if [ $? -ne 0 ]; then
    exit 125  # Cannot test this commit, skip it
fi

# Run tests
npm test
exit $?
```

## Bisect Subcommands

### Viewing Bisect State
```bash
# See bisect log
git bisect log

# See current bisect state
cat .git/BISECT_LOG

# See remaining commits to test
git bisect visualize
git bisect view  # Same as visualize
```

### Replaying Bisect
```bash
# Save bisect session
git bisect log > bisect-session.txt

# Replay later
git bisect reset
git bisect replay bisect-session.txt
```

## Common Use Cases

### Finding Performance Regression
```bash
#!/bin/bash
# performance-test.sh

# Run benchmark
result=$(go test -bench=. -count=5 | grep "BenchmarkMain" | awk '{print $3}')

# Extract ns/op (assuming format like "1234 ns/op")
nsop=$(echo $result | awk '{print $1}')

# Good if under 1000 ns/op
if [ "$nsop" -lt 1000 ]; then
    exit 0
else
    exit 1
fi
```

```bash
# Run automated bisect
git bisect start HEAD v2.0.0
git bisect run ./performance-test.sh
```

### Finding Broken Feature
```bash
#!/bin/bash
# feature-test.sh

# Setup test environment
docker-compose up -d
sleep 10

# Run specific test
pytest tests/test_user_feature.py

result=$?

# Cleanup
docker-compose down

exit $result
```

### Finding Compilation Error
```bash
#!/bin/bash
# build-test.sh

# Attempt build
make clean
make 2>/dev/null

if [ $? -eq 0 ]; then
    exit 0  # Builds successfully
else
    exit 1  # Build fails
fi
```

## Advanced Techniques

### Bisect Terms (Good/Bad Aliases)
```bash
# Use custom terms instead of good/bad
git bisect start --term-old=fast --term-new=slow

# Now use:
git bisect fast    # instead of good
git bisect slow    # instead of bad
```

### Bisect with Complex Logic
```bash
#!/bin/bash
# complex-test.sh

# Multiple test conditions
build_passes=false
tests_pass=false

# Check build
if make build; then
    build_passes=true
fi

# Check tests
if npm test; then
    tests_pass=true
fi

# Complex logic
if $build_passes && $tests_pass; then
    exit 0  # Good
elif ! $build_passes; then
    exit 125  # Skip - can't test
else
    exit 1  # Bad - builds but tests fail
fi
```

## Bisect Best Practices

1. **Know your good commit**: Find a commit where feature definitely worked
2. **Make test specific**: Test only the broken behavior, not everything
3. **Keep test fast**: Faster tests = faster bisect
4. **Use skip for build failures**: Don't waste time on unbuildable commits
5. **Automate when possible**: Manual testing is error-prone
6. **Test before bisecting**: Verify your test works on good and bad commits
7. **Document the bug**: Clear understanding helps write better tests

## Troubleshooting

### Bisect Not Finding Bug
```bash
# Verify test works
git checkout <known-bad-commit>
./test.sh  # Should fail

git checkout <known-good-commit>
./test.sh  # Should pass

# If both pass or both fail, fix your test script
```

### Inconsistent Test Results
```bash
# Some tests are flaky (pass/fail randomly)
# Solution: Run test multiple times

#!/bin/bash
# stable-test.sh

for i in {1..5}; do
    npm test || exit 1
done
exit 0
```

### Too Many Commits
```bash
# Narrow down range first
git log --oneline v1.0..v2.0  # See all commits

# Try to narrow by date
git bisect start HEAD "$(git rev-list -1 --before='2024-01-01' HEAD)"

# Or by specific commit
git bisect start HEAD abc1234
```

## Real-World Examples

### Finding Memory Leak
```bash
#!/bin/bash
# memory-test.sh

# Run app with memory profiling
node --expose-gc --max-old-space-size=100 app.js &
PID=$!

# Monitor memory for 30 seconds
sleep 30

# Check memory usage
mem=$(ps -o rss= -p $PID)
kill $PID

# Good if under 80MB
if [ "$mem" -lt 80000 ]; then
    exit 0
else
    exit 1
fi
```

### Finding Test Flakiness
```bash
#!/bin/bash
# flaky-test-finder.sh

# Run test 100 times
for i in {1..100}; do
    npm test -- test/flaky.test.js || exit 1
done
exit 0
```

### Finding Breaking Change
```bash
#!/bin/bash
# api-compatibility-test.sh

# Run API compatibility tests
npm run test:api

if [ $? -eq 0 ]; then
    exit 0  # API compatible
else
    exit 1  # Breaking change
fi
```

## Bisect Configuration

```bash
# Show progress
git config --global bisect.showProgress true

# Default number of commits to show in visualize
git config --global bisect.visualizeCommits 20
```

## Resources

- [Git Bisect Documentation](https://git-scm.com/docs/git-bisect)
- [Fighting Regressions with Git Bisect](https://git-scm.com/book/en/v2/Git-Tools-Debugging-with-Git#_binary_search)
- [Automated Testing with Git Bisect](https://www.metaltoad.com/blog/automated-testing-git-bisect)
