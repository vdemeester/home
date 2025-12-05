# Test Workflow

Write and run tests for Emacs Lisp packages using ERT (Emacs Lisp Regression Testing).

## ERT (Built-in Testing Framework)

ERT is the standard testing framework for Emacs Lisp.

### Basic Test Structure

```elisp
;;; my-package-test.el --- Tests for my-package  -*- lexical-binding: t -*-

(require 'ert)
(require 'my-package)

(ert-deftest my-package-test-basic ()
  "Test basic functionality."
  (should (= 2 (+ 1 1)))
  (should (equal "hello" "hello"))
  (should (functionp 'my-package-enable)))

(ert-deftest my-package-test-error ()
  "Test that errors are raised."
  (should-error (my-package-divide-by-zero))
  (should-error (my-package-invalid-input "") :type 'wrong-type-argument))

(ert-deftest my-package-test-buffer ()
  "Test buffer operations."
  (with-temp-buffer
    (insert "test content")
    (goto-char (point-min))
    (should (looking-at "test"))
    (should (= (point-max) 13))))

(provide 'my-package-test)
;;; my-package-test.el ends here
```

## ERT Assertions

### should

```elisp
(ert-deftest test-should ()
  "Test should assertions."
  ;; Basic equality
  (should (= 1 1))
  (should (equal "a" "a"))
  (should (string= "hello" "hello"))

  ;; Boolean tests
  (should t)
  (should (listp '(1 2 3)))
  (should (functionp 'car))

  ;; Negation
  (should-not nil)
  (should-not (= 1 2)))
```

### should-error

```elisp
(ert-deftest test-errors ()
  "Test error handling."
  ;; Any error
  (should-error (error "Something went wrong"))

  ;; Specific error type
  (should-error (/ 1 0) :type 'arith-error)
  (should-error (car 'not-a-list) :type 'wrong-type-argument))
```

### Custom Assertions

```elisp
(defun should-contain (list element)
  "Assert that LIST contains ELEMENT."
  (should (member element list)))

(ert-deftest test-custom-assertion ()
  (should-contain '(1 2 3) 2))
```

## Test Organization

### Test File Location

```
my-package/
├── my-package.el
├── my-package-core.el
└── test/
    ├── my-package-test.el
    ├── my-package-core-test.el
    └── test-helper.el
```

### Test Helper

**test/test-helper.el**:

```elisp
;;; test-helper.el --- Test helpers  -*- lexical-binding: t -*-

;; Add parent directory to load path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))

(require 'my-package)

;; Helper functions
(defun test-helper-create-temp-file (content)
  "Create temporary file with CONTENT."
  (let ((file (make-temp-file "my-package-test-")))
    (with-temp-file file
      (insert content))
    file))

(provide 'test-helper)
;;; test-helper.el ends here
```

## Running Tests

### Interactive (in Emacs)

```elisp
;; Run all tests
M-x ert RET t RET

;; Run specific test
M-x ert RET my-package-test-basic RET

;; Run tests matching pattern
M-x ert RET "my-package.*" RET

;; Rerun failed tests
M-x ert-results-rerun-test-at-point
```

### Batch Mode

```bash
# Run all tests
emacs -batch -l my-package.el -l my-package-test.el -f ert-run-tests-batch-and-exit

# With better output
emacs -batch -l ert -l my-package.el -l my-package-test.el \
    --eval "(ert-run-tests-batch-and-exit)"
```

### With Eask

```bash
# Run all tests
eask test ert

# Run specific test file
eask test ert my-package-test.el

# With coverage (if using undercover)
eask test ert --coverage
```

## Advanced Testing

### Mocking

```elisp
(ert-deftest test-with-mocking ()
  "Test with function mocking."
  (cl-letf (((symbol-function 'current-time)
             (lambda () '(0 0 0))))
    (should (equal (current-time) '(0 0 0)))))

(ert-deftest test-mock-user-input ()
  "Mock user input."
  (cl-letf (((symbol-function 'read-string)
             (lambda (prompt) "mocked input")))
    (should (string= (read-string "Enter: ") "mocked input"))))
```

### Fixtures

```elisp
(defvar my-package-test-fixture nil
  "Test fixture data.")

(defun my-package-test-setup ()
  "Set up test fixture."
  (setq my-package-test-fixture
        (with-temp-buffer
          (insert "fixture content")
          (buffer-string))))

(defun my-package-test-teardown ()
  "Tear down test fixture."
  (setq my-package-test-fixture nil))

(ert-deftest test-with-fixture ()
  "Test using fixture."
  (unwind-protect
      (progn
        (my-package-test-setup)
        (should (string= my-package-test-fixture "fixture content")))
    (my-package-test-teardown)))
```

### Parameterized Tests

```elisp
(defun make-addition-test (a b expected)
  "Create test for addition of A and B expecting EXPECTED."
  (lambda ()
    (should (= (+ a b) expected))))

(ert-deftest test-addition-1 ()
  (funcall (make-addition-test 1 1 2)))

(ert-deftest test-addition-2 ()
  (funcall (make-addition-test 2 3 5)))

;; Or use a macro
(defmacro def-addition-test (name a b expected)
  "Define addition test NAME for A + B = EXPECTED."
  `(ert-deftest ,name ()
     ,(format "Test that %s + %s = %s" a b expected)
     (should (= (+ ,a ,b) ,expected))))

(def-addition-test test-add-positive 2 3 5)
(def-addition-test test-add-negative -1 1 0)
```

### Testing Async Code

```elisp
(ert-deftest test-async-operation ()
  "Test asynchronous operation."
  (let ((done nil)
        (result nil))
    (my-package-async-operation
     (lambda (res)
       (setq result res
             done t)))

    ;; Wait for callback
    (with-timeout (5 (error "Timeout"))
      (while (not done)
        (sleep-for 0.1)))

    (should (equal result expected-value))))
```

## Buttercup (Alternative BDD Framework)

### Installation

```elisp
;; In Eask
(development
 (depends-on "buttercup"))
```

### Basic Test

```elisp
;;; my-package-test.el --- Tests using Buttercup  -*- lexical-binding: t -*-

(require 'buttercup)
(require 'my-package)

(describe "my-package"
  (describe "basic functionality"
    (it "should add numbers correctly"
      (expect (+ 2 3) :to-equal 5))

    (it "should handle strings"
      (expect (concat "hello" " " "world")
              :to-equal "hello world")))

  (describe "error handling"
    (it "should throw error on invalid input"
      (expect (my-package-invalid-operation)
              :to-throw 'error))))

;;; my-package-test.el ends here
```

### Run Buttercup

```bash
# With Eask
eask exec buttercup -L .

# Direct
buttercup -L . -L test
```

## Coverage

### Using undercover

```elisp
;; In Eask
(development
 (depends-on "undercover"))

;; In test file
(when (require 'undercover nil t)
  (undercover "*.el"
              (:exclude "my-package-test.el")))
```

### Run with Coverage

```bash
# Generate coverage report
eask test ert

# View coverage in Coveralls (CI integration)
```

## CI/CD Testing

### GitHub Actions

```yaml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest]
        emacs-version: ['29.1', '29.2', 'snapshot']

    steps:
      - uses: actions/checkout@v4

      - uses: jcs090218/setup-emacs@master
        with:
          version: ${{ matrix.emacs-version }}

      - uses: emacs-eask/setup-eask@master

      - name: Install dependencies
        run: eask install-deps

      - name: Run tests
        run: eask test ert

      - name: Upload coverage
        if: matrix.os == 'ubuntu-latest' && matrix.emacs-version == '29.2'
        uses: codecov/codecov-action@v3
```

## Best Practices

1. **Test public API**: Focus on public functions
2. **Use descriptive names**: `test-feature-behavior` not `test1`
3. **One assertion per concept**: Multiple related assertions OK
4. **Clean up**: Use `unwind-protect` for cleanup
5. **Test edge cases**: Empty lists, nil, extremes
6. **Mock external dependencies**: File system, network, time
7. **Fast tests**: Keep tests under 1 second each
8. **Independent tests**: Each test should work alone
9. **Readable failures**: Clear error messages
10. **Test errors**: Verify error handling

## Common Patterns

### Testing Mode Activation

```elisp
(ert-deftest test-mode-activation ()
  "Test that mode activates correctly."
  (with-temp-buffer
    (my-package-mode 1)
    (should my-package-mode)
    (should (memq 'my-package--hook some-hook))
    (my-package-mode -1)
    (should-not my-package-mode)))
```

### Testing File Operations

```elisp
(ert-deftest test-file-processing ()
  "Test file processing."
  (let ((temp-file (make-temp-file "my-package-test-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "test content"))
          (should (my-package-process-file temp-file))
          (should (file-exists-p temp-file)))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))
```

### Testing Hooks

```elisp
(ert-deftest test-hooks ()
  "Test hook execution."
  (let ((called nil))
    (add-hook 'my-package-mode-hook
              (lambda () (setq called t)))
    (my-package-mode 1)
    (should called)))
```

## Debugging Tests

### Run Single Test

```elisp
M-x eval-defun  ; On test definition
M-x ert RET test-name RET
```

### Debug Failed Test

```elisp
;; Enable debugger on error
M-x toggle-debug-on-error

;; Or in test
(ert-deftest test-with-debug ()
  (let ((debug-on-error t))
    ;; Test code
    ))
```

### Print Debug Info

```elisp
(ert-deftest test-with-output ()
  "Test with debug output."
  (message "Starting test...")
  (let ((result (my-function)))
    (message "Result: %S" result)
    (should (= result expected))))
```

## Resources

- [ERT Manual](https://www.gnu.org/software/emacs/manual/html_node/ert/)
- [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup)
- [Undercover](https://github.com/undercover-el/undercover.el)
- [Testing Best Practices](https://github.com/flycheck/flycheck/blob/master/doc/contributor/contributing.rst#testing)
