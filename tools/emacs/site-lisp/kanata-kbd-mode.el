;;; kanata-kbd-mode.el --- Major mode for editing Kanata .kbd configuration files

;;; Commentary:
;; This package provides a major mode for editing Kanata .kbd files,
;; offering syntax highlighting, basic indentation, and comment support.
;; Kanata is a software keyboard remapper.
;;
;; To use, save this file as kanata-kbd-mode.el in your load-path,
;; and add (require 'kanata-kbd-mode) to your init.el.
;; Files ending in .kbd will automatically use this mode.

;;; Code:

(defvar kanata-kbd-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Treat ; as comment starter
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Parentheses for S-expression like structure
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    ;; Treat symbols as word constituents
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?@ "w" table) ; For aliases like @mykey
    (modify-syntax-entry ?& "w" table) ; For macros like &mymacro
    (modify-syntax-entry ?+ "." table) ; Treat + as punctuation for specific highlighting
    table)
  "Syntax table for `kanata-kbd-mode`.")

;; Keywords and constants for font-locking
(defconst kanata-kbd-top-level-directives
  '("defcfg" "defsrc" "deflayer" "defalias" "defapp")
  "Top-level directives in Kanata .kbd files.")

(defconst kanata-kbd-defcfg-keywords
  '("startup-layer" "tap-hold-timeout" "oneshot-timeout" "fallthrough"
    "process_unmapped_keys" "remap_win_key_to_lmet_for_macos"
    "leader-timeout" "leader-global-timeout" "leader-sequences"
    "compose-timeout" "compose-key" "compose-sequences"
    "unicode-mode-key" "unicode-timeout" "unicode-default-hex-digits"
    "experimental_cmd_allow_config_dir_relative_paths")
  "Keywords used within (defcfg ...).")

(defconst kanata-kbd-defapp-keywords
  '("exec" "title" "class")
  "Keywords used within (defapp ...).")


(defconst kanata-kbd-action-keywords
  '(;; Layer actions
    "layer-toggle" "layer-switch" "layer-clear" "layer-while-held" "layer-switch-when-held"
    "layer-tap-hold" "layer-tap-dance"
    ;; Macros & Multi
    "macro" "macro2" "macro-tap-hold" "multi" "release-key"
    ;; Modifiers and special keys
    "oneshot" "sticky" "transparent" "_" "unmapped"
    ;; Tap-hold and tap-dance
    "tap-hold" "tap-dance"
    ;; Commands
    "cmd" "cmd-async"
    ;; Unicode
    "unicode" "unicode-hex"
    ;; Special actions
    "leader" "compose" "toggle-unicode-mode" "reset-sticky-keys"
    "noexplicit" ; Used with tap-hold for example
    )
  "Special action keywords in Kanata.")

(defconst kanata-kbd-common-key-names
  '(;; Standard keys
    "esc" "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9" "f10" "f11" "f12"
    "grv" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "min" "eq" "bspc"
    "tab" "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "lbrc" "rbrc" "bsls" "bksl" ; Added bksl
    "caps" "a" "s" "d" "f" "g" "h" "j" "k" "l" "scln" "quot" "ret"
    "lsft" "z" "x" "c" "v" "b" "n" "m" "comm" "dot" "slsh" "rsft"
    "lctl" "lmet" "lalt" "spc" "ralt" "rmet" "rctl" "fn" ; Added fn from image context
    ;; Navigation and editing
    "ins" "home" "pgup" "del" "end" "pgdn"
    "up" "left" "down" "right" "rght" ; Added rght as potential alias/typo for right
    ;; Numpad
    "nlck" "kp/" "kp*" "kp-" "kp+" "kp." "kp0" "kp1" "kp2" "kp3" "kp4"
    "kp5" "kp6" "kp7" "kp8" "kp9" "kpenter"
    ;; Media keys
    "mute" "volu" "vold" "prev" "next" "play" "stop" "pp" ; Added pp
    ;; Special keys from image
    "nonusbslash" "brdn" "brup"
    ;; Special
    "C" "S" "A" "M" "W" ; Modifier shorthands like C-a, S-b
    "lC" "lS" "lA" "lM" "lW" ; Left specific modifier shorthands
    "rC" "rS" "rA" "rM" "rW" ; Right specific modifier shorthands
    )
  "Common key names in Kanata. This list is not exhaustive.")

(defconst kanata-kbd-font-lock-keywords
  (let* ((top-level-directives-regexp (regexp-opt kanata-kbd-top-level-directives 'symbols))
         (defcfg-keywords-regexp (regexp-opt kanata-kbd-defcfg-keywords 'symbols))
         (defapp-keywords-regexp (regexp-opt kanata-kbd-defapp-keywords 'symbols))
         (action-keywords-regexp (regexp-opt kanata-kbd-action-keywords 'symbols))
         (common-keys-regexp (regexp-opt kanata-kbd-common-key-names 'symbols))
         (symbol-name-regexp "\\(?:\\sw\\|\\s_\\)+")
         (layer-name-after-directive-regexp (concat "\\(?:" top-level-directives-regexp "\\)\\s-+\\(\\(?:" symbol-name-regexp "\\)\\)"))
         (alias-name-after-defalias-regexp (concat "\\(?:defalias\\)\\s-+\\(\\(?:" symbol-name-regexp "\\)\\)"))
         ;; Specific regex for @aliases
         (at-alias-usage-regexp (concat "@\\(" symbol-name-regexp "\\)"))
         ;; Specific regex for &macros (if you use them differently)
         (amp-macro-usage-regexp (concat "&\\(" symbol-name-regexp "\\)")))

    `(;; Comments (anything after ;)
      (";.+" . font-lock-comment-face)

      ;; Top-level directives
      (,top-level-directives-regexp . font-lock-keyword-face)

      ;; Keywords within (defcfg ...)
      (,defcfg-keywords-regexp . font-lock-type-face)

      ;; Keywords within (defapp ...)
      (,defapp-keywords-regexp . font-lock-type-face)

      ;; Action keywords
      (,action-keywords-regexp . font-lock-builtin-face)

      ;; Layer names after deflayer, or first argument to startup-layer etc.
      (,layer-name-after-directive-regexp 1 font-lock-variable-name-face)

      ;; Alias definition name (after defalias)
      (,alias-name-after-defalias-regexp 1 font-lock-variable-name-face)

      ;; @alias usage (e.g., @mykey) - highlight as function names
      (,at-alias-usage-regexp 1 font-lock-function-name-face)

      ;; &macro usage (e.g., &mymacro) - highlight as constants (or choose another face)
      (,amp-macro-usage-regexp 1 font-lock-constant-face)

      ;; Common key names
      (,(concat "\\<" common-keys-regexp "\\>") . font-lock-constant-face)

      ;; The '+' symbol when used between key names, primarily in defsrc
      (,(concat "\\(?:\\<" common-keys-regexp "\\>\\|\\(?:\\sw\\|\\s_\\)+\\)" ; A key or symbol
                "\\s-*\\(\\+\\)\\s-*"                                        ; The + operator (captured)
                "\\(?:\\<" common-keys-regexp "\\>\\|\\(?:\\sw\\|\\s_\\)+\\)") ; Another key or symbol
       1 font-lock-warning-face) ; Highlight '+' distinctively

      ;; Modifier prefixes like C-, S-, A-, M-, W- when followed by a key
      ("\\([CSAMW]\\(?:[lrCSAMW]\\)*\\)-\\(\\w+\\)" ; Allow stacked modifiers like C-S-a
       (1 font-lock-preprocessor-face) ; The modifier prefix
       (2 font-lock-constant-face))    ; The key itself

      ;; Strings (e.g., for application names in defapp, paths in cmd)
      ("\"\\(?:\\\\.[^\"]*\\|[^\"]\\)*\"" . font-lock-string-face)

      ;; Numbers (e.g., for timeouts, macro steps)
      ("\\b[0-9]+\\b" . font-lock-string-face) ; Using string face for visibility
      ))
  "Font lock keywords for `kanata-kbd-mode`.")


;; Mode definition
;;;###autoload
(define-derived-mode kanata-kbd-mode prog-mode "KanataKBD"
  "Major mode for editing Kanata .kbd configuration files."
  :syntax-table kanata-kbd-mode-syntax-table
  (setq-local font-lock-defaults '(kanata-kbd-font-lock-keywords))
  (setq-local comment-start "; ")
  (setq-local comment-start-skip ";+\\s-*"))

;; Associate .kbd files with this mode
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kbd\\'" . kanata-kbd-mode))

(provide 'kanata-kbd-mode)

;;; kanata-kbd-mode.el ends here
