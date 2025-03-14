;;; config-llm.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; LLM configuration
;;; Code:

(use-package copilot
  :hook
  (prog-mode . copilot-mode)
  ;; (markdown-mode . copilot-mode) ;; Enable this on-demand only
  ;; (text-mode . copilot-mode) ;; I may not want copilot in org-mode for example.
  (log-edit-mode . copilot-mode)
  (vc-git-log-edit-mode . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("C-g" . copilot-clear-overlay)
        ("C-j" . copilot-next-completion)
        ("C-k" . copilot-previous-completion)
        ("M-RET" . copilot-accept-completion)
        ("C-f" . copilot-accept-completion)
        ("C-l" . copilot-panel-complete))
  :custom
  (copilot-idle-delay 1)
  (copilot-max-char -1)
  (copile-indent-offset-warning-disable t))

(setq copilot-chat-commit-prompt "Here is the result of running `git diff --cached`. Based on this, suggest a **Conventional Commit message**. Ensure the message includes both a clear title describing the change and a body explaining the change. Do not invent anything new; just comprehend the diff and explain it.

- Do not add extra markdown formatting.
- Always make sure the commit message is in markdown format.
- Do not include any additional text outside the commit message.
- Make sure the title is a max of 50 characters long and not more.
- The summaries need to be wrapped to 80 characters long and not more (or break the line).
- Avoid overused words and phrases often associated with AI-generated text. Do not use the following words: *delve, tapestry, vibrant, landscape, realm, embark, excels, vital, comprehensive, intricate, pivotal, moreover, arguably, notably.*
- Avoid the following phrases: *dive into, it’s important to note, it’s important to remember, certainly, here are, important to consider, based on the information provided, remember that, navigating the [landscape]/[complexities of], delving into the intricacies of, a testament to.*
- Do not include any generic AI disclaimers or self-references (e.g., \"As an AI language model...\").

# Conventional Commits 1.0.0

## Summary

Conventional Commits is a specification for commit messages that follows these rules to ensure clarity and consistency:

### Format

`<type>[optional scope]: <description>`

`[body]`

### Types

1. **fix:** A bug fix correlating to a PATCH version.
2. **feat:** A new feature correlating to a MINOR version.

Other types include:

- **build:** Changes to build systems or dependencies.
- **chore:** Maintenance tasks (e.g., dependency updates).
- **ci:** Changes to CI configuration.
- **refactor:** Code changes not adding features or fixing bugs.
- **test:** Changes to or addition of tests.

Here is the result of `git diff --cached`:")

(use-package copilot-chat
  :custom
  (copilot-chat-model "claude-3.7-sonnet")
  :bind
  (("C-c a c p" . copilot-chat-prompt-transient-menu)
   ("C-c a c c" . copilot-chat-insert-commit-message)
   ("C-c a c o" . copilot-chat-optimize)
   ("C-c a c m" . copilot-chat-set-model)
   ("C-c a c w" . my-copilot-chat-copy-source-block)
   ("C-c a c y" . copilot-chat-yank)
   ("C-c a c Y" . copilot-chat-yank-pop)
   ("C-c a c b" . copilot-chat-display)
   ("C-c a c a" . copilot-chat-switch-to-buffer)
   ("C-c a c f" . copilot-chat-custom-prompt-function)
   ("C-c a c s" . copilot-chat-custom-prompt-selection)
   (:map embark-general-map
	 ("M a d" . copilot-chat-doc)
	 ("M a e" . copilot-chat-explain)
	 ("M a o" . copilot-chat-optimize)
	 ("M a p" . copilot-chat-custom-prompt-selection)
	 ("M a r" . copilot-chat-review))
   (:map copilot-chat-prompt-mode-map
         ("C-M-w" . my-copilot-chat-copy-source-block)
         ("C-q" . delete-window)))
  :config
  (setq copilot-chat-prompts copilot-chat-markdown-prompt)
  (defun my-copilot-chat-copy-source-block ()
    "Copy the source block at point to kill ring."
    (interactive)
    (let* ((temp-buffer-name "*copilot-kr-temp*"))
      (with-current-buffer (get-buffer-create temp-buffer-name)
        (erase-buffer)
        (copilot-chat-yank)
        (kill-ring-save (point-min) (point-max))
        (kill-buffer))
      (message "Source block copied to kill ring")))

  (defun copilot-chat-prompt-transient-menu ()
    "Show a transient menu for Copilot Chat actions."
    (interactive)
    (unless (use-region-p)
      (mark-defun))
    (transient-define-prefix copilot-chat-prompt-menu ()
      "Copilot Chat Menu"
      ["Copilot Chat Actions"
       ["Target"
        ("c" "Commit" copilot-chat-insert-commit-message)
        ("o" "Optimize" copilot-chat-optimize)
        ("r" "Review" copilot-chat-review)
        ("f" "Fix" copilot-chat-fix)
        ("e" "Explain" copilot-chat-explain)
        ("d" "Doc" copilot-chat-doc)]
       ["Commands"
        ("d" "Display chat" copilot-chat-display)
        ("h" "Hide chat" copilot-chat-hide)
        ("R" "Reset & reopen" (lambda ()
                                (interactive)
                                (copilot-chat-reset)
                                (copilot-chat-display)))
        ("x" "Reset" copilot-chat-reset)
        ("g" "Go to buffer" copilot-chat-switch-to-buffer)
        ("m" "Set model" copilot-chat-set-model)
        ("q" "Quit" transient-quit-one)]
       ["Actions"
        ("p" "Custom prompt" copilot-chat-custom-prompt-selection)
        ("i" "Ask and insert" copilot-chat-ask-and-insert)
        ("m" "Insert commit message" copilot-chat-insert-commit-message)
        ("b" "Buffers" copilot-chat-transient-buffers)]
       ["Data"
        ("y" "Yank last code block" copilot-chat-yank)
        ("s" "Send code to buffer" copilot-chat-send-to-buffer)]])
    (copilot-chat-prompt-menu))
  :after (copilot embark)
  :commands
  (copilot-chat-mode))

(use-package aidermacs
  :bind (("C-c a a m" . aidermacs-transient-menu))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-auto-commits nil)
  :config
  (aidermacs-setup-minor-mode))

(use-package gptel
  :hook (gptel-mode . poly-gfm-mode)
  :bind
  (:map gfm-mode-map
        ("C-c C-k" . gptel-abort)
        ("C-c C-m" . gptel-menu)
        ("C-c C-c" . gptel-send))
  (:map gptel-mode-map
        ("C-c C-k" . gptel-abort)
        ("C-c C-m" . gptel-menu)
        ("C-c C-c" . gptel-send))
  :config
  (setq gptel-default-mode #'markdown-mode
        gptel-backend
        (gptel-make-ollama "Ollama"
          :host "localhost:11434" 
          :stream t                             
          :models '("mistral-small" "deepseek-r1:7b" "deepseek-coder:6.7b" "llama3.2" "llama3.1")))
  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    ;; :key (password-store-get "groq/api")
    :models '("llama-3.3-70b-versatile"
              "llama-3.1-70b-versatile"
              "llama-3.1-8b-instant"
              "llama3-70b-8192"
              "llama3-8b-8192"
              "mixtral-8x7b-32768"
              "gemma-7b-it")))

(provide 'config-llm)
;;; config-llm.el ends here
