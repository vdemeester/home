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
  :hook
  (gptel-mode . visual-line-mode)
  :bind
  (:map gfm-mode-map
        ("C-c C-k" . gptel-abort)
        ("C-c C-m" . gptel-menu)
        ("C-c C-c" . gptel-send))
  (:map gptel-mode-map
        ("C-c C-k" . gptel-abort)
        ("C-c C-m" . gptel-menu)
        ("C-c C-c" . gptel-send))
  :custom
  (gptel-default-mode #'markdown-mode)
  :config
  ;; (general-leader
  ;;   "o"   '(:ignore t :wk "GPTel")
  ;;   "o o" '(gptel :wk "Start GPTel")
  ;;   "o m" '(gptel-menu :wk "GPTel menu"))
  (require 'gptel-curl)
  (require 'gptel-gemini)
  (require 'gptel-ollama)
  (require 'gptel-transient)
  (require 'gptel-rewrite)
  (require 'gptel-org)
  (require 'gptel-openai)
  (require 'gptel-openai-extras)
  (require 'gptel-autoloads)
  (setq gptel-model 'gemini-2.0-pro-exp
	gptel-backend (gptel-make-gemini "Gemini"
			;; :models '("gemini-2.0-flash"
			;; 	  "gemini-2.0-flash-lite-preview-02-05"
			;; 	  "gemini:gemini-2.0-flash-thinking-exp"
			;; 	  "gemini:gemini-2.0-pro-exp")
			:key (passage-get "ai/gemini/api_key"))
	)

  (gptel-make-deepseek "Deepseek"
		       :key  (passage-get "ai/deepseek/api_key")
		       ;; :models '("deepseek-reasoner" "deepseek-chat" )
		       )

  (gptel-make-openai "MistralLeChat"
    :host "api.mistral.ai/v1"
    :endpoint "/chat/completions"
    :protocol "https"
    :key (passage-get "ai/mistralai/api_key")
    :models '("mistral-small"))
  
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (passage-get "ai/openroute/api_key")
    :models '(cognitivecomputations/dolphin3.0-mistral-24b:free
	      cognitivecomputations/dolphin3.0-r1-mistral-24b:free
	      deepseek/deepseek-r1-zero:free
	      deepseek/deepseek-chat:free
	      deepseek/deepseek-r1-distill-qwen-32b:free
	      deepseek/deepseek-r1-distill-llama-70b:free
	      google/gemini-2.0-flash-lite-preview-02-05:free
	      google/gemini-2.0-pro-exp-02-05:free
	      google/gemini-2.5-pro-exp-03-25
	      google/gemini-2.5-pro-exp-03-25:free
	      google/gemma-3-12b-it:free
	      google/gemma-3-27b-it:free
	      google/gemma-3-4b-it:free
	      mistralai/mistral-small-3.1-24b-instruct:free
	      open-r1/olympiccoder-32b:free
	      qwen/qwen2.5-vl-3b-instruct:free
	      qwen/qwen-2.5-coder-32b-instruct:free
	      qwen/qwq-32b:free
              codellama/codellama-70b-instruct
              google/gemini-pro
              google/palm-2-codechat-bison-32k
              meta-llama/codellama-34b-instruct
              mistralai/mixtral-8x7b-instruct
	      openai/gpt-3.5-turbo))

  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key (passage-get "ai/groq/wakasu")
    :models '("llama-3.3-70b-versatile"
              "llama-3.1-70b-versatile"
              "llama-3.1-8b-instant"
              "llama3-70b-8192"
              "llama3-8b-8192"
              "deepseek-r1-distill-qwen-32b"
              "deepseek-r1-distill-llama-70b-specdec"
              "qwen-2.5-coder-32b"
              "mixtral-8x7b-32768"
              "gemma-7b-it"))
  
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '("smollm:latest"
              "llama3.1:latest"
              "deepseek-r1:latest"
              "mistral-small:latest"
              "deepseek-r1:7b"
              "nomic-embed-text:latest")))

(use-package gptel-context
  :after gptel
  :config
  ;; (general-leader
  ;;   "o c" '(:ignore t :which-key "GPTel Context")
  ;;   "o c a" 'gptel-context-add
  ;;   "o c r" 'gptel-context-remove
  ;;   "o c s" '(lambda ()
  ;;              (interactive)
  ;;              (gptel-context-remove-all nil)
  ;;              (unless (use-region-p)
  ;;                (mark-defun))
  ;;              (gptel-context-add)
  ;;              (my-switch-to-gptel-buffer)))
  )

(defun my-switch-to-gptel-buffer (&optional arg)
  "Switch to the most recent buffer with gptel-mode enabled or start it."
  (interactive "P")
  (let (target-buffer)
    (setq target-buffer (cl-find-if 
                         (lambda (buf)
                           (with-current-buffer buf
                             (bound-and-true-p gptel-mode)))
                         (buffer-list)))
    (unless target-buffer
      (call-interactively 'gptel))
    (pop-to-buffer target-buffer)))

(provide 'config-llm)
;;; config-llm.el ends here
