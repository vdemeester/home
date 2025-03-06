;;; config-llm.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; LLM configuration
;;; Code:

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
