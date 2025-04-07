;;; org-func.el --- -*- lexical-binding: t -*-
;;

;; https://endlessparentheses.com/updating-org-mode-include-statements-on-the-fly.html
;;;###autoload
(defun save-and-update-includes ()
  "Update the line numbers of #+INCLUDE:s in current buffer.
Only looks at INCLUDEs that have either :range-begin or :range-end.
This function does nothing if not in `org-mode', so you can safely
add it to `before-save-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\+INCLUDE: *\"\\([^\"]+\\)\".*:range-\\(begin\\|end\\)"
              nil 'noerror)
        (let* ((file (expand-file-name (match-string-no-properties 1)))
               lines begin end)
          (forward-line 0)
          (when (looking-at "^.*:range-begin *\"\\([^\"]+\\)\"")
            (setq begin (match-string-no-properties 1)))
          (when (looking-at "^.*:range-end *\"\\([^\"]+\\)\"")
            (setq end (match-string-no-properties 1)))
          (setq lines (decide-line-range file begin end))
          (when lines
            (if (looking-at ".*:lines *\"\\([-0-9]+\\)\"")
                (replace-match lines :fixedcase :literal nil 1)
              (goto-char (line-end-position))
              (insert " :lines \"" lines "\""))))))))

(defun decide-line-range (file begin end)
  "Visit FILE and decide which lines to include.
BEGIN and END are regexps which define the line range to use."
  (let (l r)
    (save-match-data
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (if (null begin)
            (setq l "")
          (search-forward-regexp begin)
          (setq l (line-number-at-pos (match-beginning 0))))
        (if (null end)
            (setq r "")
          (search-forward-regexp end)
          (setq r (1+ (line-number-at-pos (match-end 0)))))
        (format "%s-%s" (+ l 1) (- r 1)))))) ;; Exclude wrapper

(defun vde/get-outline-path (element)
  "Return the outline path (as a list of titles) for ELEMENT, which is a headline."
  (let (path)
    (while (and element (eq (org-element-type element) 'headline))
      (let ((title (org-element-property :title element)))
        (when title
          (push title path)))
      (setq element (org-element-property :parent element)))
    (reverse path)))

;;;###autoload
(defun vde/org-clock-in-any-heading ()
  "Clock into any Org heading from `org-agenda-files' that is not DONE or CANCELED."
  (interactive)
  (let (headings)
    (dolist (file org-agenda-files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries (lambda ()
                              (let* ((element (org-element-context))
                                     (todo (org-element-property :todo-keyword element)))
                                (when (not (member todo '("DONE" "CANCELED")))
                                  (let* ((path (vde/get-outline-path element)))
                                    (push (list :path path
                                                :file (buffer-file-name)
                                                :position (point))
                                          headings)))))
                           t 'file))))
    (let* (candidates)
      (dolist (h headings)
        (let* ((path (plist-get h :path))
               (path-str (mapconcat 'identity path " > "))
               (file (plist-get h :file))
               (candidate (format "%s : %s" path-str (file-name-nondirectory file)))
               (data (list file (plist-get h :position))))
          (push (cons candidate data) candidates)))
      (let* ((selected-candidate (completing-read "Select heading: " candidates))
             (matching (cl-find-if (lambda (c) (string= (car c) selected-candidate)) candidates)))
        (when matching
          (let* ((data (cdr matching))
                 (file (car data))
                 (pos (cadr data)))
            (find-file file)
            (goto-char pos)
            (org-clock-in)))))))

(provide 'org-func)
;;; org-func.el ends here
