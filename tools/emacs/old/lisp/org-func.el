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

;;;###autoload
(defun vde/org-next-visible-heading-or-link (&optional arg)
  "Move to the next visible heading or link, whichever comes first.
With prefix ARG and the point on a heading(link): jump over subsequent
headings(links) to the next link(heading), respectively.  This is useful
to skip over a long series of consecutive headings(links)."
  (interactive "P")
  (let ((next-heading (save-excursion
                        (org-next-visible-heading 1)
                        (when (org-at-heading-p) (point))))
        (next-link (save-excursion
                     (when (vde/org-next-visible-link) (point)))))
    (when arg
      (if (and (org-at-heading-p) next-link)
          (setq next-heading nil)
        (if (and (looking-at org-link-any-re) next-heading)
            (setq next-link nil))))
    (cond
     ((and next-heading next-link) (goto-char (min next-heading next-link)))
     (next-heading (goto-char next-heading))
     (next-link (goto-char next-link)))))

;;;###autoload
(defun vde/org-previous-visible-heading-or-link (&optional arg)
  "Move to the previous visible heading or link, whichever comes first.
With prefix ARG and the point on a heading(link): jump over subsequent
headings(links) to the previous link(heading), respectively.  This is useful
to skip over a long series of consecutive headings(links)."
  (interactive "P")
  (let ((prev-heading (save-excursion
                        (org-previous-visible-heading 1)
                        (when (org-at-heading-p) (point))))
        (prev-link (save-excursion
                     (when (vde/org-next-visible-link t) (point)))))
    (when arg
      (if (and (org-at-heading-p) prev-link)
          (setq prev-heading nil)
        (if (and (looking-at org-link-any-re) prev-heading)
            (setq prev-link nil))))
    (cond
     ((and prev-heading prev-link) (goto-char (max prev-heading prev-link)))
     (prev-heading (goto-char prev-heading))
     (prev-link (goto-char prev-link)))))

;; Adapted from org-next-link to only consider visible links
;;;###autoload
(defun vde/org-next-visible-link (&optional search-backward)
  "Move forward to the next visible link.
When SEARCH-BACKWARD is non-nil, move backward."
  (interactive)
  (let ((pos (point))
        (search-fun (if search-backward #'re-search-backward
                      #'re-search-forward)))
    ;; Tweak initial position: make sure we do not match current link.
    (cond
     ((and (not search-backward) (looking-at org-link-any-re))
      (goto-char (match-end 0)))
     (search-backward
      (pcase (org-in-regexp org-link-any-re nil t)
        (`(,beg . ,_) (goto-char beg)))))
    (catch :found
      (while (funcall search-fun org-link-any-re nil t)
        (let ((folded (org-invisible-p nil t)))
          (when (or (not folded) (eq folded 'org-link))
            (let ((context (save-excursion
                             (unless search-backward (forward-char -1))
                             (org-element-context))))
              (pcase (org-element-lineage context '(link) t)
                (link
                 (goto-char (org-element-property :begin link))
                 (throw :found t)))))))
      (goto-char pos)
      ;; No further link found
      nil)))

;;;###autoload
(defun vde/org-shifttab (&optional arg)
  "Move to the previous visible heading or link.
If already at a heading, move first to its beginning.  When inside a table,
move to the previous field."
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively #'org-table-previous-field))
   ((and (not (bolp)) (org-at-heading-p)) (beginning-of-line))
   (t (call-interactively #'vde/org-previous-visible-heading-or-link))))

;;;###autoload
(defun vde/org-tab (&optional arg)
  "Move to the next visible heading or link.
When inside a table, re-align the table and move to the next field."
  (interactive)
  (cond
   ((org-at-table-p) (org-table-justify-field-maybe)
    (call-interactively #'org-table-next-field))
   (t (call-interactively #'vde/org-next-visible-heading-or-link))))

(provide 'org-func)
;;; org-func.el ends here
