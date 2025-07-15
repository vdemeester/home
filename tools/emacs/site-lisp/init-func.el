;;; init-func.el --- -*- lexical-binding: t -*-
;;

;;;###autoload
(defmacro vde/run-and-delete-frame (name &rest body)
  "Define a function NAME that executes BODY and then closes the current frame.
   Intended for use with emacsclient -c -e."
  `(defun ,name ()
     ,(format "Performs a task and then closes the current frame for %S." name)
     (interactive)
     ,@body
     (delete-frame)))

;;;###autoload
(defun vde/el-load-dir (dir)
  "Load el files from the given folder `DIR'."
  (let ((files (directory-files dir nil "\.el$")))
    (while files
      (load-file (concat dir (pop files))))))

;;;###autoload
(defun vde/short-hostname ()
  "Return hostname in short (aka wakasu.local -> wakasu)."
  (string-match "[0-9A-Za-z-]+" system-name)
  (substring system-name (match-beginning 0) (match-end 0)))


(provide 'init-func)
;;; init-func.el ends here
