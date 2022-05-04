;;; publish-common.el --- Commons code for www publishing projects -*- lexical-binding: t; -*-
;; Author: Vincent Demeester <vincent@sbr.pm>

;;; Commentary:
;;
;;; Code:
;; load org
(require 'org)
(require 'dash)
;; load org export functions
(require 'ox-publish)
(require 'ox-rss)
(require 'ox-html)
;; load org link functions
(require 'ol-man)
(require 'ol-git-link)
;; Those are mine
(require 'ol-github)
(require 'ol-gitlab)
(require 'org-attach)
;; load additional libraries
(require 'go-mode)
(require 'css-mode)
(require 'yaml-mode)
(require 'nix-mode)

(require 's)

(setq org-export-use-babel nil)
(setq org-link-abbrev-alist '(("att" . org-attach-expand-link)))

;; setting to nil, avoids "Author: x" at the bottom
(setq org-export-with-section-numbers nil
      org-export-with-smart-quotes t
      org-export-with-toc nil)

(defvar sbr-date-format "%b %d, %Y")

(setq org-html-divs '((preamble "header" "top")
                      (content "main" "content")
                      (postamble "footer" "postamble"))
      org-html-container-element "section"
      org-html-metadata-timestamp-format sbr-date-format
      org-html-checkbox-type 'unicode
      org-html-html5-fancy t
      org-html-doctype "html5"
      org-html-htmlize-output-type 'css
      org-html-htmlize-font-prefix "org-"
      org-src-fontify-natively t
      org-html-coding-system 'utf-8-unix)

(defun sbr/org-export-format-drawer (name content)
  "HTML export of drawer with NAME and CONTENT.
name is the name of the drawer, that will be used as class.
content is the content of the drawer"
  (format "<div class='drawer %s'>\n<h6>%s</h6>\n%s</div>"
          (downcase name)
          (capitalize name)
          content))
(setq org-html-format-drawer-function 'sbr/org-export-format-drawer)

(defun read-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defvar sbr-website-html-head
  "<link rel='icon' type='image/x-icon' href='/images/favicon.ico'/>
<meta name='viewport' content='width=device-width, initial-scale=1'>
<link rel='stylesheet' href='/css/new.css' type='text/css'/>
<link rel='stylesheet' href='/css/syntax.css' type='text/css'/>
<link href='/index.xml' rel='alternate' type='application/rss+xml' title='Vincent Demeester' />")

(defun sbr-website-html-preamble (plist)
  "PLIST: An entry."
  ;; Skip adding subtitle to the post if :KEYWORDS don't have 'post' has a
  ;; keyword
  (when (string-match-p "post" (format "%s" (plist-get plist :keywords)))
    (plist-put plist
               :subtitle (format "Published on %s by %s."
                                 (org-export-get-date plist sbr-date-format)
                                 (car (plist-get plist :author)))))

  ;; Below content will be added anyways
  "<nav>
<img src=\"/images/favicon.ico\" id=\"sitelogo\"/> <a href='/'>home</a> /
<a href='/posts/'>posts</a> (<a href='/index.xml'>rss</a>) /
<a href='/articles/'>articles</a> /
<a href='https://dl.sbr.pm/'>files</a> /
<a href='/about/'>about</a></li>
</nav>")

(defvar sbr-website-html-postamble
  "<footer>
     <span class='questions'>Questions, comments ? Please use my <a href=\"https://lists.sr.ht/~vdemeester/public-inbox\">public inbox</a> by sending a plain-text email to <a href=\"mailto:~vdemeester/public-inbox@lists.sr.ht\">~vdemeester/public-inbox@lists.sr.ht</a>.</span>
     <span class='opinions'>Opinions stated here are my own and do not express the views of my employer, spouse, children, pets, neighbors, secret crushes, favorite authors, or anyone else who is not me. And maybe not even me, depending on how old this is.</span>
     <span class='copyright'>
      Content and design by Vincent Demeester
      (<a rel='licence' href='http://creativecommons.org/licenses/by-nc-sa/3.0/'>Some rights reserved</a>)
    </span><br />
    <span class='engine'>
      Powered by <a href='https://www.gnu.org/software/emacs/'>Gnu Emacs</a> and <a href='https://orgmode.org'>orgmode</a>
    </span>
</footer>")
(defvar site-attachments
  (regexp-opt '("jpg" "jpeg" "gif" "png" "svg"
                "ico" "cur" "css" "js" "woff" "html" "pdf" "otf"))
  "File types that are published as static files.")

(defun sbr/org-sitemap-format-entry (entry style project)
  "Format posts with author and published data in the index page.

ENTRY: file-name
STYLE:
PROJECT: `posts in this case."
  (cond ((not (directory-name-p entry))
         (format "%s — [[file:%s][%s]]
                 :PROPERTIES:
                 :PUBDATE: [%s]
                 :END:"
                 (format-time-string "%Y-%m-%d"
                                     (org-publish-find-date entry project))
                 entry
                 (org-publish-find-title entry project)
                 (format-time-string "%Y-%m-%d"
                                     (org-publish-find-date entry project))))
        ((eq style 'tree) (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun sbr/org-publish-sitemap (title list)
  ""
  (concat "#+TITLE: " title "\n\n"
          (org-list-to-subtree list)))

(defun sbr/org-get-first-paragraph (file)
  "Get string content of first paragraph of file."
  (ignore-errors
    (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (show-all)
    (let ((first-begin (progn
                         (org-forward-heading-same-level 1)
                         (next-line)
                         (point)))
          (first-end (progn
                       (org-next-visible-heading 1)
                       (point))))
      (buffer-substring first-begin first-end)))))

(defun sbr/org-rss-publish-to-rss (plist filename pub-dir)
  "Prepare rss.org file before exporting."
  (let* ((postsdir (plist-get plist :base-directory)))
    (with-current-buffer (find-file filename)
      (erase-buffer)
      (insert "#+TITLE: Posts\n")
      (insert "#+AUTHOR: Vincent Demeester\n")
      (insert "#+OPTIONS: toc:nil\n")
      (let* ((files-all
              (reverse (directory-files "." nil
                                        "[0-9-]+.*\\.org$")))
             (files (seq-subseq files-all 0 (min (length files-all) 30))))
        (message (format "foo: %s" filename))
        (dolist (post files)
          (let* ((post-file post)
                 (post-title (org-publish-find-title post-file plist))
                 (preview-str (sbr/org-get-first-paragraph post-file))
                 (date (replace-regexp-in-string
                        "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)-.*"
                        "\\1" post)))
            (insert (concat "* [[file:" postsdir "/" post "][" post-title "]]\n\n"))
            (org-set-property "ID" post)
            (org-set-property "RSS_TITLE" post-title)
            ;; ox-rss prepends html-link-home to permalink
            (org-set-property "RSS_PERMALINK"
                              (concat postsdir "/"
                                      (file-name-sans-extension post)
                                      ".html"))
            (org-set-property
             "PUBDATE"
             (format-time-string
              "<%Y-%m-%d %a %H:%M>"
              (org-time-string-to-time
               (replace-regexp-in-string
                "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)-.*"
                "\\1" post))))
            (insert preview-str)
            (newline 1)
            (insert (concat "[[file:" postsdir "/" post "][(Read more)]]\n\n"))))
        (save-buffer))))
  (let ((user-mail-address "t")
        (org-export-with-broken-links t)
        (org-rss-use-entry-url-as-guid nil))
    (org-rss-publish-to-rss plist filename pub-dir)))

(advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)

(defun unpackaged/org-export-get-reference (datum info)
  "Like `org-export-get-reference', except uses heading titles instead of random numbers."
  (let ((cache (plist-get info :internal-references)))
    (or (car (rassq datum cache))
        (let* ((crossrefs (plist-get info :crossrefs))
               (cells (org-export-search-cells datum))
               ;; Preserve any pre-existing association between
               ;; a search cell and a reference, i.e., when some
               ;; previously published document referenced a location
               ;; within current file (see
               ;; `org-publish-resolve-external-link').
               ;;
               ;; However, there is no guarantee that search cells are
               ;; unique, e.g., there might be duplicate custom ID or
               ;; two headings with the same title in the file.
               ;;
               ;; As a consequence, before re-using any reference to
               ;; an element or object, we check that it doesn't refer
               ;; to a previous element or object.
               (new (or (cl-some
                         (lambda (cell)
                           (let ((stored (cdr (assoc cell crossrefs))))
                             (when stored
                               (let ((old (org-export-format-reference stored)))
                                 (and (not (assoc old cache)) stored)))))
                         cells)
                        (when (org-element-property :raw-value datum)
                          ;; Heading with a title
                          (unpackaged/org-export-new-title-reference datum cache))
                        ;; NOTE: This probably breaks some Org Export
                        ;; feature, but if it does what I need, fine.
                        (org-export-format-reference
                         (org-export-new-reference cache))))
               (reference-string new))
          ;; Cache contains both data already associated to
          ;; a reference and in-use internal references, so as to make
          ;; unique references.
          (dolist (cell cells) (push (cons cell new) cache))
          ;; Retain a direct association between reference string and
          ;; DATUM since (1) not every object or element can be given
          ;; a search cell (2) it permits quick lookup.
          (push (cons reference-string datum) cache)
          (plist-put info :internal-references cache)
          reference-string))))

(defun unpackaged/org-export-new-title-reference (datum cache)
  "Return new reference for DATUM that is unique in CACHE."
  (cl-macrolet ((inc-suffixf (place)
                             `(progn
                                (string-match (rx bos
                                                  (minimal-match (group (1+ anything)))
                                                  (optional "--" (group (1+ digit)))
                                                  eos)
                                              ,place)
                                ;; HACK: `s1' instead of a gensym.
                                (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                           (match-string 2 ,place)))
                                        (suffix (if suffix
                                                    (string-to-number suffix)
                                                  0)))
                                  (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
    (let* ((title (org-element-property :raw-value datum))
           (ref (url-hexify-string (substring-no-properties title)))
           (parent (org-element-property :parent datum)))
      (while (--any (equal ref (car it))
                    cache)
        ;; Title not unique: make it so.
        (if parent
            ;; Append ancestor title.
            (setf title (concat (org-element-property :raw-value parent)
                                "--" title)
                  ref (url-hexify-string (substring-no-properties title))
                  parent (org-element-property :parent parent))
          ;; No more ancestors: add and increment a number.
          (inc-suffixf ref)))
      ref)))

(provide 'publish-common)
;;; publish-common.el ends here
