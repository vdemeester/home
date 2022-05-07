;;; publish.el --- Publish www project -*- lexical-binding: t; -*-
;; Author: Vincent Demeester <vincent@sbr.pm>

;;; Commentary:
;; This script will convert the org-mode files in this directory into
;; html.

;;; Code:
(require 'package)
(require 'publish-common)

(setq org-publish-project-alist
      `(("content"
         :base-directory "content"
         :base-extension "org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :publishing-directory "./public/posts"
         :exclude ,(regexp-opt '("README.org" "draft" "legacy"))
         :auto-sitemap t
         :with-footnotes t
         :with-toc nil
         :with-drawers t
         :sitemap-filename "index.org"
         :sitemap-title "content"
         :sitemap-format-entry sbr/org-sitemap-format-entry
         :sitemap-style list
         :sitemap-sort-files anti-chronologically
         ;; :sitemap-function sbr/org-publish-sitemap
         ;; :html-head-include-scripts nil
         ;; :html-head-include-default-style nil
         ;; :html-head ,sbr-website-html-head
         ;; :html-preamble sbr-website-html-preamble
         ;; :html-postamble ,sbr-website-html-postamble
         )
        ;; TODO: add rss for content
        ("about"
         :base-directory "content/about"
         :base-extension "org"
         :exclude ,(regexp-opt '("README.org" "draft"))
         :index-filename "index.org"
         :recursive nil
         :with-footnotes t
         :with-toc nil
         :with-drawers t
         :publishing-function org-html-publish-to-html
         :publishing-directory "./public/about"
         :html-head-include-scripts nil
         :html-head-include-default-style nil
         :html-head ,sbr-website-html-head
         :html-preamble sbr-website-html-preamble
         :html-postamble ,sbr-website-html-postamble)
        ("index"
         :base-directory "content/"
         :base-extension "org"
         :exclude ,(regexp-opt '("README.org" "draft"))
         :index-filename "index.org"
         :recursive nil
         :with-footnotes t
         :with-toc nil
         :with-drawers t
         :with-title nil
         :publishing-function org-html-publish-to-html
         :publishing-directory "./public"
         :html-head-include-scripts nil
         :html-head-include-default-style nil
         :html-head ,sbr-website-html-head
         :html-preamble sbr-website-html-preamble
         :html-postamble ,sbr-website-html-postamble)
        ("css"
         :base-directory "./css"
         :base-extension ,site-attachments
         :recursive t
         :publishing-directory "./public/css"
         :publishing-function org-publish-attachment
         :recursive t)
        ("images"
         :base-directory "./content/images"
         :base-extension ,site-attachments
         :publishing-directory "./public/images"
         :publishing-function org-publish-attachment
         :recursive t)
        ("assets"
         :base-directory "./assets"
         :base-extension ,site-attachments
         :publishing-directory "./public/assets"
         :publishing-function org-publish-attachment
         :recursive t)
        ;; legacy
        ("posts"
         :base-directory "content/legacy/posts"
         :base-extension "org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :publishing-directory "./public/posts"
         :exclude ,(regexp-opt '("README.org" "draft"))
         :auto-sitemap t
         :with-footnotes t
         :with-toc nil
         :with-drawers t
         :sitemap-filename "index.org"
         :sitemap-title "Posts"
         :sitemap-format-entry sbr/org-sitemap-format-entry
         :sitemap-style list
         :sitemap-sort-files anti-chronologically
         :sitemap-function sbr/org-publish-sitemap
         :html-head-include-scripts nil
         :html-head-include-default-style nil
         :html-head ,sbr-website-html-head
         :html-preamble sbr-website-html-preamble
         :html-postamble ,sbr-website-html-postamble)
        ("posts-rss"
         :base-directory "content/legacy/posts"
         :base-extension "org"
         :recursive t
         :html-link-home "https://vincent.demeester.fr/"
         :rss-link-home "https://vincent.demeester.fr/posts/"
         :html-link-use-abs-url t
         :rss-extension "xml"
         :publishing-directory "./public"
         :publishing-function (sbr/org-rss-publish-to-rss)
         :section-number nil
         :exclude ".*"
         :include ("index.org"))
        ("articles"
         :base-directory "content/legacy/articles"
         :base-extension "org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :publishing-directory "./public/articles"
         :exclude ,(regexp-opt '("README.org" "draft"))
         :auto-sitemap t
         :with-footnotes t
         :with-toc nil
         :with-drawers t
         :sitemap-filename "sitemap.org"
         :sitemap-title "Articles"
         :sitemap-style tree
         :sitemap-sort-files anti-chronologically
         ;;:sitemap-format-entry sbr/org-sitemap-format-entry
         ;;:sitemap-function sbr/org-publish-sitemap
         :html-head-include-scripts nil
         :html-head-include-default-style nil
         :html-head ,sbr-website-html-head
         :html-preamble sbr-website-html-preamble
         :html-postamble ,sbr-website-html-postamble)
        ("articles-assets"
         :exclude ,(regexp-opt '("*.org"))
         :base-directory "content/legacy/articles"
         :base-extension ,site-attachments
         :publishing-directory "./public/articles"
         :publishing-function org-publish-attachment
         :recursive t)
        ("legacy"
         :base-directory "./content/legacy"
         :base-extension ,site-attachments
         :publishing-directory "./public/"
         :publishing-function org-publish-attachment
         :recursive t)
        ("all" :components ("posts" "about" "index" "articles" "articles-assets" "css" "images" "assets" "legacy" "posts-rss"))))

(defun publish ()
  "Build vincent.demeester.fr website"
  (delete-directory (expand-file-name "~/.org-timestamps") t)
  (org-publish-all))

(provide 'publish)
;;; publish.el ends here
