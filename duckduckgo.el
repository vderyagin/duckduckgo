;;; duckduckgo.el --- An interface for DuckDuckGo web search engine -*- lexical-binding: t -*-

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 25 Feb 2015
;; Version: 0.4.2

;; Package-Requires: ((consult))

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.
;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; https://duckduckgo.com/bang

;;; Code:

(require 'consult)
(require 'seq)
(require 'map)

(eval-when-compile
  (require 'subr-x))

(defgroup duckduckgo nil
  "An interface for DuckDuckGo web search engine"
  :prefix "duckduckgo-"
  :group 'tools)

(defcustom duckduckgo-alternate-browser-function #'eww-browse-url
  "Alternate value for `browse-url-browser-function'"
  :group 'duckduckgo
  :type 'function)

(defcustom duckduckgo-queue-buffer-name "*duckduckgo-queue*"
  "Name for buffer used for editing the queue"
  :group 'duckduckgo
  :type 'string)

(defcustom duckduckgo-bangs
  '(
    ("google.com"              . "!google")
    ("aliexpress.com"          . "!aliexpress")
    ("amazon.com"              . "!amazon")
    ("bitbucket.org"           . "!bitbucket")
    ("DuckDuckGo image search" . "!i")
    ("DuckDuckGo"              . "!duckduckgo")
    ("EmacsWiki"               . "!emacswiki")
    ("Encyclopedia Metallum"   . "!metal")
    ("Free Dictionary"         . "!thefreedictionary")
    ("Gentoo Packages"         . "!ebuilds")
    ("GitHub Gists"            . "!gist")
    ("github.com"              . "!github")
    ("gitlab.com"              . "!gitlab")
    ("GoDoc"                   . "!godoc")
    ("goodreads.com"           . "!goodreads")
    ("Google Images"           . "!gisafeoff")
    ("Google Maps"             . "!googlemaps")
    ("Google Translate"        . "!gtranslate")
    ("IETF RFCs"               . "!rfc")
    ("IMDB"                    . "!imdb")
    ("NPM"                     . "!npm")
    ("Project Gutenberg"       . "!projectgutenberg")
    ("Reddit Subreddits"       . "!subreddit")
    ("Reddit"                  . "!reddit")
    ("StackOverflow"           . "!stackoverflow")
    ("TVDB.com"                . "!tvdb")
    ("Twitter"                 . "!twitter")
    ("Urban Dictionary"        . "!urbandictionary")
    ("Wayback Machine"         . "!wayback")
    ("Wikipedia.org"           . "!w")
    ("Wiktionary.org"          . "!wiktionary")
    ("WolframAlpha"            . "!wolframalpha")
    ("YouTube"                 . "!youtube")
    )
  "Search engines to choose from"
  :group 'duckduckgo
  :type '(repeat (cons (string :tag "Name/description")
                       (string :tag "Bang"))))

(defvar duckduckgo--queue nil)
(defvar duckduckgo--previous-search-terms nil)

(defun duckduckgo--read-queries ()
  (cl-loop with end-of-input = nil
           with default-value = (and (region-active-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end)))
           with map = (let ((map (make-sparse-keymap)))
                        (set-keymap-parent map minibuffer-local-map)
                        (define-key map (kbd "<return>")
                                    (lambda ()
                                      (interactive)
                                      (setq end-of-input t)
                                      (call-interactively #'exit-minibuffer)))
                        (define-key map (kbd "C-<return>")
                                    (lambda ()
                                      (interactive)
                                      (setq default-value nil)
                                      (call-interactively #'exit-minibuffer)))
                        map)
           for prompt = (format "Search query%s (%s): "
                                (if default-value
                                    (format " (default \"%s\")" default-value)
                                  "")
                                "press RET to proceed, C-RET to enter another query")
           for query = (let ((minibuffer-local-map map))
                         (read-string prompt nil nil default-value))
           unless (string-empty-p query) collect query into queries
           until (and end-of-input queries)
           finally return queries))

(defun duckduckgo--search-url (website query)
  (let ((query-template (if (string-prefix-p "!" website)
                            "%s %s"
                          "!safeoff site:%s %s")))
    (concat "https://duckduckgo.com/?q="
            (url-hexify-string (format query-template website query)))))

(defun duckduckgo--urls (bangs queries)
  (seq-mapcat (lambda (bang)
                (seq-map (lambda (query) (duckduckgo--search-url bang query))
                         queries))
              bangs))

(defun duckduckgo--do-search (candidates queries)
  (seq-each #'browse-url
            (duckduckgo--urls candidates queries)))

(defun duckduckgo--do-search-alternate-browser (candidates queries)
  (let ((browse-url-browser-function duckduckgo-alternate-browser-function))
    (seq-each #'browse-url
              (duckduckgo--urls candidates queries))))

(defun duckduckgo--copy-to-kill-ring (candidates queries)
  (kill-new
   (string-join (duckduckgo--urls candidates queries)
                "\n")))

(defun duckduckgo--annotate-candidate (candidate)
  (concat
   (propertize " " 'display '(space :align-to center))
   (map-elt duckduckgo-bangs candidate)))

;;;###autoload
(defun duckduckgo (&optional arg)
  (interactive "p")
  (let* ((queries (or (and (not (region-active-p))
                           duckduckgo--queue)
                      (duckduckgo--read-queries)))
         (selected-candidate
          (consult--read
           duckduckgo-bangs
           :require-match nil
           :annotate #'duckduckgo--annotate-candidate))
         (bang (or (map-elt duckduckgo-bangs selected-candidate)
                   selected-candidate)))
    (duckduckgo-clear-queue)
    (setq duckduckgo--previous-search-terms queries)
    (funcall
     (pcase arg
       (1 #'duckduckgo--do-search)
       (4 #'duckduckgo--do-search-alternate-browser)
       (_ #'duckduckgo--copy-to-kill-ring))
     (list bang) queries)))

(defun duckduckgo-again (&optional arg)
  "Run search again on the same terms, can be run multiple times."
  (interactive "p")
  (let* ((queries (or duckduckgo--previous-search-terms
                      (user-error "No previous search")))
         (selected-candidate
          (consult--read
           duckduckgo-bangs
           :require-match nil
           :annotate #'duckduckgo--annotate-candidate))
         (bang (or (map-elt duckduckgo-bangs selected-candidate)
                   selected-candidate)))
    (funcall
     (pcase arg
       (1 #'duckduckgo--do-search)
       (4 #'duckduckgo--do-search-alternate-browser)
       (_ #'duckduckgo--copy-to-kill-ring))
     (list bang) queries)))

;;;###autoload
(defun duckduckgo-add-to-queue ()
  (interactive)
  (let ((region (and (use-region-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end)))))
    (or
     (and region
          (duckduckgo--add-string-to-queue region))
     (duckduckgo--add-string-to-queue (read-string "Query to add to queue: ")))
    (deactivate-mark)))

(defun duckduckgo--add-string-to-queue (str)
  "Add STR to queue, returns `nil' if string was not added, non-`nil' otherwise."
  (when-let* ((str)
              (cleaned-input (string-trim (replace-regexp-in-string "\s*\n\s*" " " str)))
              ((not (string-empty-p cleaned-input))))
    (add-to-list 'duckduckgo--queue cleaned-input)))

;;;###autoload
(defun duckduckgo-queue-edit ()
  (interactive)
  (let ((buf (get-buffer-create duckduckgo-queue-buffer-name)))
    (with-current-buffer buf
      (keymap-local-set "C-c C-c" #'duckduckgo-queue-edit-apply)
      (keymap-local-set "C-c C-k" #'duckduckgo-queue-edit-discard)
      (keymap-local-set "C-c C-x" #'duckduckgo-queue-edit-clear)
      (erase-buffer)
      (save-excursion
        (insert (string-join duckduckgo--queue "\n"))))
    (pop-to-buffer buf)
    (message "%s to save, %s to discard, %s to purge the queue"
             (key-description (where-is-internal 'duckduckgo-queue-edit-apply nil t))
             (key-description (where-is-internal 'duckduckgo-queue-edit-discard nil t))
             (key-description (where-is-internal 'duckduckgo-queue-edit-clear nil t)))))

(defun duckduckgo-queue-edit-apply ()
  (interactive)
  (unless (string= (buffer-name) duckduckgo-queue-buffer-name)
    (user-error "Not supposed to be invoked outside of ddg queue buffer"))
  (duckduckgo-clear-queue)
  (seq-each #'duckduckgo--add-string-to-queue
            (thread-first
              (buffer-substring-no-properties (point-min) (point-max))
              (split-string "\n" t "\s+")
              seq-reverse))
  (kill-buffer))

(defun duckduckgo-queue-edit-discard ()
  (interactive)
  (unless (string= (buffer-name) duckduckgo-queue-buffer-name)
    (user-error "Not supposed to be invoked outside of ddg queue buffer"))
  (kill-buffer))

(defun duckduckgo-queue-edit-clear ()
  (interactive)
  (unless (string= (buffer-name) duckduckgo-queue-buffer-name)
    (user-error "Not supposed to be invoked outside of ddg queue buffer"))
  (duckduckgo-clear-queue)
  (kill-buffer))

(defun duckduckgo-clear-queue ()
  (interactive)
  (setq duckduckgo--queue nil))

(provide 'duckduckgo)

;;; duckduckgo.el ends here
