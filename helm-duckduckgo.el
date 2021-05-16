;;; helm-duckduckgo.el --- A Helm interface for DuckDuckGo web search engine -*- lexical-binding: t -*-

;; Copyright (C) 2015-2021 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 25 Feb 2015
;; Version: 0.2.5
;; Package-Requires: ((helm))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'helm)
(require 'seq)
(require 'subr-x)

(defgroup helm-duckduckgo nil
  "Helm interface for DuckDuckGo web search engine"
  :prefix "helm-duckduckgo-"
  :group 'helm
  :group 'tools)

(defcustom helm-duckduckgo-alternate-browser-function #'eww-browse-url
  "Alternate value for `browse-url-browser-function'"
  :group 'helm-duckduckgo
  :type 'function)

(defcustom helm-duckduckgo-bangs
  '(
    ("Google search engine"                         . "!google")
    ("Google Feeling Lucky"                         . "!lucky")

    ("Aliexpress store"                             . "!aliexpress")
    ("Amazon.com store"                             . "!amazon")
    ("BitBucket.org mercurial/git code hosting"     . "!bitbucket")
    ("DuckDuckGo image search"                      . "!i")
    ("DuckDuckGo search engine"                     . "!duckduckgo")
    ("Elixir documentation"                         . "!elixir-docs")
    ("EmacsWiki"                                    . "!emacswiki")
    ("Encyclopedia Metallum (The Metal Archives)"   . "!metal")
    ("Free Dictionary"                              . "!thefreedictionary")
    ("Gentoo Packages"                              . "!ebuilds")
    ("Gentoo Portage Overlays"                      . "!gpo")
    ("Gentoo Wiki"                                  . "!gentoo")
    ("Gentoo's Bugzilla"                            . "!gbugs")
    ("GitHub Gists - code snippet hosting"          . "!gist")
    ("GitHub.com git code hosting"                  . "!github")
    ("GitLab.com git code hosting"                  . "!gitlab")
    ("GoDoc search"                                 . "!godoc")
    ("GoodReads reading/book recomendation website" . "!goodreads")
    ("Google Images"                                . "!googleimages")
    ("Google Maps"                                  . "!googlemaps")
    ("Google Translate"                             . "!gtranslate")
    ("Hoogle haskell search engine"                 . "!hoogle")
    ("IETF RFC"                                     . "!rfc")
    ("IMDB - internet movie data base"              . "!imdb")
    ("NPM package search"                           . "!npm")
    ("Project Gutenberg - a digital library"        . "!projectgutenberg")
    ("Reddit Subreddits"                            . "!subreddit")
    ("Reddit"                                       . "!reddit")
    ("Rotten Tomatoes"                              . "!rottentomatoes")
    ("RubyGems.org search"                          . "!rubygems")
    ("SoundCloud music"                             . "!soundcloud")
    ("StackOverflow"                                . "!stackoverflow")
    ("TVDB.com - An open database for TV fans"      . "!tvdb")
    ("Twitter"                                      . "!twitter")
    ("Urban Dictionary"                             . "!urbandictionary")
    ("Wayback Machine"                              . "!wayback")
    ("Weather Underground"                          . "!wunderground")
    ("Wikipedia"                                    . "!w")
    ("Wiktionary (dictionary)"                      . "!wiktionary")
    ("WolframAlpha (search engine)"                 . "!wolframalpha")
    ("YouTube (video hosting)"                      . "!youtube")
    ("ruby-doc.org"                                 . "!rubydoc")
    )
  "Search engines to choose from"
  :group 'helm-duckduckgo
  :type '(repeat (cons (string :tag "Name/description")
                       (string :tag "Bang"))))

(defvar helm-duckduckgo-queries nil)

(defun helm-duckduckgo-read-queries ()
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

(defun helm-duckduckgo-search-url (website query)
  (let ((query-template (if (string-prefix-p "!" website)
                            "%s %s"
                          "!safeoff site:%s %s")))
    (concat "http://duckduckgo.com/?q="
            (url-hexify-string (format query-template website query)))))

(defun helm-duckduckgo-urls (bangs queries)
  (seq-mapcat (lambda (bang)
                (seq-map (lambda (query) (helm-duckduckgo-search-url bang query))
                         queries))
              bangs))

(defun helm-duckduckgo-do-search (&rest _)
  (seq-each #'browse-url
            (helm-duckduckgo-urls (helm-marked-candidates)
                                  helm-duckduckgo-queries)))

(defun helm-duckduckgo-do-search-alternate-browser (&rest _)
  (let ((browse-url-browser-function helm-duckduckgo-alternate-browser-function))
    (helm-duckduckgo-do-search)))

(defun helm-duckduckgo-copy-to-kill-ring (&rest _)
  (kill-new
   (string-join (helm-duckduckgo-urls (helm-marked-candidates)
                                      helm-duckduckgo-queries)
                "\n")))

;;;###autoload
(defun helm-duckduckgo ()
  (interactive)
  (let ((helm-duckduckgo-queries (helm-duckduckgo-read-queries))
        (mode-line '("search engine(s)"
                     "RET:Run search f2:Run search in alternate browser f3:Copy URLs to kill-ring"))
        (actions '(("Run search in default browser"
                    . helm-duckduckgo-do-search)
                   ("Run search in alternate browser"
                    . helm-duckduckgo-do-search-alternate-browser)
                   ("Copy search URL(s) to kill-ring"
                    . helm-duckduckgo-copy-to-kill-ring))))
    (helm :prompt "Search with: "
          :buffer "*helm duckduckgo*"
          :sources (list (list (cons 'name  "Search Options")
                               (cons 'candidates helm-duckduckgo-bangs)
                               (cons 'action actions)
                               (cons 'mode-line mode-line))
                         (helm-build-dummy-source "Search different website (use website URL or !bang)"
                           :action actions
                           :mode-line mode-line)))))

(provide 'helm-duckduckgo)

;;; helm-duckduckgo.el ends here
