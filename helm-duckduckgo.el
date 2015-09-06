;;; helm-duckduckgo.el --- A Helm interface for DuckDuckGo web search engine -*- lexical-binding: t -*-

;; Copyright (C) 2015 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 25 Feb 2015
;; Version: 0.1
;; Package-Requires: ((helm) (dash-functional))

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

(defconst helm-duckduckgo-bangs
  '(
    ("Google search engine"                         . "google")

    ("Aliexpress store"                             . "aliexpress")
    ("Amazon.com store"                             . "amazon")
    ("BitBucket.org mercurial/git code hosting"     . "bitbucket")
    ("C2 Wiki"                                      . "c2")
    ("DuckDuckGo search engine"                     . "duckduckgo")
    ("EmacsWiki"                                    . "emacswiki")
    ("Encyclopedia Metallum (The Metal Archives)"   . "metal")
    ("Free Dictionary"                              . "thefreedictionary")
    ("Gentoo Packages"                              . "ebuilds")
    ("Gentoo Portage Overlays"                      . "gpo")
    ("Gentoo Wiki"                                  . "gentoo")
    ("Gentoo's Bugzilla"                            . "gbugs")
    ("GitHub.com git code hosting"                  . "github")
    ("Github Gists - code snippet hosting"          . "gist")
    ("GoodReads reading/book recomendation website" . "gr")
    ("Google Images"                                . "i")
    ("Google Maps"                                  . "googlemaps")
    ("Google Translate"                             . "gtranslate")
    ("Hoogle haskell search engine"                 . "hoogle")
    ("IETF RFC"                                     . "rfc")
    ("IMDB - internet movie data base"              . "imdb")
    ("IsoHunt torrent tracker"                      . "ih")
    ("Kick Ass Torrents torrent tracker"            . "kickasstorrents")
    ("OpenSubtitles.org subtitles search engine"    . "opensubtitles")
    ("Project Gutenberg - a digital library"        . "projectgutenberg")
    ("RARBG torrent tracker"                        . "rarbg")
    ("Reddit Subreddits"                            . "subreddit")
    ("Reddit"                                       . "reddit")
    ("Rutracker torrent tracker"                    . "rutracker")
    ("SoundCloud music"                             . "soundcloud")
    ("StackOverflow"                                . "stackoverflow")
    ("StackOverflow"                                . "stackoverflow")
    ("The Pirate Bay torrent tracker"               . "thepiratebay")
    ("Tumblr"                                       . "tumblr")
    ("Twitter"                                      . "twitter")
    ("Urban Dictionary"                             . "urbandictionary")
    ("Wayback Machine"                              . "wayback")
    ("Weather Underground"                          . "wunderground")
    ("Wikipedia (RU)"                               . "wru")
    ("Wikipedia"                                    . "w")
    ("Wiktionary (dictionary)"                      . "wiktionary")
    ("WolframAlpha (search engine)"                 . "wolframalpha")
    ("YouTube (video hosting)"                      . "youtube")
    ))

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
           for prompt = (format "Search query%s: "
                                (if default-value
                                    (format " (default \"%s\")" default-value)
                                  ""))
           for query = (let ((minibuffer-local-map map))
                         (read-string prompt nil nil default-value))
           unless (string-empty-p query) collect query
           until end-of-input))

(defun helm-duckduckgo-search-url (bang query)
  (concat "http://duckduckgo.com/?q=" (url-hexify-string (format "!%s %s" bang query))))

(defun helm-duckduckgo-urls (bangs queries)
  (seq-mapcat (lambda (bang)
                (seq-map (-partial #'helm-duckduckgo-search-url bang)
                         queries))
              bangs))

(defun helm-duckduckgo-do-search (_)
  (seq-each #'browse-url
            (helm-duckduckgo-urls (helm-marked-candidates)
                                  helm-duckduckgo-queries)))

(defun helm-duckduckgo-copy-to-kill-ring (_)
  (kill-new
   (string-join (helm-duckduckgo-urls (helm-marked-candidates)
                                      helm-duckduckgo-queries)
                "\n")))

;;;###autoload
(defun helm-duckduckgo ()
  (interactive)
  (let ((helm-duckduckgo-queries (helm-duckduckgo-read-queries)))
    (helm :prompt "Search with: "
          :sources '((name . "Search Options")
                     (candidates . helm-duckduckgo-bangs)
                     (action . (("Run Search" . helm-duckduckgo-do-search)
                                ("Copy to Kill-ring" . helm-duckduckgo-copy-to-kill-ring))))
          :buffer "*helm duckduckgo*")))

(provide 'helm-duckduckgo)

;;; helm-duckduckgo.el ends here
