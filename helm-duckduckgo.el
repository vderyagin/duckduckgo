;;; helm-duckduckgo.el --- A Helm interface for DuckDuckGo web search engine -*- lexical-binding: t -*-

;; Copyright (C) 2015 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 25 Feb 2015
;; Version: 0.1
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

(defconst helm-duckduckgo-bangs
  '(
    ("Aliexpress store"                             . "aliexpress")
    ("Amazon.com store"                             . "amazon")
    ("BitBucket.org mercurial/git code hosting"     . "bitbucket")
    ("C2 Wiki"                                      . "c2")
    ("DuckDuckGo search engine"                     . "duckduckgo")
    ("EmacsWiki"                                    . "emacswiki")
    ("Free Dictionary"                              . "freedictionary")
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
    ("Google search engine"                         . "google")
    ("Hoogle haskell search engine"                 . "hoogle")
    ("IMDB - internet movie data base"              . "imdb")
    ("IsoHunt torrent tracker"                      . "ih")
    ("Kick Ass Torrents torrent tracker"            . "kickasstorrents")
    ("OpenSubtitles.org subtitles search engine"    . "opensubtitles")
    ("RARBG torrent tracker"                        . "rarbg")
    ("Reddit Subreddits"                            . "subreddit")
    ("Reddit"                                       . "reddit")
    ("Rutracker torrent tracker"                    . "rutracker")
    ("SoundCloud music"                             . "soundcloud")
    ("StackOverflow"                                . "stackoverflow")
    ("The Pirate Bay torrent tracker"               . "thepiratebay")
    ("Tumblr"                                       . "tumblr")
    ("Twitter"                                      . "twitter")
    ("Urban Dictionary"                             . "urbandictionary")
    ("Wayback Machine"                              . "wayback")
    ("Wikipedia (RU)"                               . "wru")
    ("Wikipedia"                                    . "w")
    ("Wiktionary (dictionary)"                      . "wiktionary")
    ("WolframAlpha (search engine)"                 . "wolframalpha")
    ("YouTube (video hosting)"                      . "youtube")
    ))

(defun helm-duckduckgo-read-queries ()
  (let* ((end-of-input nil)
         (map (let ((map (make-sparse-keymap)))
                (set-keymap-parent map minibuffer-local-map)
                (define-key map (kbd "<return>")
                  (lambda ()
                    (interactive)
                    (setq end-of-input t)
                    (call-interactively #'exit-minibuffer)))
                (define-key map (kbd "C-<return>") #'exit-minibuffer)
                map)))
    (cl-loop until end-of-input
             collect (read-from-minibuffer "Search query: " nil map))))

(defun helm-duckduckgo-do-search (_)
  (cl-loop with queries = (helm-duckduckgo-read-queries)
           for bang in (helm-marked-candidates)
           for urls = (mapcar
                       (lambda (query) (concat "http://duckduckgo.com/?q=" (url-hexify-string (format "!%s %s" bang query))))
                       queries)
           do (mapc #'browse-url urls)))

;;;###autoload
(defun helm-duckduckgo ()
  (interactive)
  (helm :prompt "Search with: "
        :sources '((name . "Search Options")
                   (candidates . helm-duckduckgo-bangs)
                   (action . (("Run Search" . helm-duckduckgo-do-search))))
        :buffer "*helm duckduckgo*"))


(provide 'helm-duckduckgo)

;;; helm-duckduckgo.el ends here
