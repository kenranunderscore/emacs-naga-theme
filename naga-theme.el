;;; naga-theme.el --- Dark color theme with green foreground color

;; Copyright (C) 2021-2022 Johannes Maier

;; Author: Johannes Maier <johannes.maier@mailbox.org>
;; Version: 0.1
;; Homepage: https://github.com/kenranunderscore/emacs-naga-theme
;; Keywords: faces themes
;; Package-Requires: ((emacs "24.1"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The main feature of this dark color theme is its usage of a green
;; foreground color, achieving a retro look while still being easy on
;; the eyes.

;; Note that while I've been using this as my main theme for nearly a
;; year now, it still is very much a work in progress, as I've only
;; styled and checked the packages I actually use so far.  Some main
;; colors might still be subject to change, I'm mainly unhappy with
;; pink (maybe try out a more purple-ish tone instead) and the
;; coloring of strings.

;;; Code:

(deftheme naga
  "Created 2021-09-26.")

(require 'color)

(defmacro create-theme-colors ()
  "Expects the color variables to be bound."
  '(mapcar
    (lambda (entry)
      (list (car entry)
            `((t ,@(cdr entry)))))
    `((default (:foreground ,fg :background ,bg))
      (minibuffer-prompt (:foreground ,light-olive))
      (highlight (:foreground ,fg :background ,dark-blue))
      (region (:background ,dark-blue))
      (secondary-selection (:foreground "black" :background ,(color-darken-name "dark green" 7)))
      (vertical-border (:foreground "gray30"))
      (help-key-binding (:foreground ,gold :background ,bg :box ,gold))
      (link (:foreground ,cyan :underline t))
      (font-lock-builtin-face (:foreground ,orange))
      (font-lock-comment-face (:foreground ,comment))
      (font-lock-constant-face (:foreground ,pink))
      (font-lock-doc-face (:slant oblique :foreground "olivedrab4"))
      (font-lock-function-name-face (:foreground ,cyan))
      (font-lock-keyword-face (:foreground ,yellow))
      (font-lock-preprocessor-face (:inherit (font-lock-constant-face)))
      (font-lock-string-face (:foreground ,string))
      (font-lock-type-face (:foreground "olivedrab3"))
      (font-lock-variable-name-face (:foreground ,sea-green))
      (font-lock-warning-face (:slant italic :foreground ,orange-red))
      (fringe (:background ,bg))
      (warning (:foreground ,orange-red :weight regular))
      (mode-line (:background "#041a04" :foreground ,fg :box ,fg))
      (mode-line-buffer-id (:weight bold))
      (mode-line-emphasis (:weight bold))
      (mode-line-inactive (:box "#555555" :background ,bg :foreground ,comment))
      (isearch (:foreground ,bg :weight semi-bold :background ,fg))
      (lazy-highlight (:foreground ,fg :background "blue3"))
      (show-paren-match (:foreground ,bg :background "cyan4"))
      (show-paren-mismatch (:foreground "red" :background ,dark-blue))

      ;; orderless
      (orderless-match-face-0 (:foreground ,orange))
      (orderless-match-face-1 (:foreground ,yellow))
      (orderless-match-face-2 (:foreground ,pink))
      (orderless-match-face-3 (:foreground ,light-olive))

      ;; outline-*, and by extension org-level-*
      (outline-1 (:weight bold :foreground ,fg))
      (outline-2 (:foreground ,yellow))
      (outline-3 (:foreground ,cyan))
      (outline-4 (:foreground ,orange))
      (outline-5 (:foreground ,pink))
      (outline-6 (:foreground ,light-olive))
      (outline-7 (:foreground ,sea-green))
      (outline-8 (:foreground "dark khaki"))

      ;; company
      (company-tooltip (:background "gray10"))
      (company-tooltip-common (:foreground ,orange))
      (company-tooltip-selection (:background ,dark-blue :weight bold))

      ;; which-key
      (which-key-key-face (:foreground ,yellow))
      (which-key-group-description-face (:foreground ,sea-green))
      (which-key-command-description-face (:foreground ,fg))

      ;; dired and related
      (diredfl-dir-name (:foreground ,light-olive))
      (diredfl-file-name (:foreground ,fg))
      (diredfl-file-suffix (:foreground ,fg))
      (diredfl-ignored-file-name (:inherit (font-lock-comment-face)))

      ;; line numbers
      (line-number (:foreground "gray15"))
      (line-number-current-line (:foreground "dark green"))

      ;; org
      (org-todo (:foreground ,orange-red))
      (org-done (:foreground ,fg))
      (org-headline-todo (:foreground ,orange-red))
      (org-headline-done (:foreground ,comment :strike-through t))
      (org-document-title (:foreground ,cyan))
      (org-document-info (:foreground ,cyan))
      (org-verbatim (:foreground ,pink))
      (org-code (:foreground ,light-olive))
      (org-block (:background "#121212"))
      (org-block-begin-line (:foreground ,comment))
      (org-block-end-line (:inherit 'org-block-begin-line :extend nil))
      (org-special-keyword (:foreground ,comment))

      ;; magit
      (magit-section-heading (:foreground ,orange :weight semi-bold))
      (magit-section-highlight (:background ,dark-blue))
      (magit-branch-local (:foreground ,yellow))
      (magit-branch-remote (:foreground ,cyan))
      (magit-tag (:foreground ,light-olive))
      (magit-diff-file-heading-highlight (:background ,dark-blue))
      (magit-diff-context-highlight (:background "gray15" :foreground "gray65"))
      (magit-diff-context (:foreground "gray40"))
      (magit-diff-hunk-heading (:background "gray12" :foreground "gray70"))
      (magit-diff-hunk-heading-highlight (:background "gray20" :foreground "gray80"))

      ;; manpages
      (Man-overstrike (:foreground ,cyan))

      ;; mu4e
      (mu4e-highlight-face (:weight semi-bold :foreground ,orange))

      ;; whitespace-mode
      (whitespace-space (:foreground ,whitespace-fg :background ,bg))
      (whitespace-tab (:foreground ,whitespace-fg :background ,bg))
      (whitespace-line (:foreground ,orange-red :background ,bg))

      ;; gnus and message-mode
      (gnus-header (:inherit default))

      ;; helm
      (helm-match (:foreground ,orange))
      (helm-source-header (:foreground ,bg :background ,fg))
      (helm-header (:foreground ,sea-green))
      (helm-selection (:foreground ,fg :background ,dark-blue))
      (helm-M-x-key (:foreground ,gold :background ,bg :box ,gold))
      (helm-ff-directory (:foreground ,light-olive :background ,bg))
      (helm-buffer-directory (:inherit helm-ff-directory))
      (helm-ff-dotted-directory (:foreground ,fg :background ,bg))
      (helm-ff-dotted-symlink-directory (:foreground ,dark-blue :background ,bg)))))

;; Set all the colors to their actual values.
(let ((bg "#040404")
      (fg "#0ac30a")
      (fg-dark "#078807")
      (yellow "gold2")
      (gold "DarkGoldenrod2")
      (cyan "cyan3")
      (string "olivedrab2")
      (pink "#cc59d2")
      ;; (orange "#dc7612")
      (orange "#ff9000")
      (light-olive "olivedrab2")
      (comment "#707370")
      (dark-blue "#01018a")
      (sea-green "medium sea green")
      (orange-red "orange red")
      (whitespace-fg "#151515"))
  (apply #'custom-theme-set-faces
         (cons 'naga (create-theme-colors))))

(provide-theme 'naga)

;;; naga-theme.el ends here
