;;; naga-dimmed-theme.el --- Dark color theme with green foreground color

;; Author: Johannes Maier <johannes.maier@mailbox.org>

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

;; The dimmed version of `naga', with more muted colors and fewer
;; different ones, giving it an overall even more green look.

;;; Code:

(deftheme naga-dimmed
  "Created 2024-03-09.")

;;;###autoload
(when (and load-file-name
           (boundp 'custom-theme-load-path))
  (let ((dir (file-name-as-directory
              (file-name-directory load-file-name))))
    (add-to-list 'custom-theme-load-path dir)
    (unless (featurep 'naga-base)
      (load-file (expand-file-name "naga-base.el" dir)))))

;; Set all the colors to their actual values.
(let ((bg "#040404")
      (bg-green "#041a04")
      (fg "#088e08")
      (fg-medium "#088e08")
      (fg-dark "#078807")
      (yellow "#b89c00")
      (gold "#eead0e")
      (cyan "#00afa0")
      (string "#83bc10")
      (purple "#825c84")
      (orange "#d99000")
      (comment "#707370")
      (comment-light "#909590")
      (comment-dark "#353535")
      (docstring "#4f710a")
      (grey "#aabaaa")
      (dark-blue "#01073a")
      (alt-green (if naga-theme-use-legacy-sea-green
                     "#0eb40e"
                   "#54aa0e"))
      (orange-red "#ff4500")
      (red "#ef1500")
      (whitespace-fg "#555f55")
      (block "#060606")
      (block-light "#252525")
      (amaranth "#e52b50"))
  (apply #'custom-theme-set-faces
         (cons 'naga-dimmed (naga--create-theme-colors))))

(provide-theme 'naga-dimmed)

;;; naga-dimmed-theme.el ends here

;; Local Variables:
;; fill-column: 70
;; End:
