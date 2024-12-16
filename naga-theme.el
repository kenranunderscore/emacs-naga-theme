;;; naga-theme.el --- Dark color theme with green foreground color

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

;; The default version of `naga'.

;;; Code:

(deftheme naga
  "Created 2021-09-26.")

;;;###autoload
(when (and load-file-name
           (boundp 'custom-theme-load-path))
  (let ((dir (file-name-as-directory
              (file-name-directory load-file-name))))
    (add-to-list 'custom-theme-load-path dir)
    (unless (featurep 'naga-base)
      (load-file (expand-file-name "naga-base.el" dir)))))

(let ((bg "#040404")
      (bg-green "#041a04")
      (fg "#0ac30a")
      (fg-medium "#089e08")
      (fg-dark "#078807")
      (yellow "#eec900")
      (gold "#eead0e")
      (cyan "#00bfb0")
      (string "#b3ee3a")
      (purple "#cc59d2")
      (orange "#ff9000")
      (comment "#707370")
      (comment-light "#909590")
      (comment-dark "#353535")
      (docstring "#698b22")
      (grey "#aabaaa")
      (dark-blue "#01018a")
      (alt-green (if naga-theme-use-legacy-sea-green
                     "#3cb371"
                   "#60c410"))
      (orange-red "#ff4500")
      (red "#ff1500")
      (whitespace-fg "#555f55")
      (block "#060606")
      (block-light "#252525")
      (amaranth "#e52b50"))
  (apply #'custom-theme-set-faces
         (cons 'naga (naga--create-theme-colors))))

(provide-theme 'naga)

;;; naga-theme.el ends here

;; Local Variables:
;; fill-column: 70
;; End:
