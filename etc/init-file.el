;;; init-file.el --- file and directory utility  -*- lexical-binding: t  -*-

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(use-package autorevert
  :delight auto-revert-mode
  :custom ((auto-revert-use-notify nil)
           (auto-revert-verbose nil)
           (global-auto-revert-non-file-buffers t))
  :hook (after-init . global-auto-revert-mode))

(use-package dired
  :bind ((:map dired-mode-map
               ([mouse-2] . dired-find-file)))
  :custom
  ((delete-by-moving-to-trash t)
   (dired-auto-revert-buffer t)
   (dired-dwim-target t)
   (dired-recursive-copies 'always)
   (dired-recursive-deletes 'top)))

(use-package wdired
  :after dired
  :bind ((:map dired-mode-map
               ("C-c C-q" . wdired-change-to-wdired-mode))))

(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)))

(provide 'init-file)

;;; init-file.el ends here
