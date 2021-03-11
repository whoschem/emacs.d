;;; init-buffer.el --- buffer settings  -*- lexical-binding: t  -*-

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

(use-package ibuffer
  :bind* (("C-x C-b" . ibuffer))
  :custom
  ((ibuffer-show-empty-filter-groups nil)
   (ibuffer-filter-group-name-face 'font-lock-doc-face)
   (ibuffer-formats '((mark modified read-only locked " "
                            (name 20 20 :left :elide)
                            " "
                            (size-h 9 -1 :right)
                            " "
                            (mode 12 12 :left :elide)
                            " " filename-and-process)
                      (mark " "
                            (name 24 24)
                            " " filename))))
  :config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size)))

  (defun ibuffer-set-up-preferred-filters ()
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))

  (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters))

;; Configure uniquification of buffer names
(use-package uniquify
  :custom
  ((uniquify-after-kill-buffer-p t)
   (uniquify-buffer-name-style 'reverse)
   (uniquify-ignore-buffers-re "^\\*")
   (uniquify-separator " • ")))

(provide 'init-buffer)

;;; init-buffer.el ends here

