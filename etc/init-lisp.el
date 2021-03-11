;;; init-lisp.el --- Emacs Lisp and Lisp programming  -*- lexical-binding: t  -*-

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

;;; Emacs Lisp environment

(setq-default debugger-bury-or-kill 'kill)

(use-package eldoc
  :delight
  :hook (after-init . global-eldoc-mode))

;; Pretty-print the result of ELisp expressions
(use-package pp
  :bind (([remap eval-expression] . pp-eval-expression)
         ([remap eval-last-sexp] . pp-eval-last-sexp)))

;; Interactively stepping through the expansion of macros
(use-package macrostep
  :load-path "lisp/macrostep"
  :bind ((:map emacs-lisp-mode-map
               ("C-c x" . macrostep-expand))))

;; Jump between the *ielm* session and ELisp buffers
(defvar-local repl-original-buffer nil
  "Buffer from which we jumped to this REPL.")
(defvar repl-switch-function 'switch-to-buffer-other-window)

(defun switch-to-ielm ()
  "Jump to *ielm* session from the current buffer."
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (if (get-buffer "*ielm*")
        (funcall repl-switch-function "*ielm*")
      (ielm))
    (setq repl-original-buffer orig-buffer)))

(defun repl-switch-back ()
  "Switch back to the buffer from which we reached this REPL."
  (interactive)
  (if repl-original-buffer
      (funcall repl-switch-function repl-original-buffer)
    (error "No original buffer")))

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'switch-to-ielm))
(with-eval-after-load 'ielm
  (define-key ielm-map (kbd "C-c C-z") 'repl-switch-back))

;;; Lisp Programming

(defun enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defun set-up-hippie-expand-for-lisp ()
  "Locally set `hippie-expand' completion functions for Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

(defvar lisps-mode-hook
  '(enable-check-parens-on-save)
  "Hook run in all Lisp modes.")

(defun lisps-mode-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'lisps-mode-hook))

(use-package paredit
  :load-path "lisp/paredit"
  :delight " Par"
  :config (add-to-list 'lisps-mode-hook 'paredit-mode))

(use-package aggressive-indent
  :load-path "lisp/aggressive-indent"
  :config (add-to-list 'lisps-mode-hook 'aggressive-indent-mode))

(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook))
  (add-hook hook #'lisps-mode-setup))

(provide 'init-lisp)

;;; init-lisp.el ends here
