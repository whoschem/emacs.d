;;; init-programming.el --- PROgramming cluster -*- lexical-binding: t  -*-

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

;;;; compilation environment

(setq-default compilation-scroll-output t)

(defvar last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(with-eval-after-load 'compile
  (defun save-compilation-buffer (&rest _)
    "Save the compilation buffer to find it later."
    (setq last-compilation-buffer next-error-last-buffer))
  (advice-add 'compilation-start :after 'save-compilation-buffer)

  (defun find-prev-compilation (orig &optional edit-command)
    "Find the previous compilation buffer, if present, and recompile there."
    (if (and (null edit-command)
             (not (derived-mode-p 'compilation-mode))
             last-compilation-buffer
             (buffer-live-p (get-buffer last-compilation-buffer)))
        (with-current-buffer last-compilation-buffer
          (funcall orig edit-command))
      (funcall orig edit-command)))
  (advice-add 'recompile :around 'find-prev-compilation))

(defun shell-command-in-view-mode (start end command &optional output-buffer replace &rest other-args)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless (or output-buffer replace)
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))
(advice-add 'shell-command-on-region :after 'shell-command-in-view-mode)

(with-eval-after-load 'compile
  (require 'ansi-color)
  (defun colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'colourise-compilation-buffer))


;;; Various completion frameworks

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :custom ((hippie-expand-try-functions-list
            '(try-complete-file-name
              try-complete-file-name-partially
              try-expand-dabbrev
              try-expand-dabbrev-all-buffers
              try-expand-dabbrev-from-kill))))

;;;; Complete Anything

(use-package company
  :load-path "lisp/company"
  :delight
  :bind (("M-C-/" . company-complete)
         (:map company-mode-map
               ("M-/" . company-complete)
               ([completion-at-point] . company-complete)
               ([indent-for-tab-command] . company-indent-or-complete-common))
         (:map company-active-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("C-d" . company-show-doc-buffer)
               ("M-." . company-show-location)))
  :custom ((company-dabbrev-downcase nil)
           (company-dabbrev-ignore-case nil)
           (company-dabbrev-other-buffers 'all)
           (company-require-match nil)
           (company-selection-wrap-around t)
           (company-show-numbers t)
           (company-tooltip-align-annotations t))
  :hook (after-init . global-company-mode))

;;;; Template system

(use-package yasnippet
  :load-path "lisp/yasnippet"
  :hook ((text-mode prog-mode) . yas-minor-mode)
  :config (yas-reload-all))

(provide 'init-programming)

;;; init-programming.el ends here
