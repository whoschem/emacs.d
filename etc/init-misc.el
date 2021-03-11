;;; init-misc.el --- Miscellaneous configuration  -*- lexical-binding: t  -*-

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

;;; Misc settings

(setq-default
 auto-save-default nil
 case-fold-search t
 column-number-mode t
 confirm-kill-emacs 'y-or-n-p
 create-lockfiles nil
 display-line-numbers-width 3
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs loves you!\n\n")
 make-backup-files nil
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 tooltip-delay 1.0)

(add-hook 'after-init-hook 'so-long-enable)
(add-hook 'after-init-hook 'transient-mark-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun disable-features-during-macro-call (orig &rest args)
  "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
  (let (post-command-hook
        font-lock-mode
        (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
    (apply orig args)))
(advice-add 'kmacro-call-macro :around 'disable-features-during-macro-call)

(use-package undo-tree
  :delight
  :bind (("C-x /" . undo-tree-undo)
         ("C-x ?" . undo-tree-redo)
         ("C-x u" . undo-tree-visualizer))
  :hook (after-init . global-undo-tree-mode))

;; Spell checking
(use-package flyspell
  :when (and (require 'ispell)
             (executable-find ispell-program-name))
  :delight
  :hook (text-mode . flyspell-mode))

;; Quickly jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)

;; Display the key by currently entered incomplete command
(use-package which-key
  :load-path "lisp/which-key"
  :delight
  :custom ((which-key-idle-delay 1.5)
           (which-key-popup-type 'side-window)
           (which-key-side-window-location 'bottom))
  :hook (after-init . which-key-mode))

;; apropos family
(defhydra +hydra-apropos (:color blue
                                 :hint nil)
  "
_a_propos        _c_ommand
_d_ocumentation  _l_ibrary
_v_ariable       _u_ser-option
^ ^          valu_e_"
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value))

(global-set-key (kbd "C-c h") '+hydra-apropos/body)

;; Rarely used modes
(defhydra +hydra-rare-mode (:color pink)
  "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
_t_ truncate-lines:    %`truncate-lines
_w_ whitespace-mode:   %`whitespace-mode

"
  ("a" abbrev-mode nil)
  ("d" toggle-debug-on-error nil)
  ("f" auto-fill-mode nil)
  ("t" toggle-truncate-lines nil)
  ("w" whitespace-mode nil)
  ("q" nil "quit"))

(global-set-key (kbd "C-c C-v") '+hydra-rare-mode/body)

(provide 'init-misc)

;;; init-misc.el ends here
