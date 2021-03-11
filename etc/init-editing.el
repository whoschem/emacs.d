;;; init-editing.el --- Better editing experience  -*- lexical-binding: t  -*-

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

(setq-default
 bookmark-default-file (expand-file-name ".bookmarks" user-emacs-directory)
 indent-tabs-mode nil
 mouse-yank-at-point t
 set-mark-command-repeat-pop t
 truncate-lines nil
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'delete-selection-mode)
(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'global-prettify-symbols-mode)
(add-hook 'after-init-hook 'show-paren-mode)

(with-eval-after-load 'subword
  (delight 'subword-mode))

(use-package whitespace
  :custom (show-trailing-whitespace nil)
  :hook ((prog-mode text-mode) . (lambda () (setq-local show-trailing-whitespace t))))

(use-package whitespace-cleanup-mode
  :load-path "site-lisp/whitespace-cleanup-mode"
  :delight
  :hook (after-init . global-whitespace-cleanup-mode))

(use-package rime
  :load-path "lisp/rime"
  :bind ((:map rime-mode-map
               ("C-`" . 'rime-send-keybinding)
               ("C-'" . 'rime-force-enable))
         (:map rime-active-mode-map
               ("<tab>" . 'rime-inline-ascii)))
  :custom ((default-input-method 'rime)
           (rime-disable-predicates
            '(rime-predicate-prog-in-code-p
              rime-predicate-hydra-p
              rime-predicate-punctuation-line-begin-p))
           (rime-inline-predicates
            '(rime-predicate-space-after-cc-p
              rime-predicate-current-uppercase-letter-p))))

(use-package rainbow-delimiters
  :load-path "site-lisp/rainbow-delimiters"
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-escape-sequences
  :load-path "site-lisp/highlight-escape-sequences"
  :hook (after-init . hes-mode))

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Zap *up* to char is a handy pair for zap-to-char
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; vi-like mode
(defhydra +hydra-vi (:color amaranth)
  "vi"
  ("l" forward-char nil)
  ("h" backward-char nil)
  ("j" next-line nil)
  ("k" previous-line nil)
  ("v" scroll-up nil)
  ("V" scroll-down nil)
  ("w" forward-word nil)
  ("b" backward-word nil)
  ("{" backward-page nil)
  ("}" forward-page nil)
  ("g" keyboard-quit nil)
  ("m" set-mark-command "mark")
  ("a" move-beginning-of-line "beg")
  ("e" move-end-of-line "end")
  ("d" delete-region "del" :color blue)
  ("y" kill-ring-save "yank" :color blue)
  ("q" nil "quit"))

(global-set-key (kbd "C-z") '+hydra-vi/body)

;; `rectangle-mark-mode'
(defhydra +hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                      :color pink
                                      :post (deactivate-mark))
  "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _o_k        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _x_kill     _u_ndo
"
  ("h" rectangle-backward-char nil)
  ("l" rectangle-forward-char nil)
  ("k" rectangle-previous-line nil)
  ("j" rectangle-next-line nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("x" kill-rectangle nil)
  ("o" nil nil))

(global-set-key (kbd "C-x SPC") '+hydra-rectangle/body)

;; narrow commands
(defhydra +hydra-narrow (:color pink)
  "Narrow"
  ("d" narrow-to-defun "defun")
  ("g" goto-line-relative "goto line")
  ("n" narrow-to-region "region")
  ("p" narrow-to-page "page")
  ("w" widen "widen")
  ("q" nil "quit" :exit t))

(global-set-key (kbd "C-x n") '+hydra-narrow/body)

;; tabs management
(use-package tab-bar
  :bind* (([(meta shift left)] . tab-previous)
          ([(meta shift right)] . tab-next))
  :hook (after-init . tab-bar-mode)
  :config
  (defhydra +hydra-tabs (:color pink)
    "
^Switch^     ^Find^       ^Move^         ^Action^
^^^^^^^^------------------------------------------
_n_ext       _f_ile       _N_ext         _c_reate
_p_rev       _R_eadonly   _P_rev         _d_elete
_b_uffer      ^^^^                delete _o_thers
_s_earch        ^_T_ab mode^             _r_ename
"
    ("n" tab-bar-switch-to-next-tab nil)
    ("p" tab-bar-switch-to-prev-tab nil)
    ("b" switch-to-buffer-other-tab nil)
    ("s" tab-bar-switch-to-tab nil)
    ("f" find-file-other-tab nil)
    ("R" find-file-read-only-other-tab nil)
    ("N" (tab-bar-move-tab 1) nil)
    ("P" (tab-bar-move-tab -1) nil)
    ("c" tab-bar-new-tab nil)
    ("d" tab-bar-close-tab nil)
    ("o" tab-bar-close-other-tabs nil :exit t)
    ("r" tab-bar-rename-tab nil)
    ("t" other-tab-prefix "prefix")
    ("T" tab-bar-mode nil)
    ("q" nil "quit" :exit t))

  (global-set-key (kbd "C-x t") '+hydra-tabs/body))

(provide 'init-editing)

;;; init-editing.el ends here
