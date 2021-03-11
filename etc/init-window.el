;;; init-window.el --- window manipulation  -*- lexical-binding: t  -*-

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

;; Navigate window layouts with "C-c <left>" and "C-c <right>"
(add-hook 'after-init-hook 'winner-mode)
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 U") 'winner-redo)

;; Directionally jump to the neighbor window
(unless (memq window-system '(nt w32))
  (windmove-default-keybindings 'control))

;; Quickly switch window
(use-package window-numbering
  :load-path "lisp/window-numbering"
  :hook (after-init . window-numbering-mode))

(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'toggle-delete-other-windows)

;; When splitting window, show (other-buffer) in the new window
(defun split-window-horizontally-with-other-buffer ()
  "Split horizontally this window and switch to the new window."
  (interactive)
  (split-window-horizontally)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-buffer))))

(global-set-key (kbd "C-x 2") 'split-window-horizontally-with-other-buffer)

(defun split-window-vertically-with-other-buffer ()
  "Split vertically this window and switch to the new window."
  (interactive)
  (split-window-vertically)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-buffer))))

(global-set-key (kbd "C-x 3") 'split-window-vertically-with-other-buffer)

(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x _") 'split-window-horizontally-instead)

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-vertically-instead)

(defun toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "C-c <down>") 'toggle-current-window-dedication)

;; window commands family
(defhydra +hydra-window-commands (:color pink)
  "
  ^Move^      ^Split^      ^Action^             ^Resize^
^^^^^^^^---------------------------------------------------------
   ^_k_^     _r_: right   _x_: delete current  _H_: shrink horiz
 _h_   _l_   _R_: horiz   _o_: delete others   _J_: enlarge
   ^_j_^     _b_: below   ^^                   _K_: shrink
^^^^         _B_: verti   ^^                   _L_: enlarge horiz
^_q_^: quit  ^^           ^^                   _n_: balance
"
  ("h" windmove-left nil)
  ("j" windmove-down nil)
  ("k" windmove-up nil)
  ("l" windmove-right nil)
  ("r" split-window-right nil)
  ("R" split-window-horizontally-instead nil)
  ("b" split-window-below nil)
  ("B" split-window-vertically-instead nil)
  ("H" shrink-window-horizontally nil)
  ("J" enlarge-window nil)
  ("K" shrink-window nil)
  ("L" enlarge-window-horizontally nil)
  ("n" balance-windows nil)
  ("o" delete-other-windows nil :exit t)
  ("x" delete-window nil)
  ("q" nil nil))

(global-set-key (kbd "C-c w") '+hydra-window-commands/body)

(provide 'init-window)

;;; init-window.el ends here
