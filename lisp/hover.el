;;; hover.el --- Other Echo Area  -*- lexical-binding: t  -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides `hover-message' intended to be used in place of
;; `message' when semi-permanent hints are needed, in order to not
;; interfere with Echo Area.

;;; Code:

(require 'cl-lib)

(defgroup hover nil
  "The other echo area."
  :group 'minibuffer
  :prefix "hover-")

(defcustom hover-use-separator nil
  "Whether to draw a line between the Hover window and the Echo Area."
  :group 'hover
  :type 'boolean)

(defcustom hover-use-padding nil
  "Whether to use horizontal padding in the Hover window."
  :group 'hover
  :type 'boolean)

(defface hover-separator
  '((t (:inherit border :weight bold :extend t)))
  "Face used to draw line between the hover window and the echo area.
This is only used if option `hover-use-separator' is non-nil.
Only the background color is significant."
  :group 'hover)

(defvar hover-wnd nil
  "Holds the current Hover window.")

(defvar display-line-numbers)
(defvar display-fill-column-indicator)
(defvar tab-line-format)

(defvar hover-window-hook nil
  "Hook to run by `hover-window' when a new window is created.")

(defun hover-window ()
  "Ensure that Hover window is live and return it."
  (if (window-live-p hover-wnd)
      hover-wnd
    (let ((ori (selected-window))
          buf)
      (prog1 (setq hover-wnd
                   (select-window
                    (let ((ignore-window-parameters t))
                      (split-window
                       (frame-root-window) -1 'below))
                    'norecord))
        (if (setq buf (get-buffer " *Hover*"))
            (switch-to-buffer buf 'norecord)
          (switch-to-buffer " *Hover*" 'norecord)
          (fundamental-mode)
          (set-window-hscroll hover-wnd 0)
          (setq window-size-fixed t)
          (setq mode-line-format nil)
          (setq header-line-format nil)
          (setq tab-line-format nil)
          (setq cursor-type nil)
          (setq display-line-numbers nil)
          (setq display-fill-column-indicator nil)
          (set-window-dedicated-p hover-wnd t)
          (set-window-parameter hover-wnd 'no-other-window t)
          (run-hooks 'hover-window-hook))
        (select-window ori 'norecord)))))

(defvar hover-force-update nil
  "When non-nil, `hover-message' will refresh even for the same string.")

(defun hover--pad-to-center (str width)
  "Pad STR with spaces on the left to be centered to WIDTH."
  (let* ((strs (split-string str "\n"))
         (padding (make-string
                   (/ (- width (length (car strs))) 2)
                   ?\ )))
    (mapconcat (lambda (s) (concat padding s)) strs "\n")))

(defun hover-message (format-string &rest args)
  "Set Hover window contents to (`format' FORMAT-STRING ARGS)."
  (let* ((str (apply #'format format-string args))
         (n-lines (cl-count ?\n str))
         deactivate-mark)
    (with-selected-window (hover-window)
      (when hover-use-padding
        (setq str (hover--pad-to-center str (window-width))))
      (unless (and (string= (buffer-string) str)
                   (null hover-force-update))
        (delete-region (point-min) (point-max))
        (insert str)
        (when (and (window-system) hover-use-separator)
          (unless (looking-back "\n" nil)
            (insert "\n"))
          (insert
           (propertize "__" 'face 'hover-separator 'display '(space :height (1)))
           (propertize "\n" 'face 'hover-separator 'line-height t)))
        (set (make-local-variable 'window-min-height) n-lines)
        (setq truncate-lines (> n-lines 1))
        (let ((window-resize-pixelwise t)
              (window-size-fixed nil))
          (fit-window-to-buffer nil nil 1)))
      (goto-char (point-min)))))

(defun hover-delete-window ()
  "Delete Hover window and kill its buffer."
  (when (window-live-p hover-wnd)
    (let ((buf (window-buffer hover-wnd)))
      (delete-window hover-wnd)
      (kill-buffer buf))))

(provide 'hover)

;;; hover.el ends here
