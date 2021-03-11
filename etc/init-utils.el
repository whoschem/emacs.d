;;; init-utils.el --- runtime utility  -*- lexical-binding: t  -*-

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

(fset 'yes-or-no-p 'y-or-n-p)

;; Always load the newest bytecode or file
(setq load-prefer-newer t)

;; Configure the default locale
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; Emacs Lisp Package Archive
(use-package package
  :preface (setq package-enable-at-startup nil)
  :custom
  ((package-archives '(("gnu"   . "http://127.0.0.1/elpa/gnu/")
                       ("melpa" . "http://127.0.0.1/elpa/melpa/")))
   (package-user-dir (expand-file-name (format "elpa-%s.%s"
                                               emacs-major-version
                                               emacs-minor-version)
                                       user-emacs-directory)))
  :config (package-initialize))

;; Fork environment variables from shell
(use-package exec-path-from-shell
  :unless (display-graphic-p)
  :custom (exec-path-from-shell-variables
           '("PATH" "MANPATH" "LANG" "LC_CTYPE"
             "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
  :config
  (exec-path-from-shell-initialize))

;; Delight the mode line
(defun delight (mode &optional to-what)
  "Delight MODE name in mode line to TO-WHAT
If TO-WHAT is not provided, the mode will not show in the mode line."
  (let ((minor (assq mode minor-mode-alist)))
    (when minor
      (setcdr minor (list to-what)))))

(provide 'init-utils)

;;; init-utils.el ends here
