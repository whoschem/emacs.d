;;; init-session.el --- session management  -*- lexical-binding: t  -*-

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

;; Tracking rencent opened files
(use-package recentf
  :custom ((recentf-exclude '("/tmp/" "/ssh:"))
           (recentf-max-saved-items 1000)
           (recentf-save-file (expand-file-name ".recentf" user-emacs-directory)))
  :hook (after-init . recentf-mode))

;; Restore histories and registers after saving
(use-package savehist
  :custom ((history-length 1000)
           (savehist-file (expand-file-name ".history" user-emacs-directory)))
  :hook (after-init . savehist-mode))

;; Automatically save place in files
(use-package saveplace
  :custom ((save-place-file (expand-file-name ".places" user-emacs-directory))
           (save-place-limit 1000))
  :hook (after-init . save-place-mode))

;; Save partial status of Emacs when killed
(use-package desktop
  :custom
  ((desktop-auto-save-timeout 600)
   (desktop-base-file-name ".desktop")
   (desktop-base-lock-name ".deeskop.lock")
   (desktop-globals-to-save
    '((comint-input-ring           50)
      (compile-history             30)
      desktop-missing-file-warning
      (dired-regexp-history        20)
      (extended-command-history    30)
      (face-name-history           20)
      (file-name-history           100)
      (grep-find-history           30)
      (grep-history                30)
      (minibuffer-history          50)
      (org-clock-history           50)
      (org-refile-history          50)
      (org-tags-history            50)
      (query-replace-history       60)
      (read-expression-history     60)
      (regexp-history              60)
      (regexp-search-ring          20)
      register-alist
      (search-ring                 20)
      (shell-command-history       50)
      tags-file-name
      tags-table-list)))

  :config (desktop-save-mode 1))

(provide 'init-session)

;;; init-session.el ends here
