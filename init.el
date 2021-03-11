 ;;; init.el --- Initialization starts here.  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Chen Yi

;; Author: Chen Yi

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

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "etc" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defconst system-unix-like-p
  (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin)))

(eval-when-compile
  (require 'use-package)
  (require 'hydra))

;; Bootstrap straight.el
(use-package straight
  :custom ((straight-vc-git-default-clone-depth 1))
  :config
  (straight--reset-caches)

  (if (straight--modifications 'check-on-save)
      (straight-live-modifications-mode 1)
    (straight-live-modifications-mode -1))

  (if straight-use-symlinks
      (straight-symlink-emulation-mode -1)
    (straight-symlink-emulation-mode 1))

  (if straight-enable-package-integration
      (straight-package-neutering-mode 1)
    (straight-package-neutering-mode -1))

  (if straight-enable-use-package-integration
      (straight-use-package-mode 1)
    (straight-use-package-mode -1)))

;; Bootstrap configuration
(require 'init-utils)
(require 'init-frame)
(require 'init-window)

(require 'init-session)
(require 'init-misc)

(require 'init-search)
(require 'init-buffer)
(require 'init-file)

(require 'init-selectrum)
(require 'init-editing)
(require 'init-programming)

(require 'init-gnus)
(require 'init-org)
(require 'init-lisp)

(use-package server
  :when system-unix-like-p
  :hook (after-init . (lambda () (unless (server-running-p)
                              (server-start)))))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

;;; init.el ends here
