;;; init-frame.el --- frame level display  -*- lexical-binding: t  -*-

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

;; Integrate with terminals
(autoload 'mwheel-install "mwheel")

(use-package xt-mouse
  :unless (display-graphic-p)
  :hook (after-init . (lambda () (xterm-mouse-mode 1))))

;; window and frame display
(setq-default blink-cursor-interval 0.4
              frame-resize-pixelwise t
              inhibit-startup-screen t
              use-dialog-box nil
              use-file-dialog nil
              window-resize-pixelwise t)

(menu-bar-mode -1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Easily adjust text scale for all buffers
(use-package default-text-scale
  :load-path "site-lisp/default-text-scale"
  :when (display-graphic-p)
  :hook (after-init . default-text-scale-mode))

;; Prefered themes
(use-package tomorrow-themes
  :load-path "site-lisp/tomorrow-themes"
  :init (setq custom-safe-themes t)
  :config (setq-default custom-enabled-themes '(tomorrow-night))
  :hook (after-init . (lambda () (dolist (theme custom-enabled-themes)
                              (unless (custom-theme-p theme)
                                (load-theme theme)))
                        (custom-set-variables `(custom-enabled-themes
                                                (quote ,custom-enabled-themes))))))

(provide 'init-frame)

;;; init-frame.el ends here
