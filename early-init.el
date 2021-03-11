;;; early-init.el --- Emacs 27+ pre-initialisation settings  -*- lexical-binding: t -*-

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

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.

;;; Code:

(setq package-enable-at-startup nil)

;; Defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-threshold (* 100 1024 1024))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

;;; early-init.el ends here
