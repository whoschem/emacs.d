;;; init-search.el --- search settings  -*- lexical-binding: t  -*-

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

(use-package isearch
  :bind ((:map isearch-mode-map
               ([remap isearch-delete-char] . isearch-del-char)
               ("C-c C-o" . isearch-occur)))
  :custom
  ((isearch-lazy-count t)
   (isearch-lazy-count-format "%S/%S ")))

;; grep and grep-like tools
(use-package grep
  :custom
  ((grep-highlight-matches t)
   (grep-scroll-output t)))

(use-package wgrep
  :after grep
  :bind (("C-c C-q" . wgrep-change-to-wgrep-mode)))

(provide 'init-search)

;;; init-search.el ends here
