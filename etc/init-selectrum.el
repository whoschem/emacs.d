;;; init-selectrum.el --- selectrum completion framework  -*- lexical-binding: t  -*-

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

(use-package selectrum
  :straight '(:host github :repo "raxod502/selectrum")
  :custom ((selectrum-fix-vertical-window-height t))
  :hook (after-init . selectrum-mode))

(use-package prescient
  :straight '(:host github :repo "raxod502/prescient.el"
                    :files ("prescient.el"))
  :hook (after-init . prescient-persist-mode))

(use-package selectrum-prescient
  :straight '(:host github :repo "raxod502/prescient.el"
                    :files ("selectrum-prescient.el"))
  :bind* (([remap execute-extended-command] . execute-extended-command))
  :hook (after-init . selectrum-prescient-mode))

(use-package marginalia
  :straight '(:host github :repo "minad/marginalia")
  :bind ((:map minibuffer-local-map
               ("M-A" . marginalia-cycle)))
  :custom ((marginalia-annotators
            '(marginalia-annotators-heavy marginalia-annotators-light nil)))
  :hook (after-init . marginalia-mode))

(provide 'init-selectrum)

;;; init-selectrum.el ends here
