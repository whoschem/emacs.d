;;; init-gnus.el --- GNU news reader  -*- lexical-binding: t  -*-

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

(use-package gnus-sum
  :config
  (defhydra +hydra-gnus-summary (:color blue)
    "
_F_: Forward (C-c C-f)              _s_: Show thread
_e_: Resend (S D e)                 _h_: Hide thread
_r_: Reply                          _n_: Refresh (/ N)
_R_: Reply with original            _!_: Mail -> disk
_w_: Reply all (S w)                _d_: Disk -> mail
_W_: Reply all with original (S W)  _c_: Read all
^^                                  _#_: Mark
_q_: quit                           _A_: Show Raw article
"
    ("F" gnus-summary-mail-forward nil)
    ("e" gnus-summary-resend-message-edit nil)
    ("r" gnus-summary-reply nil)
    ("R" gnus-summary-reply-with-original nil)
    ("w" gnus-summary-wide-reply nil)
    ("W" gnus-summary-wide-reply-with-original nil)
    ("s" gnus-summary-show-thread nil)
    ("h" gnus-summary-hide-thread nil)
    ("n" gnus-summary-insert-new-articles nil)
    ("!" gnus-summary-tick-article-forward nil)
    ("d" gnus-summary-put-mark-as-read-next nil)
    ("c" gnus-summary-catchup-and-exit nil)
    ("#" gnus-topic-mark-topic nil)
    ("A" gnus-summary-show-raw-article nil)
    ("q" nil nil))

  (define-key gnus-summary-mode-map "y" '+hydra-gnus-summary/body))

(use-package gnus-art
  :config
  (defhydra +hydra-gnus-article (:color blue)
    "
_o_: Save attachment
_F_: Forward
_R_: Reply with original
_W_: Reply all with original (S W)
"
    ("F" gnus-summary-mail-forward nil)
    ("R" gnus-article-reply-with-original nil)
    ("W" gnus-article-wide-reply-with-original nil)
    ("o" (lambda () (interactive) (let* ((file (gnus-mime-save-part)))
                               (when file (copy-yank-str file))))
     nil)
    ("q" nil nil))

  (define-key gnus-article-mode-map "y" '+hydra-gnus-article/body))

(provide 'init-gnus)

;;; init-gnus.el ends here
