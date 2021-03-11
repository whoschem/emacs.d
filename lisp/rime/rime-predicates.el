;;; rime-predicates.el --- Predicates for rime to automatic input Chinese/English.  -*- lexical-binding: t -*-

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

;; With these predicates, You can continuously input mixed Chinese and English
;; text with punctuation, only using the Spacebar and the Enter key to assist,
;; without the extra switch key.

;;; Code:

(defun rime-predicate-after-alphabet-char-p ()
  "Whether the cursor is after a alphabet character.
Should be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (string-match-p "[a-zA-Z][0-9\x21-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]*$" string))))

(defun rime-predicate-after-ascii-char-p ()
  "Whether the cursor is after a ascii character.
Should be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (string-match-p "[a-zA-Z0-9\x21-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]$" string))))

(defun rime-predicate-prog-in-code-p ()
  "Whether the cursor is in code.
Should be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (derived-mode-p 'prog-mode 'conf-mode)
       (not (or (nth 3 (syntax-ppss))
                (nth 4 (syntax-ppss))))))

(defun rime-predicate-hydra-p ()
  "Whether a Hydra keymap is activated.
Should be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (featurep 'hydra)
       (bound-and-true-p hydra-curr-map)))

(defun rime-predicate-current-input-punctuation-p ()
  "Whether the current charactor entered is a punctuation.
Should be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and rime--current-input-key
       (or (and (<= #x21 rime--current-input-key) (<= rime--current-input-key #x2f))
           (and (<= #x3a rime--current-input-key) (<= rime--current-input-key #x40))
           (and (<= #x5b rime--current-input-key) (<= rime--current-input-key #x60))
           (and (<= #x7b rime--current-input-key) (<= rime--current-input-key #x7f)))))

(defun rime-predicate-punctuation-after-space-cc-p ()
  "Whether input a punctuation after a Chinese charactor with whitespace.
Should be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (rime-predicate-current-input-punctuation-p)
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (string-match-p "\\cc +$" string))))

(defun rime-predicate-punctuation-after-ascii-p ()
  "Whether input a punctuation after a ascii charactor with whitespace.
Should be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (rime-predicate-current-input-punctuation-p)
       (rime-predicate-after-ascii-char-p)))

(defun rime-predicate-punctuation-line-begin-p ()
  "Enter half-width punctuation at the beginning of the line.

Detect whether the current cursor is at the beginning of a
line and the character last inputted is symbol.

Should be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (<= (point) (save-excursion (back-to-indentation) (point)))
       (rime-predicate-current-input-punctuation-p)))

(defun rime-predicate-auto-english-p ()
  "Auto switch Chinese/English input state.

After activating this probe function, use the following rules
to automatically switch between Chinese and English input:

  1. When the current character is an English
     character (excluding spaces), enter the next character as an
     English character.
  2. When the current character is a Chinese character or the
     input character is a beginning character, the input character is
     a Chinese character.
  3. With a single space as the boundary, automatically switch
     between Chinese and English characters.

That is, a sentence of the form \"我使用 emacs 编辑此函数\"
automatically switches between Chinese and English input methods.

Should be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (if (string-match-p " +$" string)
             (string-match-p "\\cc +$" string)
           (not (string-match-p "\\cc$" string))))))

(defun rime-predicate-space-after-ascii-p ()
  "If cursor is after a whitespace which follow a ascii character."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (and (string-match-p " +$" string)
              (not (string-match-p "\\cc +$" string))))))

(defun rime-predicate-space-after-cc-p ()
  "Whether cursor is after a whitespace which follow a non-ascii character."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (string-match-p "\\cc +$" string))))

(defun rime-predicate-current-uppercase-letter-p ()
  "Whether the current charactor entered is a uppercase letter.
Should be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and rime--current-input-key
       (>= rime--current-input-key ?A)
       (<= rime--current-input-key ?Z)))

(defun rime-predicate-org-latex-mode-p ()
  "Whether cursor is inside an org-mode's LaTeX fragment, macro or its arguments."
  (and (derived-mode-p  'org-mode)
       (or (org-inside-LaTeX-fragment-p)
           (org-inside-latex-macro-p))))

(defun rime-predicate-org-in-src-block-p ()
  "Whether point is in an org-mode's code source block."
  (and (derived-mode-p 'org-mode)
       (org-in-src-block-p)))

(defun rime-predicate-in-code-string-p ()
  "Whether point is in the code string(not comment string)."
  (eq (plist-get (text-properties-at (point)) 'face) 'font-lock-string-face))

(defun rime-predicate-in-code-string-after-ascii-p ()
  "Whether point is in the code string and after a ascii character."
  (and
   (eq (plist-get (text-properties-at (point)) 'face) 'font-lock-string-face)
   (rime-predicate-after-ascii-char-p)))

(defun rime-predicate-tex-math-or-command-p ()
  "Whether point is inside a (La)TeX math environment, or a (La)TeX command.

Return t if the buffer is in `tex-mode' and one of the following three cases occurs:

  1. The point is inside a (La)TeX math environment;
  2. The current character is `$' or `\\', either at the beginning of a line, or after an ascii/space.
  3. The string before the point is of the form a (La)TeX command.
     If the command have a parameter, it is closed by `}' or `%'.
     If not, it is closed by a space or `%'.

Should be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (derived-mode-p 'tex-mode)
       (or (and (featurep 'tex-site)
                (texmathp))
           (and rime--current-input-key
                (or (= #x24 rime--current-input-key)
                    (= #x5c rime--current-input-key))
                (or (= (point) (line-beginning-position))
                    (= #x20 (char-before))
                    (rime-predicate-after-ascii-char-p)))
           (and (> (point) (save-excursion (back-to-indentation) (point)))
                (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
                  (or (string-match-p "[\x5c][\x21-\x24\x26-\x7e]*$" string)
                      (string-match-p "[\x5c][a-zA-Z\x23\x40]+[\x7b][^\x7d\x25]*$" string)))))))

(provide 'rime-predicates)

;;; rime-predicates.el ends here
