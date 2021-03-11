;;; paredit.el --- minor mode for editing parentheses.  -*- lexical-binding: t -*-

;; Copyright (C) 2005-2019  Taylor R. Campbell

;; Author: Taylor R. Campbell <campbell+paredit@mumble.net>
;; Created: 2005-07-31
;; Keywords: lisp

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

;; The paredit minor mode, Paredit mode, binds common character keys,
;; such as `(', `)', `"', and `\', to commands that carefully insert
;; S-expression structures in the buffer:
;;
;;   ( inserts `()', leaving the point in the middle;
;;   ) moves the point over the next closing delimiter;
;;   " inserts `""' if outside a string, or inserts an escaped
;;      double-quote if in the middle of a string, or moves over the
;;      closing double-quote if at the end of a string; and
;;   \ prompts for the character to escape, to avoid inserting lone
;;      backslashes that may break structure.
;;
;; In comments, these keys insert themselves.  If necessary, you can
;; insert these characters literally outside comments by pressing
;; `C-q' before these keys, in case a mistake has broken the
;; structure.
;;
;; These key bindings are designed so that when typing new code in
;; paredit mode, you can generally type exactly the same sequence of
;; keys you would have typed without paredit mode.
;;
;; Paredit mode also binds common editing keys, such as `DEL', `C-d',
;; and `C-k', to commands that respect S-expression structures in the
;; buffer:
;;
;;   DEL deletes the previous character, unless it is a delimiter: DEL
;;        will move the point backward over a closing delimiter, and
;;        will delete a delimiter pair together if between an open and
;;        closing delimiter;
;;
;;   C-d deletes the next character in much the same manner; and
;;
;;   C-k kills all S-expressions that begin anywhere between the point
;;        and the end of the line or the closing delimiter of the
;;        enclosing list, whichever is first.
;;
;; If necessary, you can delete a character, kill a line, &c.,
;; irrespective of S-expression structure, by pressing `C-u' before
;; these keys, in case a mistake has broken the structure.
;;
;; Finally, paredit mode binds some keys to complex S-expression
;; editing operations.  For example, `C-<right>' makes the enclosing
;; list promote up an S-expression to its right.

;; Some paredit commands automatically reindent code.  When they do,
;; they try to indent as locally as possible, to avoid interfering
;; with any indentation you might have manually written.  Only the
;; advanced S-expression manipulation commands automatically reindent,
;; and only the forms that they immediately operated upon (and their
;; subforms).
;;
;; This code is written for clarity, not efficiency.  It frequently
;; walks over S-expressions redundantly.  If you have problems with
;; the time it takes to execute some of the commands, let me know.
;;
;; This assumes Unix-style LF line endings.

;;; Code:

;;; Error handler

(defvar paredit-sexp-error-type
  (with-temp-buffer
    (insert "(")
    (condition-case condition
        (backward-sexp)
      (error (if (eq (car condition) 'error)
                 (warn "%s%s%s"
                       "Paredit is unable to discriminate"
                       " S-expression parse errors from"
                       " other errors"))
             (car condition)))))

(defmacro paredit-handle-sexp-errors (body &rest handler)
  (declare (indent 1))
  `(condition-case ()
       ,body
     (,paredit-sexp-error-type ,@handler)))

(defmacro paredit-ignore-sexp-errors (&rest body)
  (declare (indent 0))
  `(paredit-handle-sexp-errors (progn ,@body)
     nil))

(defmacro paredit-preserving-column (&rest body)
  "Evaluate BODY and restore point to former column, relative to code.
Assume BODY will change only indentation.

If point was on code, move with the code.
If point was on indentation, stay in indentation."
  (declare (indent 0))
  (let ((column (make-symbol "column"))
        (indentation (make-symbol "indentation")))
    `(let ((,column (current-column))
           (,indentation (paredit-current-indentation)))
       (let ((value (progn ,@body)))
         (paredit-restore-column ,column ,indentation)
         value))))


;;; Command Entity

;;;; Basic insertion

;;;;; Delimiter insertion

(defun paredit-conc-name (&rest strings)
  (intern (apply 'concat strings)))

(defmacro paredit-define-pair (open close name)
  (declare (indent defun))
  `(progn
     (defun ,(paredit-conc-name "paredit-open-" name) (&optional n)
       ,(concat "Insert a balanced " name " pair.

With a prefix argument N, put the closing " name " after N
S-expressions forward.

If the region is active, `transient-mark-mode' is enabled, and the
region's start and end fall in the same parenthesis depth, insert a
" name " pair around the region.

If in a string or a comment, insert a single " name ".
If in a character literal, do nothing.  This prevents changing what was
in the character literal to a meaningful delimiter unintentionally.")
       (interactive "P")
       (cond ((or (paredit-in-string-p)
                  (paredit-in-comment-p))
              (insert ,open))
             ((not (paredit-in-char-p))
              (paredit-insert-pair n ,open ,close 'goto-char)
              (save-excursion (backward-up-list) (indent-sexp)))))
     (defun ,(paredit-conc-name "paredit-close-" name) ()
       ,(concat "Move past one closing delimiter and reindent.
\(Agnostic to the specific closing delimiter.)

If in a string or comment, insert a single closing " name ".
If in a character literal, do nothing.  This prevents changing what was
in the character literal to a meaningful delimiter unintentionally.")
       (interactive)
       (paredit-move-past-close ,close))
     (defun ,(paredit-conc-name "paredit-close-" name "-and-newline") ()
       ,(concat "Move past one closing delimiter, add a newline,"
                " and reindent.

If there was a margin comment after the closing delimiter, preserve it
on the same line.")
       (interactive)
       (paredit-move-past-close-and-newline ,close))
     (defun ,(paredit-conc-name "paredit-wrap-" name)
         (&optional argument)
       ,(concat "Wrap the following S-expression.
See `paredit-wrap-sexp' for more details.")
       (interactive "P")
       (paredit-wrap-sexp argument ,open ,close))
     (add-to-list 'paredit-wrap-commands
                  ',(paredit-conc-name "paredit-wrap-" name))))

(defvar paredit-wrap-commands '(paredit-wrap-sexp)
  "List of paredit commands that wrap S-expressions.
Used by `paredit-yank-pop'; for internal paredit use only.")

(paredit-define-pair ?\( ?\) "round")
(paredit-define-pair ?\[ ?\] "square")
(paredit-define-pair ?\{ ?\} "curly")
(paredit-define-pair ?\< ?\> "angled")

(defun paredit-move-past-close (close)
  (paredit-move-past-close-and
    close (lambda ()
            (paredit-blink-paren-match nil))))

(defun paredit-move-past-close-and-newline (close)
  (paredit-move-past-close-and
    close (lambda ()
            (let ((comment.point (paredit-find-comment-on-line)))
              (newline)
              (if comment.point
                  (save-excursion
                    (forward-line -1)
                    (end-of-line)
                    (indent-to (cdr comment.point))
                    (insert (car comment.point)))))
            (lisp-indent-line)
            (paredit-ignore-sexp-errors (indent-sexp))
            (paredit-blink-paren-match t))))

(defun paredit-move-past-close-and (close if-moved)
  (declare (indent defun))
  (if (or (paredit-in-string-p)
          (paredit-in-comment-p))
      (insert close)
    (if (paredit-in-char-p) (forward-char))
    (paredit-move-past-close-and-reindent close)
    (funcall if-moved)))

(defun paredit-find-comment-on-line ()
  "Find a margin comment on the current line.

Return nil if there is no such comment or if there is anything but
whitespace until such a comment.

If such a comment exists, delete the comment (including all leading
whitespace) and return a cons whose car is the comment as a string
and whose cdr is the point of the comment's initial semicolon,
relative to the start of the line."
  (save-excursion
    (paredit-skip-whitespace t (point-at-eol))
    (and (eq ?\; (char-after))
         (not (eq ?\; (char-after (1+ (point)))))
         (not (or (paredit-in-string-p)
                  (paredit-in-char-p)))
         (let* ((start                 ; Move to before the semicolon.
                 (progn (backward-char) (point)))
                (comment
                 (buffer-substring start (point-at-eol))))
           (paredit-skip-whitespace nil (point-at-bol))
           (delete-region (point) (point-at-eol))
           (cons comment (- start (point-at-bol)))))))

(defun paredit-insert-pair (n open close forward)
  (let* ((regionp
          (and (paredit-region-active-p)
               (paredit-region-safe-for-insert-p)))
         (end
          (and regionp
               (not n)
               (prog1 (region-end) (goto-char (region-beginning))))))
    (let ((spacep (paredit-space-for-delimiter-p nil open)))
      (if spacep (insert " "))
      (insert open)
      (save-excursion
        ;; Move past the desired region.
        (cond (n
               (funcall forward
                        (paredit-scan-sexps-inside (point)
                                                   (prefix-numeric-value n))))
              (regionp
               (funcall forward (+ end (if spacep 2 1)))))
        ;; The string case can happen if we are inserting string
        ;; delimiters.  The comment case may happen by moving to the
        ;; end of a buffer that has a comment with no trailing newline.
        (if (and (not (paredit-in-string-p))
                 (paredit-in-comment-p))
            (newline))
        (insert close)
        (if (paredit-space-for-delimiter-p t close)
            (insert " "))))))

(defun paredit-scan-sexps-inside (point n)
  (save-excursion
    (goto-char point)
    (let ((direction (if (< 0 n) +1 -1))
          (magnitude (abs n))
          (count 0))
      (catch 'exit
        (while (< count magnitude)
          (let ((p
                 (paredit-handle-sexp-errors (scan-sexps (point) direction)
                   nil)))
            (if (not p) (throw 'exit nil))
            (goto-char p))
          (setq count (+ count 1)))))
    (point)))

(defun paredit-region-safe-for-insert-p ()
  (save-excursion
    (let ((beginning (region-beginning))
          (end (region-end)))
      (goto-char beginning)
      (let* ((beginning-state (paredit-current-parse-state))
             (end-state
              (parse-partial-sexp beginning end nil nil beginning-state)))
        (and (=  (nth 0 beginning-state) ; 0. depth in parens
                 (nth 0 end-state))
             (eq (nth 3 beginning-state) ; 3. non-nil if inside a
                 (nth 3 end-state))      ;    string
             (eq (nth 4 beginning-state) ; 4. comment status, yada
                 (nth 4 end-state))
             (eq (nth 5 beginning-state) ; 5. t if following char
                 (nth 5 end-state))))))) ;    quote

(defvar paredit-space-for-delimiter-predicates nil
  "List of predicates for whether to put space by delimiter at point.

Each predicate is a function that is is applied to two arguments, ENDP
and DELIMITER, and that returns a boolean saying whether to put a
space next to the delimiter -- before/after the delimiter if ENDP is
false/true, respectively.

If any predicate returns false, no space is inserted: every predicate
has veto power.

Each predicate may assume that the point is not at the beginning/end of
the buffer, and that the point is preceded/followed by a word
constituent, symbol constituent, string quote, or delimiter matching
DELIMITER, if ENDP is false/true, respectively.

Each predicate should examine only text before/after the point if ENDP is
false/true, respectively.")

(defun paredit-space-for-delimiter-p (endp delimiter)
  "If at the buffer limit, don't insert a space.  If there is a word,
symbol, other quote, or non-matching parenthesis delimiter (i.e., a
close when want an open the string or an open when we want to
close the string), insert a space."
  (and (not (if endp (eobp) (bobp)))
       (memq (char-syntax (if endp (char-after) (char-before)))
             (list ?w ?_ ?\"
                   (let ((matching (matching-paren delimiter)))
                     (and matching (char-syntax matching)))
                   (and (not endp)
                        (eq ?\" (char-syntax delimiter))
                        ?\) )))
       (catch 'exit
         (dolist (predicate paredit-space-for-delimiter-predicates)
           (if (not (funcall predicate endp delimiter))
               (throw 'exit nil)))
         t)))

(defun paredit-move-past-close-and-reindent (close)
  (let ((open (paredit-missing-close)))
    (if open
        (if (eq close (matching-paren open))
            (save-excursion
              (message "Missing closing delimiter: %c" close)
              (insert close))
          (error "Mismatched missing closing delimiter: %c ... %c"
                 open close))))
  (up-list)
  (if (catch 'return                  ; Return t if should delete
        (while t                      ; leading spaces and NIL if not.
          (save-excursion
            (let ((before-paren (1- (point))))
              (back-to-indentation)
              (cond ((not (eq (point) before-paren))
                     ;; Can't call `paredit-delete-leading-whitespace'
                     ;; here -- we must return from `save-excursion'
                     ;; first.
                     (throw 'return t))
                    ((save-excursion (forward-line -1)
                                     (end-of-line)
                                     (paredit-in-comment-p))
                     ;; Moving the closing delimiter any further
                     ;; would put it into a comment, so we just
                     ;; indent the closing delimiter where it is and
                     ;; abort the loop, telling its continuation that
                     ;; no leading whitespace should be deleted.
                     (lisp-indent-line)
                     (throw 'return nil))
                    (t (delete-indentation)))))))
      (paredit-delete-leading-whitespace)))

(defun paredit-missing-close ()
  (save-excursion
    (paredit-handle-sexp-errors (backward-up-list)
      (error "Not inside a list."))
    (let ((open (char-after)))
      (paredit-handle-sexp-errors (progn (forward-sexp) nil)
        open))))

(defun paredit-delete-leading-whitespace ()
  ;; This assumes that we're on the closing delimiter already.
  (save-excursion
    (backward-char)
    (while (let ((syn (char-syntax (char-before))))
             (and (or (eq syn ?\ ) (eq syn ?-)) ; whitespace syntax
                  ;; The above line is a perfect example of why the
                  ;; following test is necessary.
                  (not (paredit-in-char-p (1- (point))))))
      (delete-char -1))))

(defun paredit-blink-paren-match (another-line-p)
  (if (and blink-matching-paren
           (or (not show-paren-mode) another-line-p))
      (paredit-ignore-sexp-errors
        (save-excursion
          (backward-sexp)
          (forward-sexp)
          ;; `show-paren-mode' inhibits any blinking, so we disable it
          ;; locally here.
          (let ((show-paren-mode nil))
            (blink-matching-open))))))

(defun paredit-doublequote (&optional n)
  "Insert a pair of double-quotes.

With a prefix argument N, wrap the following N S-expressions in
double-quotes, escaping intermediate characters if necessary.

If the region is active, `transient-mark-mode' is enabled, and the
region's start and end fall in the same parenthesis depth, insert a
pair of double-quotes around the region, again escaping intermediate
characters if necessary.

Inside a comment, insert a literal double-quote.
At the end of a string, move past the closing double-quote.
In the middle of a string, insert a backslash-escaped double-quote.

If in a character literal, do nothing.  This prevents accidentally
changing a what was in the character literal to become a meaningful
delimiter unintentionally."
  (interactive "P")
  (cond ((paredit-in-string-p)
         (if (eq (point) (- (paredit-enclosing-string-end) 1))
             (forward-char)        ; Just move past the closing quote.
           ;; Don't split a \x into an escaped backslash and a string end.
           (if (paredit-in-string-escape-p) (forward-char))
           (insert ?\\ ?\" )))
        ((paredit-in-comment-p)
         (insert ?\" ))
        ((not (paredit-in-char-p))
         (paredit-insert-pair n ?\" ?\" 'paredit-forward-for-quote))))

(defun paredit-meta-doublequote (&optional n)
  "Move to the end of the string.
If not in a string, act as `paredit-doublequote'; if not prefix argument
s specified and the region is not active or `transient-mark-mode' is
isabled, the default is to wrap one S-expression, however, not zero."
  (interactive "P")
  (if (not (paredit-in-string-p))
      (paredit-doublequote (or n (and (not (paredit-region-active-p)) 1)))
    (goto-char (paredit-enclosing-string-end))))

(defun paredit-meta-doublequote-and-newline (&optional n)
  "Move to the end of the string, insert a newline, and indent.
If not in a string, act as `paredit-doublequote'; if not prefix argument
is specified and the region is not active or `transient-mark-mode' is
disabled, the default is to wrap one S-expression, however, not zero."
  (interactive "P")
  (if (not (paredit-in-string-p))
      (paredit-doublequote (or n (and (not (paredit-region-active-p)) 1)))
    (progn (goto-char (paredit-enclosing-string-end))
           (newline)
           (lisp-indent-line)
           (paredit-ignore-sexp-errors (indent-sexp)))))

(defun paredit-forward-for-quote (end)
  (let ((state (paredit-current-parse-state)))
    (while (< (point) end)
      (let ((new-state (parse-partial-sexp (point) (1+ (point))
                                           nil nil state)))
        (if (paredit-in-string-p new-state)
            (if (not (paredit-in-string-escape-p))
                (setq state new-state)
              ;; Escape character: turn it into an escaped escape
              ;; character by appending another backslash.
              (insert ?\\ )
              ;; Now the point is after both escapes, and we want to
              ;; rescan from before the first one to after the second
              ;; one.
              (setq state
                    (parse-partial-sexp (- (point) 2) (point)
                                        nil nil state))
              ;; Advance the end point, since we just inserted a new
              ;; character.
              (setq end (1+ end)))
          ;; String: escape by inserting a backslash before the quote.
          (backward-char)
          (insert ?\\ )
          ;; The point is now between the escape and the quote, and we
          ;; want to rescan from before the escape to after the quote.
          (setq state
                (parse-partial-sexp (1- (point)) (1+ (point))
                                    nil nil state))
          ;; Advance the end point for the same reason as above.
          (setq end (1+ end)))))))

;;;;; Escape insertion

(defun paredit-backslash ()
  "Insert a backslash followed by a character to escape."
  (interactive)
  (cond ((paredit-in-string-p) (paredit-backslash-interactive))
        ((paredit-in-comment-p) (insert ?\\))
        ((paredit-in-char-p) (forward-char) (paredit-backslash-interactive))
        (t (paredit-backslash-interactive))))

(defun paredit-backslash-interactive ()
  "Read a character to insert after the backslash.
If anything goes wrong -- the user hits delete (entering the
rubout `character'), aborts with `C-g', or enters non-character
input -- then delete the backslash to avoid a dangling escape."
  (insert ?\\ )

  (let ((delete-p t))
    (unwind-protect
        (let ((char (read-char "Character to escape: " t)))
          (if (not (eq char ?\^?))
              (progn (message "Character to escape: %c" char)
                     (insert char)
                     (setq delete-p nil))))
      (if delete-p
          (progn (message "Deleting escape")
                 (delete-char -1))))))

(defun paredit-newline ()
  "Insert a newline and indent it.
This is like `newline-and-indent', but it not only indents the line
that the point is on but also the S-expression following the point,
if there is one.

If on an escaped character, move forward one character first.
If in a string, just insert a literal newline.
If in a comment and if followed by invalid structure, call
`indent-new-comment-line' to keep the invalid structure in a
comment."
  (interactive)
  (cond ((paredit-in-string-p)
         (newline))
        ((paredit-in-comment-p)
         (if (paredit-region-ok-p (point) (point-at-eol))
             (progn (newline-and-indent)
                    (paredit-ignore-sexp-errors (indent-sexp)))
           (indent-new-comment-line)))
        (t
         (if (paredit-in-char-p)
             (forward-char))
         (newline-and-indent)
         ;; Indent the following S-expression, but don't signal an
         ;; error if there's only a closing delimiter after the point.
         (paredit-ignore-sexp-errors (indent-sexp)))))

(defun paredit-reindent-defun (&optional argument)
  "Reindent the definition that the point is on.
If the point is in a string or a comment, fill the paragraph instead,
and with a prefix argument, justify as well."
  (interactive "P")
  (if (or (paredit-in-string-p)
          (paredit-in-comment-p))
      (lisp-fill-paragraph argument)
    (paredit-preserving-column
      (save-excursion
        (end-of-defun)
        (beginning-of-defun)
        (indent-sexp)))))

;;;;; Comment insertion

(defun paredit-semicolon (&optional n)
  "Insert a semicolon.
With a prefix argument N, insert N semicolons.

If in a string, do just that and nothing else.
If in a character literal, move to the beginning of the character
literal before inserting the semicolon.

If the enclosing list ends on the line after the point, break the line
after the last S-expression following the point.
If a list begins on the line after the point but ends on a different
line, break the line after the last S-expression following the point
before the list."
  (interactive "p")
  (if (or (paredit-in-string-p) (paredit-in-comment-p))
      (insert (make-string (or n 1) ?\; ))
    (if (paredit-in-char-p)
        (backward-char 2))
    (let ((line-break-point (paredit-semicolon-find-line-break-point)))
      (if line-break-point
          (paredit-semicolon-with-line-break line-break-point (or n 1))
        (insert (make-string (or n 1) ?\; ))))))

(defun paredit-semicolon-find-line-break-point ()
  (and (not (eolp))                   ;Implies (not (eobp)).
       (let ((eol (point-at-eol)))
         (save-excursion
           (catch 'exit
             (while t
               (let ((line-break-point (point)))
                 (cond ((paredit-handle-sexp-errors (progn (forward-sexp) t)
                          nil)
                        ;; Successfully advanced by an S-expression.
                        ;; If that S-expression started on this line
                        ;; and ended on another one, break here.
                        (cond ((not (eq eol (point-at-eol)))
                               (throw 'exit
                                      (and (save-excursion
                                             (backward-sexp)
                                             (eq eol (point-at-eol)))
                                           line-break-point)))
                              ((eobp)
                               (throw 'exit nil))))
                       ((save-excursion
                          (paredit-skip-whitespace t (point-at-eol))
                          (or (eolp) (eobp) (eq (char-after) ?\;)))
                        ;; Can't move further, but there's no closing
                        ;; delimiter we're about to clobber -- either
                        ;; it's on the next line or we're at the end of
                        ;; the buffer.  Don't break the line.
                        (throw 'exit nil))
                       (t
                        ;; Can't move because we hit a delimiter at the
                        ;; end of this line.  Break here.
                        (throw 'exit line-break-point))))))))))

(defun paredit-semicolon-with-line-break (line-break-point n)
  (let ((line-break-marker (make-marker)))
    (set-marker line-break-marker line-break-point)
    (set-marker-insertion-type line-break-marker t)
    (insert (make-string (or n 1) ?\; ))
    (save-excursion
      (goto-char line-break-marker)
      (set-marker line-break-marker nil)
      (newline)
      (lisp-indent-line)
      ;; This step is redundant if we are inside a list, but even if we
      ;; are at the top level, we want at least to indent whatever we
      ;; bumped off the line.
      (paredit-ignore-sexp-errors (indent-sexp))
      (paredit-indent-sexps))))

(autoload 'comment-normalize-vars "newcomment")
(autoload 'comment-search-forward "newcomment")

(defun paredit-initialize-comment-dwim ()
  (require 'newcomment)
  (defalias 'paredit-initialize-comment-dwim 'comment-normalize-vars)
  (comment-normalize-vars))

(defvar paredit-comment-prefix-toplevel ";;; "
  "String of prefix for top-level comments aligned at the left margin.")
(defvar paredit-comment-prefix-code ";; "
  "String of prefix for comments indented at the same depth as code.")
(defvar paredit-comment-prefix-margin ";"
  "String of prefix for comments on the same line as code in the margin.")

(defun paredit-comment-dwim (&optional argument)
  "Call the Lisp comment command you want (Do What I Mean).
This is like `comment-dwim', but it is specialized for Lisp editing.

If transient mark mode is enabled and the mark is active, comment or
uncomment the selected region, depending on whether it was entirely
commented not not already.
If there is already a comment on the current line, with no prefix
argument, indent to that comment; with a prefix argument, kill that
comment.
Otherwise, insert a comment appropriate for the context and ensure that
any code following the comment is moved to the next line.

At the top level, where indentation is calculated to be at column 0,
insert a triple-semicolon comment; within code, where the indentation
is calculated to be non-zero, and on the line there is either no code
at all or code after the point, insert a double-semicolon comment;
and if the point is after all code on the line, insert a single-
semicolon margin comment at `comment-column'."
  (interactive "*P")
  (paredit-initialize-comment-dwim)
  (cond ((paredit-region-active-p)
         (comment-or-uncomment-region (region-beginning)
                                      (region-end)
                                      argument))
        ((paredit-comment-on-line-p)
         (if argument
             (comment-kill (if (integerp argument) argument nil))
           (comment-indent)))
        (t (paredit-insert-comment))))

(defun paredit-comment-on-line-p ()
  "True if there is a comment on the line following point.
This is expected to be called only in `paredit-comment-dwim'; do not
call it elsewhere."
  (save-excursion
    (beginning-of-line)
    (let ((comment-p nil))
      ;; Search forward for a comment beginning.  If there is one, set
      ;; `comment-p' to t; if not, it will be nil.
      (while (progn
               (setq comment-p          ; t -> no error
                     (comment-search-forward (point-at-eol) t))
               (and comment-p
                    (or (paredit-in-string-p)
                        (paredit-in-char-p (1- (point))))))
        (forward-char))
      comment-p)))

(defun paredit-insert-comment ()
  (let ((code-after-p
         (save-excursion (paredit-skip-whitespace t (point-at-eol))
                         (not (eolp))))
        (code-before-p
         (save-excursion (paredit-skip-whitespace nil (point-at-bol))
                         (not (bolp)))))
    (cond ((and (bolp)
                (let ((indent
                       (let ((indent (calculate-lisp-indent)))
                         (if (consp indent) (car indent) indent))))
                  (and indent (zerop indent))))
           ;; top-level comment
           (if code-after-p (save-excursion (newline)))
           (insert paredit-comment-prefix-toplevel))
          ((or code-after-p (not code-before-p))
           ;; code comment
           (if code-before-p
               (newline-and-indent)
             (lisp-indent-line))
           (insert paredit-comment-prefix-code)
           (if code-after-p
               (save-excursion
                 (newline)
                 (lisp-indent-line)
                 (paredit-indent-sexps))))
          (t
           ;; margin comment
           (indent-to comment-column 1) ; 1 -> force one leading space
           (insert paredit-comment-prefix-margin)))))

;;;; Deleting & Killing

(defun paredit-forward-delete (&optional argument)
  "Delete a character forward or move forward over a delimiter.

If on an opening S-expression delimiter, move forward into the
S-expression.
If on a closing S-expression delimiter, refuse to delete unless the
S-expression is empty, in which case delete the whole S-expression.

With a numeric prefix argument N, delete N characters forward.
With a `C-u' prefix argument, simply delete a character forward,
without regard for delimiter balancing."
  (interactive "P")
  (cond ((or (consp argument) (eobp))
         (delete-char +1))
        ((integerp argument)
         (if (< argument 0)
             (paredit-backward-delete argument)
           (while (> argument 0)
             (paredit-forward-delete)
             (setq argument (- argument 1)))))
        ((paredit-in-string-p)
         (paredit-forward-delete-in-string))
        ((paredit-in-comment-p)
         (paredit-forward-delete-in-comment))
        ((paredit-in-char-p)            ; escape -- delete both chars.
         (delete-char -1)
         (delete-char +1))
        ((eq (char-after) ?\\ )         ; ditto
         (delete-char +2))
        ((let ((syn (char-syntax (char-after))))
           (or (eq syn ?\( )
               (eq syn ?\" )))
         (if (save-excursion
               (paredit-handle-sexp-errors (progn (forward-sexp) t)
                 nil))
             (forward-char)
           (message "Deleting spurious opening delimiter")
           (delete-char +1)))
        ((and (not (paredit-in-char-p (1- (point))))
              (eq (char-syntax (char-after)) ?\) )
              (eq (char-before) (matching-paren (char-after))))
         (delete-char -1)               ; empty list -- delete both
         (delete-char +1))              ; delimiters.
        ((eq ?\; (char-after))
         (paredit-forward-delete-comment-start))
        ((eq (char-syntax (char-after)) ?\) )
         (if (paredit-handle-sexp-errors
                 (save-excursion (forward-char) (backward-sexp) t)
               nil)
             (message "End of list!")
           (progn
             (message "Deleting spurious closing delimiter")
             (delete-char +1))))
        ;; Just delete a single character, if it's not a closing
        ;; delimiter.  (The character literal case is already handled
        ;; by now.)
        (t (delete-char +1))))

(defun paredit-forward-delete-in-string ()
  (let ((start+end (paredit-string-start+end-points)))
    (cond ((not (eq (point) (cdr start+end)))
           ;; If it's not the close-quote, it's safe to delete.  But
           ;; first handle the case that we're in a string escape.
           (cond ((paredit-in-string-escape-p)
                  ;; We're right after the backslash, so backward
                  ;; delete it before deleting the escaped character.
                  (delete-char -1))
                 ((eq (char-after) ?\\ )
                  ;; If we're not in a string escape, but we are on a
                  ;; backslash, it must start the escape for the next
                  ;; character, so delete the backslash before deleting
                  ;; the next character.
                  (delete-char +1)))
           (delete-char +1))
          ((eq (1- (point)) (car start+end))
           ;; If it is the close-quote, delete only if we're also right
           ;; past the open-quote (i.e., it's empty), and then delete
           ;; both quotes.  Otherwise we refuse to delete it.
           (delete-char -1)
           (delete-char +1)))))

(defun paredit-check-forward-delete-in-comment ()
  "Point is in a comment, possibly at eol.
We are about to delete some characters forward; if we are at eol,
we are about to delete the line break.  Refuse to do so if if moving
the next line into the comment would break structure."
  (if (eolp)
      (let ((next-line-start (point-at-bol 2))
            (next-line-end (point-at-eol 2)))
        (paredit-check-region next-line-start next-line-end))))

(defun paredit-forward-delete-in-comment ()
  (paredit-check-forward-delete-in-comment)
  (delete-char +1))

(defun paredit-forward-delete-comment-start ()
  "Point precedes a comment start (not at eol).
Refuse to delete a comment start if the comment contains unbalanced."
  (paredit-check-region (+ (point) 1) (point-at-eol))
  (delete-char +1))

(defun paredit-backward-delete (&optional argument)
  "Delete a character backward or move backward over a delimiter.

If on a closing S-expression delimiter, move backward into the
S-expression.
If on an opening S-expression delimiter, refuse to delete unless the
S-expression is empty, in which case delete the whole S-expression.

With a numeric prefix argument N, delete N characters backward.
With a `C-u' prefix argument, simply delete a character backward,
without regard for delimiter balancing."
  (interactive "P")
  (cond ((or (consp argument) (bobp))
         (delete-char -1))
        ((integerp argument)
         (if (< argument 0)
             (paredit-forward-delete (- 0 argument))
           (while (> argument 0)
             (paredit-backward-delete)
             (setq argument (- argument 1)))))
        ((paredit-in-string-p)
         (paredit-backward-delete-in-string))
        ((paredit-in-comment-p)
         (paredit-backward-delete-in-comment))
        ((paredit-in-char-p)            ; escape -- delete both chars.
         (delete-char -1)
         (delete-char +1))
        ((paredit-in-char-p (1- (point)))
         (delete-char -2))              ; ditto
        ((let ((syn (char-syntax (char-before))))
           (or (eq syn ?\) )
               (eq syn ?\" )))
         (if (save-excursion
               (paredit-handle-sexp-errors (progn (backward-sexp) t)
                 nil))
             (backward-char)
           (message "Deleting spurious closing delimiter")
           (delete-char -1)))
        ((and (eq (char-syntax (char-before)) ?\( )
              (eq (char-after) (matching-paren (char-before))))
         (delete-char -1)               ; empty list -- delete both
         (delete-char +1))              ; delimiters.
        ((bolp)
         (paredit-backward-delete-maybe-comment-end))
        ((eq (char-syntax (char-before)) ?\( )
         (if (paredit-handle-sexp-errors
                 (save-excursion (backward-char) (forward-sexp) t)
               nil)
             (message "Beginning of list!")
           (progn
             (message "Deleting spurious closing delimiter")
             (delete-char -1))))
        ;; Delete it, unless it's an opening delimiter.  The case of
        ;; character literals is already handled by now.
        (t
         ;; Turn off the @#&*&!^&(%^ botch in GNU Emacs 24 that changed
         ;; `backward-delete-char' and `backward-delete-char-untabify'
         ;; semantically so that they delete the region in transient
         ;; mark mode.
         (let ((delete-active-region nil))
           (backward-delete-char-untabify +1)))))

(defun paredit-backward-delete-in-string ()
  (let ((start+end (paredit-string-start+end-points)))
    (cond ((not (eq (1- (point)) (car start+end)))
           ;; If it's not the open-quote, it's safe to delete.
           (if (paredit-in-string-escape-p)
               ;; If we're on a string escape, since we're about to
               ;; delete the backslash, we must first delete the
               ;; escaped char.
               (delete-char +1))
           (delete-char -1)
           (if (paredit-in-string-escape-p)
               ;; If, after deleting a character, we find ourselves in
               ;; a string escape, we must have deleted the escaped
               ;; character, and the backslash is behind the point, so
               ;; backward delete it.
               (delete-char -1)))
          ((eq (point) (cdr start+end))
           ;; If it is the open-quote, delete only if we're also right
           ;; past the close-quote (i.e., it's empty), and then delete
           ;; both quotes.  Otherwise we refuse to delete it.
           (delete-char -1)
           (delete-char +1)))))

(defun paredit-backward-delete-in-comment ()
  "Point is in a comment, possibly just after the comment start.
Refuse to delete a comment start if the comment contains unbalanced."
  (if (save-excursion
        (backward-char)
        ;; Must call `paredit-in-string-p' before `paredit-in-comment-p'.
        (not (or (paredit-in-string-p) (paredit-in-comment-p))))
      (paredit-check-region (point) (point-at-eol)))
  (backward-delete-char-untabify +1))

(defun paredit-backward-delete-maybe-comment-end ()
  "Point is at bol, possibly just after a comment end (i.e., the
previous line may have had a line comment).
Refuse to delete a comment end if moving the current line into
the previous line's comment would break structure."
  (if (save-excursion
        (backward-char)
        (and (not (paredit-in-string-p)) (paredit-in-comment-p)))
      (paredit-check-region (point-at-eol) (point-at-bol)))
  (delete-char -1))

(defun paredit-kill (&optional argument)
  "Kill a line as if with `kill-line', but respecting delimiters.

In a string, act exactly as `kill-line' but do not kill past the
closing string delimiter.
On a line with no S-expressions on it starting after the point or
within a comment, act exactly as `kill-line'.
Otherwise, kill all S-expressions that start after the point.

With a `C-u' prefix argument, just do the standard `kill-line'.
With a numeric prefix argument N, do `kill-line' that many times."
  (interactive "P")
  (cond (argument
         (kill-line (if (integerp argument) argument 1)))
        ((paredit-in-string-p)
         (paredit-kill-line-in-string))
        ((paredit-in-comment-p)
         (paredit-kill-line-in-comment))
        ((save-excursion (paredit-skip-whitespace t (point-at-eol))
                         (or (eolp) (eq (char-after) ?\; )))
         ;; Be careful about trailing backslashes.
         (if (paredit-in-char-p)
             (backward-char))
         (kill-line))
        (t (paredit-kill-sexps-on-line))))

(defun paredit-kill-line-in-string ()
  (if (save-excursion (paredit-skip-whitespace t (point-at-eol))
                      (eolp))
      (kill-line)
    (save-excursion
      ;; Be careful not to split an escape sequence.
      (if (paredit-in-string-escape-p)
          (backward-char))
      (kill-region (point)
                   (min (point-at-eol)
                        (cdr (paredit-string-start+end-points)))))))

(defun paredit-kill-line-in-comment ()
  ;; The variable `kill-whole-line' is not relevant: the point is in a
  ;; comment, and hence not at the beginning of the line.
  (paredit-check-forward-delete-in-comment)
  (kill-line))

(defun paredit-kill-sexps-on-line ()
  (if (paredit-in-char-p)               ; Move past the \ and prefix.
      (backward-char 2))                ; (# in Scheme/CL, ? in elisp)
  (let ((beginning (point))
        (eol (point-at-eol)))
    (let ((end-of-list-p (paredit-forward-sexps-to-kill beginning eol)))
      ;; If we got to the end of the list and it's on the same line,
      ;; move backward past the closing delimiter before killing.  (This
      ;; allows something like killing the whitespace in (    ).)
      (if end-of-list-p (progn (up-list) (backward-char)))
      (if kill-whole-line
          (paredit-kill-sexps-on-whole-line beginning)
        (kill-region beginning
                     ;; If all of the S-expressions were on one line,
                     ;; i.e., we're still on that line after moving past
                     ;; the last one, kill the whole line, including
                     ;; any comments; otherwise just kill to the end of
                     ;; the last S-expression we found.  Be sure,
                     ;; though, not to kill any closing parentheses.
                     (if (and (not end-of-list-p)
                              (eq (point-at-eol) eol))
                         eol
                       (point)))))))

(defun paredit-forward-sexps-to-kill (beginning eol)
  (let ((end-of-list-p nil)
        (firstp t))
    ;; Move to the end of the last S-expression that started on this
    ;; line, or to the closing delimiter if the last S-expression in
    ;; this list is on the line.
    (catch 'return
      (while t
        ;; This and the `kill-whole-line' business below fix a bug that
        ;; inhibited any S-expression at the very end of the buffer
        ;; (with no trailing newline) from being deleted.  It's a
        ;; bizarre fix that I ought to document at some point, but I am
        ;; too busy at the moment to do so.
        (if (and kill-whole-line (eobp)) (throw 'return nil))
        (save-excursion
          (paredit-handle-sexp-errors (forward-sexp)
            (up-list)
            (setq end-of-list-p (eq (point-at-eol) eol))
            (throw 'return nil))
          (if (or (and (not firstp)
                       (not kill-whole-line)
                       (eobp))
                  (paredit-handle-sexp-errors
                      (progn (backward-sexp) nil)
                    t)
                  (not (eq (point-at-eol) eol)))
              (throw 'return nil)))
        (forward-sexp)
        (if (and firstp
                 (not kill-whole-line)
                 (eobp))
            (throw 'return nil))
        (setq firstp nil)))
    end-of-list-p))

(defun paredit-kill-sexps-on-whole-line (beginning)
  (kill-region beginning
               (or (save-excursion    ; Delete trailing indentation...
                     (paredit-skip-whitespace t)
                     (and (not (eq (char-after) ?\; ))
                          (point)))
                   ;; ...or just use the point past the newline, if
                   ;; we encounter a comment.
                   (point-at-eol)))
  (cond ((save-excursion (paredit-skip-whitespace nil (point-at-bol))
                         (bolp))
         ;; Nothing but indentation before the point, so indent it.
         (lisp-indent-line))
        ((eobp) nil)      ; Protect the `char-syntax' below against NIL.
        ;; Insert a space to avoid invalid joining if necessary.
        ((let ((syn-before (char-syntax (char-before)))
               (syn-after  (char-syntax (char-after))))
           (or (and (eq syn-before ?\) )          ; Separate opposing
                    (eq syn-after  ?\( ))         ; parentheses
               (and (eq syn-before ?\" )          ; string delimiter
                    (eq syn-after  ?\" ))         ; pairs
               (and (memq syn-before '(?_ ?w))    ; or word or symbol
                    (memq syn-after  '(?_ ?w))))) ; constituents.
         (insert " "))))

;;;;; Killing words

;; This is tricky and asymmetrical because backward parsing is
;; extraordinarily difficult or impossible, so we have to implement
;; killing in both directions by parsing forward.
(defun paredit-forward-kill-word ()
  "Kill a word forward, skipping over intervening delimiters."
  (interactive)
  (let ((beginning (point)))
    (skip-syntax-forward " -")
    (let* ((parse-state (paredit-current-parse-state))
           (state (paredit-kill-word-state parse-state 'char-after)))
      (while (not (or (eobp)
                      (eq ?w (char-syntax (char-after)))))
        (setq parse-state
              (progn (forward-char 1) (paredit-current-parse-state)))
        (let* ((old-state state)
               (new-state
                (paredit-kill-word-state parse-state 'char-after)))
          (cond ((not (eq old-state new-state))
                 (setq parse-state
                       (paredit-kill-word-new-state old-state
                                                    new-state
                                                    parse-state))
                 (setq state
                       (paredit-kill-word-state parse-state
                                                'char-after))
                 (setq beginning (point)))))))
    (goto-char beginning)
    (kill-word 1)))

(defun paredit-backward-kill-word ()
  "Kill a word backward, skipping over any intervening delimiters."
  (interactive)
  (if (not (or (bobp)
               (eq (char-syntax (char-before)) ?w)))
      (let ((end (point)))
        (backward-word 1)
        (forward-word 1)
        (goto-char (min end (point)))
        (let* ((parse-state (paredit-current-parse-state))
               (state
                (paredit-kill-word-state parse-state 'char-before)))
          (while (and (< (point) end)
                      (progn
                        (setq parse-state
                              (parse-partial-sexp (point) (1+ (point))
                                                  nil nil parse-state))
                        (or (eq state
                                (paredit-kill-word-state parse-state
                                                         'char-before))
                            (progn (backward-char 1) nil)))))
          (if (and (eq state 'comment)
                   (eq ?\# (char-after (point)))
                   (eq ?\| (char-before (point))))
              (backward-char 1)))))
  (backward-kill-word 1))

;;;;; Word-killing auxiliaries

(defun paredit-kill-word-state (parse-state adjacent-char-fn)
  (cond ((paredit-in-comment-p parse-state) 'comment)
        ((paredit-in-string-p  parse-state) 'string)
        ((memq (char-syntax (funcall adjacent-char-fn))
               '(?\( ?\) ))
         'delimiter)
        (t 'other)))

(defun paredit-kill-word-new-state (old-state new-state parse-state)
  " This optionally advances the point past any comment delimiters that
should probably not be touched, based on the last state change and
the characters around the point.  It returns a new parse state,
starting from the PARSE-STATE parameter. "
  (cond ((and (not (eq old-state 'comment))
              (not (eq new-state 'comment))
              (not (paredit-in-string-escape-p))
              (eq ?\# (char-before))
              (eq ?\| (char-after)))
         (forward-char 1)
         (paredit-current-parse-state))
        ((and (not (eq old-state 'comment))
              (eq new-state 'comment)
              (eq ?\; (char-before)))
         (skip-chars-forward ";")
         (paredit-current-parse-state))
        (t parse-state)))

(defun paredit-copy-as-kill ()
  "Save in the kill ring the region that `paredit-kill' would kill."
  (interactive)
  (cond ((paredit-in-string-p)
         (paredit-copy-as-kill-in-string))
        ((paredit-in-comment-p)
         (copy-region-as-kill (point) (point-at-eol)))
        ((save-excursion (paredit-skip-whitespace t (point-at-eol))
                         (or (eolp) (eq (char-after) ?\; )))
         ;; Be careful about trailing backslashes.
         (save-excursion
           (if (paredit-in-char-p)
               (backward-char))
           (copy-region-as-kill (point) (point-at-eol))))
        (t (paredit-copy-sexps-as-kill))))

(defun paredit-copy-as-kill-in-string ()
  (save-excursion
    (if (paredit-in-string-escape-p)
        (backward-char))
    (copy-region-as-kill (point)
                         (min (point-at-eol)
                              (cdr (paredit-string-start+end-points))))))

(defun paredit-copy-sexps-as-kill ()
  (save-excursion
    (if (paredit-in-char-p)
        (backward-char 2))
    (let ((beginning (point))
          (eol (point-at-eol)))
      (let ((end-of-list-p (paredit-forward-sexps-to-kill beginning eol)))
        (if end-of-list-p (progn (up-list) (backward-char)))
        (copy-region-as-kill beginning
                             (cond (kill-whole-line
                                    (or (save-excursion
                                          (paredit-skip-whitespace t)
                                          (and (not (eq (char-after) ?\; ))
                                               (point)))
                                        (point-at-eol)))
                                   ((and (not end-of-list-p)
                                         (eq (point-at-eol) eol))
                                    eol)
                                   (t
                                    (point))))))))

;;;;; Deleting regions

(defun paredit-delete-region (start end)
  "Delete the text between point and mark, like `delete-region'.
If that text is unbalanced, signal an error instead.
With a prefix argument, skip the balance check."
  (interactive "r")
  (if (and start end (not current-prefix-arg))
      (paredit-check-region-for-delete start end))
  (setq this-command 'delete-region)
  (delete-region start end))

(defun paredit-kill-region (start end)
  "Kill the text between point and mark, like `kill-region'.
If that text is unbalanced, signal an error instead.
With a prefix argument, skip the balance check."
  (interactive "r")
  (if (and start end (not current-prefix-arg))
      (paredit-check-region-for-delete start end))
  (setq this-command 'kill-region)
  (kill-region start end))

(defun paredit-check-region-for-delete (start end)
  "Signal an error deleting text between START and END is unsafe."
  (save-excursion
    (goto-char start)
    (let* ((start-state (paredit-current-parse-state))
           (end-state (parse-partial-sexp start end nil nil start-state)))
      (paredit-check-region-for-delete:depth start start-state end end-state)
      (paredit-check-region-for-delete:string start start-state end end-state)
      (paredit-check-region-for-delete:comment start start-state end end-state)
      (paredit-check-region-for-delete:char-quote start start-state
                                                  end end-state))))

(defun paredit-check-region-for-delete:depth (start start-state end end-state)
  (let ((start-depth (nth 0 start-state))
        (end-depth (nth 0 end-state)))
    (if (not (= start-depth end-depth))
        (error "Mismatched parenthesis depth: %S at start, %S at end"
               start-depth
               end-depth))))

(defun paredit-check-region-for-delete:string (start start-state end end-state)
  (let ((start-string-p (nth 3 start-state))
        (end-string-p (nth 3 end-state)))
    (if (not (eq start-string-p end-string-p))
        (error "Mismatched string state: start %sin string, end %sin string"
               (if start-string-p "" "not ")
               (if end-string-p "" "not ")))))

(defun paredit-check-region-for-delete:comment
    (start start-state end end-state)
  (let ((start-comment-state (nth 4 start-state))
        (end-comment-state (nth 4 end-state)))
    (if (not (or (eq start-comment-state end-comment-state)
                 ;; If we are moving text into or out of a line
                 ;; comment, make sure that the text is balanced.  (The
                 ;; comment state may be a number, not t or nil at all,
                 ;; for nestable comments, which are not handled by
                 ;; this heuristic (or any of paredit, really).)
                 (and (or (and (eq start-comment-state nil)
                               (eq end-comment-state t))
                          (and (eq start-comment-state t)
                               (eq end-comment-state nil)))
                      (save-excursion
                        (goto-char end)
                        (paredit-region-ok-p (point) (point-at-eol))))))
        (error "Mismatched comment state: %s"
               (cond ((and (integerp start-comment-state)
                           (integerp end-comment-state))
                      (format "depth %S at start, depth %S at end"
                              start-comment-state
                              end-comment-state))
                     ((integerp start-comment-state)
                      "start in nested comment, end otherwise")
                     ((integerp end-comment-state)
                      "end in nested comment, start otherwise")
                     (start-comment-state
                      "start in comment, end not in comment")
                     (end-comment-state
                      "end in comment, start not in comment")
                     (t
                      (format "start %S, end %S"
                              start-comment-state
                              end-comment-state)))))))

(defun paredit-check-region-for-delete:char-quote
    (start start-state end end-state)
  (let ((start-char-quote (nth 5 start-state))
        (end-char-quote (nth 5 end-state)))
    (if (not (eq start-char-quote end-char-quote))
        (let ((phrase "character quotation"))
          (error "Mismatched %s: start %sin %s, end %sin %s"
                 phrase
                 (if start-char-quote "" "not ")
                 phrase
                 (if end-char-quote "" "not ")
                 phrase)))))

;;;; Point motion

(defmacro paredit-define-motion (name bvl doc &rest body)
  (declare (indent defun))
  `(defun ,name ,bvl
     ,doc
     ,(if (fboundp 'handle-shift-selection)
          '(interactive "^p")
        '(interactive "p"))
     ,@body))

(paredit-define-motion paredit-forward (&optional arg)
  "Move forward an S-expression, or up an S-expression forward.

If there are no more S-expressions in this one before the closing
delimiter, move past that closing delimiter; otherwise, move forward
past the S-expression following the point."
  (let ((n (or arg 1)))
    (cond ((< 0 n) (dotimes (i n)       (paredit-move-forward)))
          ((< n 0) (dotimes (i (- n))   (paredit-move-backward))))))

(paredit-define-motion paredit-backward (&optional arg)
  "Move backward an S-expression, or up an S-expression backward.

If there are no more S-expressions in this one before the opening
delimiter, move past that opening delimiter backward; otherwise, move
move backward past the S-expression preceding the point."
  (let ((n (or arg 1)))
    (cond ((< 0 n) (dotimes (i n)       (paredit-move-backward)))
          ((< n 0) (dotimes (i (- n))   (paredit-move-forward))))))

(defun paredit-move-forward ()
  (cond ((paredit-in-string-p)
         (let ((end (paredit-enclosing-string-end)))
           ;; `forward-sexp' and `up-list' may move into the next string
           ;; in the buffer.  Don't do that; move out of the current one.
           (if (paredit-handle-sexp-errors
                   (progn (paredit-handle-sexp-errors (forward-sexp)
                            (up-list))
                          (<= end (point)))
                 t)
               (goto-char end))))
        ((paredit-in-char-p)
         (forward-char))
        (t
         (paredit-handle-sexp-errors (forward-sexp)
           (up-list)))))

(defun paredit-move-backward ()
  (cond ((paredit-in-string-p)
         (let ((start (paredit-enclosing-string-start)))
           (if (paredit-handle-sexp-errors
                   (progn (paredit-handle-sexp-errors (backward-sexp)
                            (backward-up-list))
                          (<= (point) start))
                 t)
               (goto-char start))))
        ((paredit-in-char-p)
                                        ;++ Corner case: a buffer of `\|x'.  What to do?
         (backward-char 2))
        (t
         (paredit-handle-sexp-errors (backward-sexp)
           (backward-up-list)))))

;;;;; upward/downward motion

(defun paredit-up/down (n vertical-direction)
  (let ((horizontal-direction (if (< 0 n) +1 -1)))
    (while (/= n 0)
      (goto-char
       (paredit-next-up/down-point horizontal-direction vertical-direction))
      (setq n (- n horizontal-direction)))))

(defun paredit-next-up/down-point (horizontal-direction vertical-direction)
  (let ((state (paredit-current-parse-state))
        (scan-lists
         (lambda ()
           (scan-lists (point) horizontal-direction vertical-direction))))
    (cond ((paredit-in-string-p state)
           (let ((start+end (paredit-string-start+end-points state)))
             (if (< 0 vertical-direction)
                 (if (< 0 horizontal-direction)
                     (+ 1 (cdr start+end))
                   (car start+end))
               ;; We could let the user try to descend into lists
               ;; within the string, but that would be asymmetric
               ;; with the up case, which rises out of the whole
               ;; string and not just out of a list within the
               ;; string, so this case will just be an error.
               (error "Can't descend further into string"))))
          ((< 0 vertical-direction)
           ;; When moving up, just try to rise up out of the list.
           (or (funcall scan-lists)
               (buffer-end horizontal-direction)))
          ((< vertical-direction 0)
           ;; When moving down, look for a string closer than a list,
           ;; and use that if we find it.
           (let* ((list-start
                   (paredit-handle-sexp-errors (funcall scan-lists) nil))
                  (string-start
                   (paredit-find-next-string-start horizontal-direction
                                                   list-start)))
             (if (and string-start list-start)
                 (if (< 0 horizontal-direction)
                     (min string-start list-start)
                   (max string-start list-start))
               (or string-start
                   ;; Scan again: this is a kludgey way to report the
                   ;; error if there really was one.
                   (funcall scan-lists)
                   (buffer-end horizontal-direction)))))
          (t
           (error "Vertical direction must be nonzero in `%s'"
                  'paredit-up/down)))))

(defun paredit-find-next-string-start (horizontal-direction limit)
  (let ((buffer-limit-p (if (< 0 horizontal-direction) 'eobp 'bobp))
        (next-char (if (< 0 horizontal-direction) 'char-after 'char-before))
        (pastp (if (< 0 horizontal-direction) '> '<)))
    (paredit-handle-sexp-errors
        (save-excursion
          (catch 'exit
            (while t
              (if (or (funcall buffer-limit-p)
                      (and limit (funcall pastp (point) limit)))
                  (throw 'exit nil))
              (forward-sexp horizontal-direction)
              (save-excursion
                (backward-sexp horizontal-direction)
                (if (eq ?\" (char-syntax (funcall next-char)))
                    (throw 'exit (+ (point) horizontal-direction)))))))
      nil)))

(paredit-define-motion paredit-forward-down (&optional argument)
  "Move forward down into a list.
With a positive argument, move forward down that many levels.
With a negative argument, move backward down that many levels."
  (paredit-up/down (or argument +1) -1))

(paredit-define-motion paredit-backward-up (&optional argument)
  "Move backward up out of the enclosing list.
With a positive argument, move backward up that many levels.
With a negative argument, move forward up that many levels.
If in a string initially, that counts as one level."
  (paredit-up/down (- 0 (or argument +1)) +1))

(paredit-define-motion paredit-forward-up (&optional argument)
  "Move forward up out of the enclosing list.
With a positive argument, move forward up that many levels.
With a negative argument, move backward up that many levels.
If in a string initially, that counts as one level."
  (paredit-up/down (or argument +1) +1))

(paredit-define-motion paredit-backward-down (&optional argument)
  "Move backward down into a list.
With a positive argument, move backward down that many levels.
With a negative argument, move forward down that many levels."
  (paredit-up/down (- 0 (or argument +1)) -1))

;;;; Depth-changing:  Wrapping, Splicing & Raising

(defun paredit-wrap-sexp (&optional argument open close)
  "Wrap the following S-expression.

If a `C-u' prefix argument is given, wrap all S-expressions following
the point until the end of the buffer or of the enclosing list.
If a numeric prefix argument N is given, wrap N S-expressions.

Automatically indent the newly wrapped S-expression.
As a special case, if the point is at the end of a list, simply insert
a parenthesis pair, rather than inserting a lone opening delimiter
and then signalling an error, in the interest of preserving
structure.

By default OPEN and CLOSE are round delimiters."
  (interactive "P")
  (paredit-lose-if-not-in-sexp 'paredit-wrap-sexp)
  (let ((open (or open ?\( ))
        (close (or close ?\) )))
    (paredit-handle-sexp-errors
        ((lambda (n) (paredit-insert-pair n open close 'goto-char))
         (cond ((integerp argument) argument)
               ((consp argument) (paredit-count-sexps-forward))
               ((paredit-region-active-p) nil)
               (t 1)))
      (insert close)
      (backward-char)))
  (save-excursion (backward-up-list) (indent-sexp)))

(defun paredit-yank-pop (&optional argument)
  "Replace just-yanked text with the next item in the kill ring.

If this command follows a `yank', just run `yank-pop'.
If this command follows a `paredit-wrap-sexp', or any other paredit
wrapping command (see `paredit-wrap-commands'), run `yank' and
reindent the enclosing S-expression.
If this command is repeated, run `yank-pop' and reindent the enclosing
S-expression.

The argument is passed on to `yank' or `yank-pop'; see their
documentation for details."
  (interactive "*p")
  (cond ((eq last-command 'yank)
         (yank-pop argument))
        ((memq last-command paredit-wrap-commands)
         (yank argument)
         ;; `yank' futzes with `this-command'.
         (setq this-command 'paredit-yank-pop)
         (save-excursion (backward-up-list) (indent-sexp)))
        ((eq last-command 'paredit-yank-pop)
         ;; Pretend we just did a `yank', so that we can use
         ;; `yank-pop' without duplicating its definition.
         (setq last-command 'yank)
         (yank-pop argument)
         ;; Return to our original state.
         (setq last-command 'paredit-yank-pop)
         (setq this-command 'paredit-yank-pop)
         (save-excursion (backward-up-list) (indent-sexp)))
        (t (error "Last command was not a yank or a wrap: %s" last-command))))

(defun paredit-splice-sexp (&optional argument)
  "Splice the list that the point is on by removing its delimiters.

With a prefix argument as in `C-u', kill all S-expressions backward in
the current list before splicing all S-expressions forward into the
enclosing list.
With two prefix arguments as in `C-u C-u', kill all S-expressions
forward in the current list before splicing all S-expressions
backward into the enclosing list.
With a numerical prefix argument N, kill N S-expressions backward in
the current list before splicing the remaining S-expressions into the
enclosing list.  If N is negative, kill forward.

Inside a string, unescape all backslashes, or signal an error if doing
so would invalidate the buffer's structure."
  (interactive "P")
  (if (paredit-in-string-p)
      (paredit-splice-string argument)
    (if (paredit-in-comment-p)
        (error "Can't splice comment"))
    (paredit-handle-sexp-errors (paredit-enclosing-list-start)
      (error "Can't splice top level"))
    (paredit-kill-surrounding-sexps-for-splice argument)
    (let ((delete-start (paredit-enclosing-list-start))
          (delete-end
           (let ((limit
                  (save-excursion
                    (paredit-ignore-sexp-errors (forward-sexp) (backward-sexp))
                    (point))))
             (save-excursion
               (backward-up-list)
               (forward-char +1)
               (paredit-skip-whitespace t limit)
               (point)))))
      (let ((end-marker (make-marker)))
        (save-excursion
          (up-list)
          (delete-char -1)
          (set-marker end-marker (point)))
        (delete-region delete-start delete-end)
        (paredit-splice-reindent delete-start (marker-position end-marker))))))

(defun paredit-splice-reindent (start end)
  (paredit-preserving-column
    ;; If we changed the first subform of the enclosing list, we must
    ;; reindent the whole enclosing list.
    (if (paredit-handle-sexp-errors
            (save-excursion
              (backward-up-list)
              (down-list)
              (paredit-ignore-sexp-errors (forward-sexp))
              (< start (point)))
          nil)
        (save-excursion (backward-up-list) (indent-sexp))
      (paredit-indent-region start end))))

(defun paredit-kill-surrounding-sexps-for-splice (argument)
  (cond ((or (paredit-in-string-p)
             (paredit-in-comment-p))
         (error "Invalid context for splicing S-expressions"))
        ((or (not argument) (eq argument 0)) nil)
        ((or (numberp argument) (eq argument '-))
         ;; Kill S-expressions before/after the point by saving the
         ;; point, moving across them, and killing the region.
         (let* ((argument (if (eq argument '-) -1 argument))
                (saved (paredit-point-at-sexp-boundary (- argument))))
           (goto-char saved)
           (paredit-ignore-sexp-errors (backward-sexp argument))
           (paredit-kill-region-inside saved (point))))
        ((consp argument)
         (let ((v (car argument)))
           (if (= v 4)
               ;; Move backward until we hit the open paren; then
               ;; kill that selected region.
               (let ((end (point)))
                 (paredit-ignore-sexp-errors
                   (while (not (bobp))
                     (backward-sexp)))
                 (paredit-kill-region-inside (point) end))
             ;; Move forward until we hit the close paren; then
             ;; kill that selected region.
             (let ((beginning (point)))
               (paredit-ignore-sexp-errors
                 (while (not (eobp))
                   (forward-sexp)))
               (paredit-kill-region-inside beginning (point))))))
        (t (error "Bizarre prefix argument `%s'" argument))))

(defun paredit-splice-sexp-killing-backward (&optional n)
  "Splice the list the point is on by removing its delimiters, and
also kill all S-expressions before the point in the current list.
With a prefix argument N, kill only the preceding N S-expressions."
  (interactive "P")
  (paredit-splice-sexp (if n
                           (prefix-numeric-value n)
                         '(4))))

(defun paredit-splice-sexp-killing-forward (&optional n)
  "Splice the list the point is on by removing its delimiters, and
also kill all S-expressions after the point in the current list.
With a prefix argument N, kill only the following N S-expressions."
  (interactive "P")
  (paredit-splice-sexp (if n
                           (- (prefix-numeric-value n))
                         '(16))))

(defun paredit-raise-sexp (&optional argument)
  "Raise the following S-expression in a tree, deleting its siblings.
With a prefix argument N, raise the following N S-expressions.  If N
is negative, raise the preceding N S-expressions.
If the point is on an S-expression, such as a string or a symbol, not
between them, that S-expression is considered to follow the point."
  (interactive "P")
  (save-excursion
    (cond ((paredit-in-string-p)
           (goto-char (car (paredit-string-start+end-points))))
          ((paredit-in-char-p)
           (backward-sexp))
          ((paredit-in-comment-p)
           (error "No S-expression to raise in comment")))
    ;; Select the S-expressions we want to raise in a buffer substring.
    (let* ((n (prefix-numeric-value argument))
           (bound (scan-sexps (point) n))
           (sexps
            (if (< n 0)
                (buffer-substring bound (paredit-end-of-sexp-before-point))
              (buffer-substring (paredit-start-of-sexp-after-point) bound))))
      ;; Move up to the list we're raising those S-expressions out of and
      ;; delete it.
      (backward-up-list)
      (delete-region (point) (scan-sexps (point) 1))
      (let* ((indent-start (point))
             (indent-end (save-excursion (insert sexps) (point))))
        ;; If the expression spans multiple lines, its indentation is
        ;; probably broken, so reindent it -- but don't reindent
        ;; anything that we didn't touch outside the expression.
        (if (not (eq (save-excursion (goto-char indent-start) (point-at-eol))
                     (save-excursion (goto-char indent-end) (point-at-eol))))
            (indent-region indent-start indent-end nil))))))

;; The effects of convolution on the surrounding
;; whitespace are pretty random.
(defun paredit-convolute-sexp (&optional n)
  "Convolute S-expressions.
Save the S-expressions preceding point and delete them.
Splice the S-expressions following point.
Wrap the enclosing list in a new list prefixed by the saved text.

With a prefix argument N, move up N lists before wrapping."
  (interactive "p")
  (paredit-lose-if-not-in-sexp 'paredit-convolute-sexp)
  ;; Make sure we can move up before destroying anything.
  (save-excursion (backward-up-list n) (backward-up-list))
  (let (open close)
    (let ((prefix
           (let ((end (point)))
             (paredit-ignore-sexp-errors
               (while (not (bobp)) (backward-sexp)))
             (prog1 (buffer-substring (point) end)
               (backward-up-list)
               (save-excursion (forward-sexp)
                               (setq close (char-before))
                               (delete-char -1))
               (setq open (char-after))
               (delete-region (point) end)
               (if (not (eolp)) (just-one-space))))))
      (backward-up-list n)
      (paredit-insert-pair 1 open close 'goto-char)
      (insert prefix)
      (if (not (eolp)) (just-one-space))
      (save-excursion
        (backward-up-list)
        (paredit-ignore-sexp-errors (indent-sexp))))))

(defun paredit-splice-string (argument)
  (let ((original-point (point))
        (start+end (paredit-string-start+end-points)))
    (let ((start (car start+end))
          (end (cdr start+end)))
      ;; START and END both lie before the respective quote
      ;; characters, which we want to delete; thus we increment START
      ;; by one to extract the string, and we increment END by one to
      ;; delete the string.
      (let* ((escaped-string
              (cond ((not (consp argument))
                     (buffer-substring (1+ start) end))
                    ((= 4 (car argument))
                     (buffer-substring original-point end))
                    (t
                     (buffer-substring (1+ start) original-point))))
             (unescaped-string
              (paredit-unescape-string escaped-string)))
        (if (not unescaped-string)
            (error "Unspliceable string")
          (save-excursion
            (goto-char start)
            (delete-region start (1+ end))
            (insert unescaped-string))
          (if (not (and (consp argument)
                        (= 4 (car argument))))
              (goto-char (- original-point 1))))))))

(defun paredit-unescape-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (and (not (eobp))
                ;; nil -> no bound; t -> no errors.
                (search-forward "\\" nil t))
      (delete-char -1)
      (forward-char))
    (paredit-handle-sexp-errors
        (progn (scan-sexps (point-min) (point-max))
               (buffer-string))
      nil)))

;;;; Promoting & Demoting

(defun paredit-forward-promote-sexp (&optional argument)
  "Add the S-expression following the current list into that list
by moving the closing delimiter.
Automatically reindent the newly promoted S-expression with respect to
its new enclosing form.

If in a string, move the opening double-quote forward by one
S-expression and escape any intervening characters as necessary,
without altering any indentation or formatting."
  (interactive "P")
  (save-excursion
    (cond ((paredit-in-comment-p)
           (error "Invalid context for promoting S-expressions"))
          ((numberp argument)
           (if (< argument 0)
               (paredit-forward-demote-sexp (- 0 argument))
             (while (< 0 argument)
               (paredit-forward-promote-sexp)
               (setq argument (- argument 1)))))
          ((paredit-in-string-p)
           ;; If there is anything to promote into the string, take that.
           ;; Otherwise, try to promote into the enclosing list.
           (if (save-excursion
                 (goto-char (paredit-enclosing-string-end))
                 (paredit-handle-sexp-errors (progn (forward-sexp) nil)
                   t))
               (progn
                 (goto-char (paredit-enclosing-string-end))
                 (paredit-forward-promote-into-list argument))
             (paredit-forward-promote-into-string argument)))
          (t
           (paredit-forward-promote-into-list argument)))))

(defun paredit-forward-promote-into-list (&optional argument)
  (let ((nestedp nil))
    (save-excursion
      (up-list)                           ; Up to the end of the list to
      (let ((close (char-before)))        ; save and delete the closing
        (delete-char -1)                  ; delimiter.
        (let ((start (point)))
          (catch 'return                  ; Go to the end of the desired
            (while t                      ; S-expression, going up a
              (paredit-handle-sexp-errors ; list if it's not in this,
                  (progn (forward-sexp)
                         (if argument
                             (paredit-ignore-sexp-errors
                               (while (not (eobp))
                                 (forward-sexp))))
                         (throw 'return nil))
                (setq nestedp t)
                (up-list)
                (setq close                ; adjusting for mixed
                      (prog1 (char-before) ; delimiters as necessary,
                        (delete-char -1)
                        (insert close))))))
          (insert close)                   ; insert that delimiter.
          (indent-region start (point) nil))))
    (if (and (not nestedp)
             (eq (save-excursion (paredit-skip-whitespace nil) (point))
                 (save-excursion (backward-up-list) (forward-char) (point)))
             (eq (save-excursion (forward-sexp) (backward-sexp) (point))
                 (save-excursion (paredit-skip-whitespace t) (point))))
        (delete-region (save-excursion (paredit-skip-whitespace nil) (point))
                       (save-excursion (paredit-skip-whitespace t) (point))))))

(defun paredit-forward-promote-into-string (&optional argument)
  (let ((start (paredit-enclosing-string-start))
        (end (paredit-enclosing-string-end)))
    (goto-char end)
    ;; Signal any errors that we might get first, before mucking with
    ;; the buffer's contents.
    (save-excursion (forward-sexp))
    (let ((close (char-before)))
      ;; Skip intervening whitespace if we're promoting into an empty
      ;; string.
      (if (and (= (+ start 2) end)
               (eq (save-excursion (paredit-skip-whitespace t) (point))
                   (save-excursion (forward-sexp) (backward-sexp) (point))))
          (delete-region (- (point) 1)
                         (save-excursion (paredit-skip-whitespace t) (point)))
        (delete-char -1))
      (paredit-forward-for-quote
       (save-excursion
         (forward-sexp)
         (if argument
             (while (paredit-handle-sexp-errors (progn (forward-sexp) t) nil)))
         (point)))
      (insert close))))

(defun paredit-forward-demote-sexp (&optional argument)
  "Remove the last S-expression in the current list from that list
by moving the closing delimiter.
Automatically reindent the newly demoted S-expression with respect to
its new enclosing form."
  (interactive "P")
  (paredit-lose-if-not-in-sexp 'paredit-forward-demote-sexp)
  (if (and (numberp argument) (< argument 0))
      (paredit-forward-promote-sexp (- 0 argument))
    (let ((start (point)) (end nil))
      (save-excursion
        (up-list)                     ; Up to the end of the list to
        (let ((close (char-before)))  ; save and delete the closing
          (delete-char -1)            ; delimiter.
          (setq end (point))
          (paredit-ignore-sexp-errors ; Go back to where we want to
            (if (or (not argument)    ; insert the delimiter.
                    (numberp argument))
                (backward-sexp argument)
              (while (paredit-handle-sexp-errors
                         (save-excursion (backward-sexp) (<= start (point)))
                       nil)
                (backward-sexp))))
          (paredit-skip-whitespace nil) ; Skip leading whitespace.
          (cond ((bobp)
                 ;; We'll have deleted the close, but there's no open.
                 ;; Is that OK?
                 (error "Demoting all subexpressions with no open-paren?"))
                ((paredit-in-comment-p) ; Don't put the close-paren in
                 (newline)))            ; a comment.
          (insert close))
        ;; Reindent all of the newly demoted S-expressions.  Start at the
        ;; start of the first demoted S-expression, not at the close we
        ;; just inserted.
        (forward-sexp)
        (backward-sexp)
        (if (or (not argument) (numberp argument))
            (paredit-forward-and-indent argument)
          (indent-region (point) end))))))

(defun paredit-backward-promote-sexp (&optional argument)
  "Add the S-expression preceding the current list into that list
by moving the closing delimiter.
Automatically reindent the whole form into which new S-expression was
promoted.

If in a string, move the opening double-quote backward by one
S-expression and escape any intervening characters as necessary,
without altering any indentation or formatting."
  (interactive "P")
  (save-excursion
    (cond ((paredit-in-comment-p)
           (error "Invalid context for promoting S-expressions"))
          ((numberp argument)
           (if (< argument 0)
               (paredit-backward-demote-sexp (- 0 argument))
             (while (< 0 argument)
               (paredit-backward-promote-sexp)
               (setq argument (- argument 1)))))
          ((paredit-in-string-p)
           ;; If there is anything to promote into the string, take that.
           ;; Otherwise, try to promote into the enclosing list.
           (if (save-excursion
                 (goto-char (paredit-enclosing-string-start))
                 (paredit-handle-sexp-errors (progn (backward-sexp) nil)
                   t))
               (progn
                 (goto-char (paredit-enclosing-string-start))
                 (paredit-backward-promote-into-list argument))
             (paredit-backward-promote-into-string argument)))
          (t
           (paredit-backward-promote-into-list argument)))))

(defun paredit-backward-promote-into-list (&optional argument)
  (let ((nestedp nil))
    (save-excursion
      (backward-up-list)
      (let ((open (char-after)))
        (delete-char +1)
        (catch 'return
          (while t
            (paredit-handle-sexp-errors
                (progn (backward-sexp)
                       (if argument
                           (paredit-ignore-sexp-errors
                             (while (not (bobp))
                               (backward-sexp))))
                       (throw 'return nil))
              (setq nestedp t)
              (backward-up-list)
              (setq open
                    (prog1 (char-after)
                      (save-excursion (insert open) (delete-char +1)))))))
        (insert open))
      ;; Reindent the line at the beginning of wherever we inserted the
      ;; opening delimiter, and then indent the whole S-expression.
      (backward-up-list)
      (lisp-indent-line)
      (indent-sexp))
    ;; If we promoted into an empty list, don't leave dangling space:
    ;; (foo |).
    (if (and (not nestedp)
             (eq (save-excursion (paredit-skip-whitespace nil) (point))
                 (save-excursion (backward-sexp) (forward-sexp) (point)))
             (eq (save-excursion (up-list) (backward-char) (point))
                 (save-excursion (paredit-skip-whitespace t) (point))))
        (delete-region (save-excursion (paredit-skip-whitespace nil) (point))
                       (save-excursion (paredit-skip-whitespace t) (point))))))

(defun paredit-backward-promote-into-string (&optional argument)
  (let ((start (paredit-enclosing-string-start))
        (end (paredit-enclosing-string-end)))
    (goto-char start)
    ;; Signal any errors that we might get first, before mucking with
    ;; the buffer's contents.
    (save-excursion (backward-sexp))
    (let ((open (char-after))
          (target (point)))
      ;; Skip intervening whitespace if we're promoting into an empty
      ;; string.  XXX What about nonempty strings?
      (if (and (= (+ start 2) end)
               (eq (save-excursion (paredit-skip-whitespace nil) (point))
                   (save-excursion (backward-sexp) (forward-sexp) (point))))
          (delete-region (save-excursion (paredit-skip-whitespace nil) (point))
                         (+ (point) 1))
        (delete-char +1))
      (backward-sexp)
      (if argument
          (paredit-ignore-sexp-errors
            (while (not (bobp))
              (backward-sexp))))
      (insert open)
      (paredit-forward-for-quote target))))

(defun paredit-backward-demote-sexp (&optional argument)
  "Remove the first S-expression in the current list from that list
by moving the closing delimiter.
Automatically reindent the demoted S-expression and the form from which
it was demoted."
  (interactive "P")
  (paredit-lose-if-not-in-sexp 'paredit-backward-demote-sexp)
  (if (and (numberp argument) (< argument 0))
      (paredit-backward-promote-sexp (- 0 argument))
    (let ((end (make-marker)))
      (set-marker end (point))
      (save-excursion
        (backward-up-list)
        (let ((open (char-after)))
          (delete-char +1)
          (paredit-ignore-sexp-errors
            (paredit-forward-and-indent
             (if (or (not argument) (numberp argument))
                 argument
               (let ((n 0))
                 (save-excursion
                   (while (paredit-handle-sexp-errors
                              (save-excursion
                                (forward-sexp)
                                (<= (point) end))
                            nil)
                     (forward-sexp)
                     (setq n (+ n 1))))
                 n))))
          (while (progn (paredit-skip-whitespace t) (eq (char-after) ?\; ))
            (forward-line 1))
          (if (eobp)
              (error "Demoting all subexpressions with no close-paren?"))
          ;; Don't use `insert' here.  Consider, e.g., demoting from (foo|)
          ;; and how `save-excursion' works.
          (insert-before-markers open))
        (backward-up-list)
        (lisp-indent-line)
        (indent-sexp)))))

;;;; Splitting & Joining

(defun paredit-split-sexp ()
  "Split the list or string the point is on into two."
  (interactive)
  (cond ((paredit-in-string-p)
         (insert "\"")
         (save-excursion (insert " \"")))
        ((or (paredit-in-comment-p)
             (paredit-in-char-p))
         (error "Invalid context for splitting S-expression"))
        (t
         (let ((open (save-excursion (backward-up-list) (char-after)))
               (close (save-excursion (up-list) (char-before))))
           (delete-horizontal-space)
           (insert close)
           (save-excursion
             (insert ?\ )
             (insert open)
             (backward-char)
             (indent-sexp))))))

(defun paredit-join-sexps ()
  "Join the S-expressions adjacent on either side of the point.
Both must be lists, strings, or atoms; error if there is a mismatch."
  (interactive)
  (cond ((paredit-in-comment-p) (error "Can't join S-expressions in comment"))
        ((paredit-in-string-p) (error "Nothing to join in a string"))
        ((paredit-in-char-p) (error "Can't join characters")))
  (let ((left-point (paredit-end-of-sexp-before-point))
        (right-point (paredit-start-of-sexp-after-point)))
    (let ((left-char (char-before left-point))
          (right-char (char-after right-point)))
      (let ((left-syntax (char-syntax left-char))
            (right-syntax (char-syntax right-char)))
        (cond ((< right-point left-point)
               (error "Can't join a datum with itself"))
              ((and (eq left-syntax ?\) )
                    (eq right-syntax ?\( )
                    (eq left-char (matching-paren right-char))
                    (eq right-char (matching-paren left-char)))
               (paredit-join-lists-internal left-point right-point)
               (paredit-preserving-column
                 (save-excursion
                   (backward-up-list)
                   (indent-sexp))))
              ((and (eq left-syntax ?\" )
                    (eq right-syntax ?\" ))
               ;; Delete any intermediate formatting.
               (delete-region (1- left-point) (1+ right-point)))
              ((and (memq left-syntax '(?w ?_)) ; word or symbol
                    (memq right-syntax '(?w ?_)))
               (delete-region left-point right-point))
              (t (error "Mismatched S-expressions to join")))))))

(defun paredit-join-lists-internal (left-point right-point)
  (save-excursion
    ;; Leave intermediate formatting alone.
    (goto-char right-point)
    (delete-char +1)
    (goto-char left-point)
    (delete-char -1)
    ;; Kludge: Add an extra space in several conditions.
    (if (or
         ;; (foo)| ;x\n(bar) => (foo | ;x\nbar), not (foo|  ;x\nbar).
         (and (not (eolp))
              (save-excursion
                (paredit-skip-whitespace t (point-at-eol))
                (eq (char-after) ?\;)))
         ;; (foo)|(bar) => (foo| bar), not (foo|bar).
         (and (= left-point right-point)
              (not (or (eq ?\  (char-syntax (char-before)))
                       (eq ?\  (char-syntax (char-after)))))))
        (insert ?\  ))))


;;; Utilities

(defun paredit-in-string-escape-p ()
  "True if the point is on a character escape of a string.
This is true only if the character is preceded by an odd number of
backslashes.

This assumes that `paredit-in-string-p' has already returned true."
  (let ((oddp nil))
    (save-excursion
      (while (eq (char-before) ?\\ )
        (setq oddp (not oddp))
        (backward-char)))
    oddp))

(defun paredit-in-char-p (&optional position)
  "True if point is on a character escape outside a string."
  (save-excursion
    (goto-char (or position (point)))
    (paredit-in-string-escape-p)))

(defun paredit-skip-whitespace (trailing-p &optional limit)
  "Skip past any whitespace, or until the point LIMIT is reached.
If TRAILING-P is nil, skip leading whitespace; otherwise, skip trailing
whitespace."
  (funcall (if trailing-p 'skip-chars-forward 'skip-chars-backward)
           " \t\n" ; This should skip using the syntax table, but LF
           limit))   ; is a comment end, not newline, in Lisp mode.

(defalias 'paredit-region-active-p
  (lambda () (and mark-active transient-mark-mode)))

(defun paredit-kill-region-inside (start end)
  "Kill the region between START and END.
Do not append to any current kill, and
do not let the next kill append to this one."
  (interactive "r")
  ;; `kill-region' sets `this-command' to tell the next kill that the last
  ;; command was a kill.  It also checks `last-command' to see whether it
  ;; should append.  If we bind these locally, any modifications to
  ;; `this-command' will be masked, and it will not see `last-command' to
  ;; indicate that it should append.
  (let ((this-command nil)
        (last-command nil))
    (kill-region start end)))

;;;; Reindentation utilities

(defun paredit-indent-sexps ()
  "If in a list, indent all following S-expressions in the list."
  (let* ((start (point))
         (end (paredit-handle-sexp-errors (progn (up-list) (point)) nil)))
    (if end
        (indent-region start end nil))))

(defun paredit-forward-and-indent (&optional n)
  "Move forward by N S-expressions, indenting them with `indent-region'."
  (let ((start (point)))
    (forward-sexp n)
    (indent-region start (point) nil)))

(defun paredit-indent-region (start end)
  "Indent the region from START to END.
Don't reindent the line starting at START, however."
  (if (not (<= start end))
      (error "Incorrectly related points: %S, %S" start end))
  (save-excursion
    (goto-char start)
    (let ((bol (point-at-bol)))
      ;; Skip all S-expressions that end on the starting line, but
      ;; don't go past END.
      (if (and (save-excursion (goto-char end) (not (eq bol (point-at-bol))))
               (paredit-handle-sexp-errors
                   (catch 'exit
                     (while t
                       (save-excursion
                         (forward-sexp)
                         (if (not (eq bol (point-at-bol)))
                             (throw 'exit t))
                         (if (not (< (point) end))
                             (throw 'exit nil)))
                       (forward-sexp)))
                 nil))
          (progn
            ;; Point is still on the same line, but precedes an
            ;; S-expression that ends on a different line.
            (if (not (eq bol (point-at-bol)))
                (error "Internal error -- we moved forward a line!"))
            (goto-char (+ 1 (point-at-eol)))
            (if (not (<= (point) end))
                (error "Internal error -- we frobnitzed the garfnut!"))
            (indent-region (point) end nil))))))

;;;; S-expression parsing utilities

;; These routines redundantly traverse S-expressions a great deal.
;; If performance issues arise, this whole section will probably have
;; to be refactored to preserve the state longer, like paredit.scm
;; does, rather than to traverse the definition N times for every key
;; stroke as it presently does.
(defun paredit-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    ;; Calling `parse-partial-sexp' will advance the point to its second
    ;; argument (unless parsing stops due to an error, but we assume it
    ;; won't in paredit-mode).
    (parse-partial-sexp (point) point)))

(defun paredit-in-string-p (&optional state)
  "True if the parse state is within a double-quote-delimited string.
If no parse state is supplied, compute one from the beginning of the
defun to the point."
  ;; 3. non-nil if inside a string (the terminator character, really)
  (and (nth 3 (or state (paredit-current-parse-state)))
       t))

(defun paredit-string-start+end-points (&optional state)
  "Return a cons of the points of open and close quotes of the string.
The string is determined from the parse state STATE, or the parse state
from the beginning of the defun to the point.

This assumes that `paredit-in-string-p' has already returned true, i.e.,
that the point is already within a string."
  (save-excursion
    ;; 8: character address of start of comment or string; nil if not
    ;;    in one
    (let ((start (nth 8 (or state (paredit-current-parse-state)))))
      (goto-char start)
      (forward-sexp 1)
      (cons start (1- (point))))))

(defun paredit-enclosing-string-start ()
  (car (paredit-string-start+end-points)))

(defun paredit-enclosing-string-end ()
  (+ 1 (cdr (paredit-string-start+end-points))))

(defun paredit-enclosing-list-start ()
  (save-excursion
    (backward-up-list)
    (point)))

(defun paredit-enclosing-list-end ()
  (save-excursion
    (up-list)
    (point)))

(defun paredit-in-comment-p (&optional state)
  "True if parse state STATE is within a comment.
If no parse state is supplied, compute one from the beginning of the
defun to the point."
  ;; 4: nil if outside a comment, t if inside a non-nestable comment,
  ;;    else an integer (the current comment nesting)
  (and (nth 4 (or state (paredit-current-parse-state)))
       t))

(defun paredit-prefix-numeric-value (argument)
  (cond ((integerp argument) argument)
        ((eq argument '-) -1)
        ((consp argument)
         (cond ((equal argument '(4)) (paredit-count-sexps-forward))
               ((equal argument '(16)) (paredit-count-sexps-backward))
               (t (error "Invalid prefix argument: %S" argument))))
        ((paredit-region-active-p)
         (save-excursion
           (save-restriction
             (narrow-to-region (region-beginning) (region-end))
             (cond ((= (point) (point-min)) (paredit-count-sexps-forward))
                   ((= (point) (point-max)) (paredit-count-sexps-backward))
                   (t
                    (error "Point %S is not start or end of region: %S..%S"
                           (point) (region-beginning) (region-end)))))))
        (t 1)))

(defun paredit-count-sexps-forward ()
  (save-excursion
    (let ((n 0) (p nil))
      (paredit-ignore-sexp-errors
        (while (setq p (scan-sexps (point) +1))
          (goto-char p)
          (setq n (+ n 1))))
      n)))

(defun paredit-count-sexps-backward ()
  (save-excursion
    (let ((n 0) (p nil))
      (paredit-ignore-sexp-errors
        (while (setq p (scan-sexps (point) -1))
          (goto-char p)
          (setq n (+ n 1))))
      n)))

(defun paredit-point-at-sexp-boundary (n)
  (cond ((< n 0) (paredit-start-of-sexp-after-point))
        ((= n 0) (point))
        ((> n 0) (paredit-end-of-sexp-before-point))))

(defun paredit-start-of-sexp-after-point ()
  (save-excursion
    (forward-sexp)
    (backward-sexp)
    (point)))

(defun paredit-end-of-sexp-before-point ()
  (save-excursion
    (backward-sexp)
    (forward-sexp)
    (point)))

(defun paredit-lose-if-not-in-sexp (command)
  (if (or (paredit-in-string-p)
          (paredit-in-comment-p)
          (paredit-in-char-p))
      (error "Invalid context for command `%s'" command)))

(defun paredit-check-region (start end)
  "Signal an error if text between START and END is unbalanced."
  ;; `narrow-to-region' will move the point, so avoid calling it if we
  ;; don't need to.  We don't want to use `save-excursion' because we
  ;; want the point to move if `check-parens' reports an error.
  (if (not (paredit-region-ok-p start end))
      (save-restriction
        (narrow-to-region start end)
        (check-parens))))

(defun paredit-region-ok-p (start end)
  "Return true iff the region between START and END is balanced.
This is independent of context -- it doesn't check what state the
text at `start' is in."
  (save-excursion
    (paredit-handle-sexp-errors
        (progn
          (save-restriction
            (narrow-to-region start end)
            (scan-sexps (point-min) (point-max)))
          t)
      nil)))

(defun paredit-current-indentation ()
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun paredit-restore-column (column indentation)
  ;; Preserve the point's position either in the indentation or in the
  ;; code: if on code, move with the code; if in indentation, leave it
  ;; in the indentation, either where it was (if still on indentation)
  ;; or at the end of the indentation (if the code moved far enough
  ;; left).
  (let ((indentation* (paredit-current-indentation)))
    (goto-char
     (+ (point-at-bol)
        (cond ((not (< column indentation))
               (+ column (- indentation* indentation)))
              ((<= indentation* column) indentation*)
              (t column))))))


;;; Hacker loves

(defvar paredit-override-check-parens-function
  (lambda (condition) (declare ignore condition) nil)
  "Function to tell whether unbalanced text should inhibit paredit mode.")

(defvar paredit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "("              'paredit-open-round)
    (define-key map ")"              'paredit-close-round)
    (define-key map "["              'paredit-open-square)
    (define-key map "]"              'paredit-close-square)
    (define-key map ";"              'paredit-semicolon)
    (define-key map "\\"             'paredit-backslash)
    (define-key map [delete]         'paredit-forward-delete)
    (define-key map [deletechar]     'paredit-forward-delete)
    (define-key map (kbd "DEL")      'paredit-backward-delete)
    (define-key map (kbd "C-d")      'paredit-forward-delete)
    (define-key map (kbd "C-j")      'paredit-newline)
    (define-key map (kbd "C-k")      'paredit-kill)
    (define-key map (kbd "C-)")      'paredit-forward-promote-sexp)
    (define-key map (kbd "C-}")      'paredit-forward-demote-sexp)
    (define-key map (kbd "C-(")      'paredit-backward-promote-sexp)
    (define-key map (kbd "C-{")      'paredit-backward-demote-sexp)
    (define-key map (kbd "M-)")      'paredit-close-round-and-newline)
    (define-key map (kbd "M-]")      'paredit-close-square-and-newline)
    (define-key map (kbd "M-}")      'paredit-close-angled-and-newline)
    (define-key map (kbd "M-d")      'paredit-forward-kill-word)
    (define-key map (kbd "M-DEL")    'paredit-backward-kill-word)
    (define-key map (kbd "M-\"")     'paredit-meta-doublequote)
    (define-key map (kbd "M-;")      'paredit-comment-dwim)
    (define-key map (kbd "C-M-f")    'paredit-forward)
    (define-key map (kbd "C-M-b")    'paredit-backward)
    (define-key map (kbd "C-M-u")    'paredit-backward-up)
    (define-key map (kbd "C-M-d")    'paredit-forward-down)
    (define-key map (kbd "C-M-p")    'paredit-backward-down)
    (define-key map (kbd "C-M-n")    'paredit-forward-up)
    (define-key map (kbd "M-(")      'paredit-wrap-round)
    (define-key map (kbd "M-s")      'paredit-splice-sexp)
    (define-key map (kbd "M-<up>")   'paredit-splice-sexp-killing-backward)
    (define-key map (kbd "M-<down>") 'paredit-splice-sexp-killing-forward)
    (define-key map (kbd "M-r")      'paredit-raise-sexp)
    (define-key map (kbd "M-?")      'paredit-convolute-sexp)
    (define-key map (kbd "M-S")      'paredit-split-sexp)
    (define-key map (kbd "M-J")      'paredit-join-sexps)
    map)
  "Keymap for the `paredit-mode'.")

;;;###autoload
(define-minor-mode paredit-mode
  "Minor mode for pseudo-structurally editing Lisp code.

With a prefix argument, enable paredit mode even if there are
unbalanced parentheses in the buffer.

Paredit behaves badly if parentheses are unbalanced, so exercise
caution when forcing paredit mode to be enabled, and consider
fixing unbalanced parentheses instead.

\\{paredit-mode-map}"
  :lighter " Paredit"
  :keymap paredit-mode-map
  (if (and paredit-mode
           (not current-prefix-arg))
      (condition-case condition
          (check-parens)
        (error
         (if (not (funcall paredit-override-check-parens-function condition))
             (progn (setq paredit-mode nil)
                    (signal (car condition) (cdr condition))))))))

(provide 'paredit)

;;; paredit.el ends here
