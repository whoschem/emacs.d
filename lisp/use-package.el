;;; use-package.el --- A configuration macro for simplifying dotemacs.  -*- lexical-binding: t  -*-

;; Copyright (C) 2012-2017  John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Package-Requires: ((emacs "25.1"))
;; Keywords: dotemacs startup speed package

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

;; The `use-package' declaration macro allows you to isolate package
;; configuration in your ".emacs" in a way that is performance-oriented and,
;; well, just tidy.  I created it because I have over 80 packages that I use
;; in Emacs, and things were getting difficult to manage.  Yet with this
;; utility my total load time is just under 1 second, with no loss of
;; functionality!

;;; Code:

(require 'cl-lib)
(require 'bytecomp)
(require 'tabulated-list)
(require 'easy-mmode)

(eval-and-compile
  ;; Declare a synthetic theme for :custom variables.
  ;; Necessary in order to avoid having those variables saved by custom.el.
  (deftheme use-package))

(enable-theme 'use-package)

;; Remove the synthetic use-package theme from the enabled themes, so
;; iterating over them to "disable all themes" won't disable it.
(setq custom-enabled-themes (remq 'use-package custom-enabled-themes))

(eval-when-compile
  (require 'subr-x)
  (require 'regexp-opt))

(defgroup use-package nil
  "A use-package declaration for simplifying your `.emacs'."
  :group 'startup)

(defcustom use-package-keywords
  '(:disabled
    :pin
    :ensure
    :load-path
    :requires
    :defines
    :functions
    :preface
    :if :when :unless
    :no-require
    :catch
    :after
    :custom
    :custom-face
    :bind
    :bind*
    :bind-keymap
    :bind-keymap*
    :interpreter
    :mode
    :magic
    :magic-fallback
    :hook
    ;; Any other keyword that also declares commands to be autoloaded (such as
    ;; :bind) must appear before this keyword.
    :commands
    :init
    :defer
    :demand
    :load
    ;; This must occur almost last; the only forms which should appear after
    ;; are those that must happen directly after the config forms.
    :delight
    :config)
  "The set of valid keywords, in the order they are processed in.
The order of this list is *very important*, so it is only
advisable to insert new keywords, never to delete or reorder
them.  Further, attention should be paid to the NEWS.md if the
default order ever changes, as they may have subtle effects on
the semantics of use-package declarations and may necessitate
changing where you had inserted a new keyword earlier.

Note that `:disabled' is special in this list, as it causes
nothing at all to happen, even if the rest of the use-package
declaration is incorrect."
  :type '(repeat symbol)
  :group 'use-package)

(defcustom use-package-deferring-keywords
  '(:bind-keymap
    :bind-keymap*
    :commands)
  "Unless `:demand' is used, keywords in this list imply deferred loading.
The reason keywords like `:hook' are not in this list is that
they only imply deferred loading if they reference actual
function symbols that can be autoloaded from the module; whereas
the default keywords provided here always defer loading unless
otherwise requested."
  :type '(repeat symbol)
  :group 'use-package)

(defcustom use-package-ignore-unknown-keywords nil
  "If non-nil, issue warning instead of error when unknown
keyword is encountered.  The unknown keyword and its associated
arguments will be ignored in the `use-package' expansion."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-use-theme t
  "If non-nil, use a custom theme to avoid saving :custom
variables twice (once in the Custom file, once in the use-package
call)."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-verbose nil
  "Whether to report about loading and configuration details.
If you customize this, then you should require the `use-package'
feature in files that use `use-package', even if these files only
contain compiled expansions of the macros.  If you don't do so,
then the expanded macros do their job silently."
  :type '(choice (const :tag "Quiet, without catching errors" errors)
                 (const :tag "Quiet" nil)
                 (const :tag "Verbose" t)
                 (const :tag "Debug" debug))
  :group 'use-package)

(defcustom use-package-check-before-init nil
  "If non-nil, check that package exists before executing its `:init' block.
This check is performed by calling `locate-library'."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-always-defer nil
  "If non-nil, assume `:defer t' unless `:demand' is used.
See also `use-package-defaults', which uses this value."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-always-demand nil
  "If non-nil, assume `:demand t' unless `:defer' is used.
See also `use-package-defaults', which uses this value."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-defaults
  '(;; this '(t) has special meaning; see `use-package-handler/:config'
    (:config '(t) t)
    (:init nil t)
    (:catch t (lambda (name args)
                (not use-package-expand-minimally)))
    (:pin use-package-always-pin use-package-always-pin)
    (:ensure (list use-package-always-ensure)
             (lambda (name args)
               (and use-package-always-ensure
                    (not (plist-member args :load-path)))))
    (:defer use-package-always-defer
            (lambda (name args)
              (and use-package-always-defer
                   (not (plist-member args :defer))
                   (not (plist-member args :demand)))))
    (:demand use-package-always-demand
             (lambda (name args)
               (and use-package-always-demand
                    (not (plist-member args :defer))
                    (not (plist-member args :demand))))))
  "Default values for specified `use-package' keywords.
Each entry in the alist is a list of three elements:
The first element is the `use-package' keyword.

The second is a form that can be evaluated to get the default
value.  It can also be a function that will receive the name of
the use-package declaration and the keyword plist given to
`use-package', in normalized form.  The value it returns should
also be in normalized form (which is sometimes *not* what one
would normally write in a `use-package' declaration, so use
caution).

The third element is a form that can be evaluated to determine
whether or not to assign a default value; if it evaluates to nil,
then the default value is not assigned even if the keyword is not
present in the `use-package' form.  This third element may also be
a function, in which case it receives the name of the package (as
a symbol) and a list of keywords (in normalized form).  It should
return nil or non-nil depending on whether defaulting should be
attempted."
  :type `(repeat
          (list (choice :tag "Keyword"
                        ,@(mapcar #'(lambda (k) (list 'const k))
                                  use-package-keywords))
                (choice :tag "Default value" sexp function)
                (choice :tag "Enable if non-nil" sexp function)))
  :group 'use-package)

(defcustom use-package-merge-key-alist
  '((:if    . (lambda (new old) `(and ,new ,old)))
    (:after . (lambda (new old) `(:all ,new ,old)))
    (:defer . (lambda (new old) old))
    (:bind  . (lambda (new old) (append new (list :break) old))))
  "Alist of keys and the functions used to merge multiple values.
For example, if the following form is provided:

  (use-package foo :if pred1 :if pred2)

Then based on the above defaults, the merged result will be:

  (use-package foo :if (and pred1 pred2))

This is done so that, at the stage of invoking handlers, each
handler is called only once."
  :type `(repeat
          (cons (choice :tag "Keyword"
                        ,@(mapcar #'(lambda (k) (list 'const k))
                                  use-package-keywords)
                        (const :tag "Any" t))
                function))
  :group 'use-package)

(defcustom use-package-hook-name-suffix "-hook"
  "Text append to the name of hooks mentioned by :hook.
Set to nil if you don't want this to happen; it's only a
convenience."
  :type '(choice string (const :tag "No suffix" nil))
  :group 'use-package)

(defcustom use-package-minimum-reported-time 0.1
  "Minimal load time that will be reported.
Note that `use-package-verbose' has to be set to a non-nil value
for anything to be reported at all."
  :type 'number
  :group 'use-package)

(defcustom use-package-inject-hooks nil
  "If non-nil, add hooks to the `:init' and `:config' sections.
In particular, for a given package `foo', the following hooks
become available:

  `use-package--foo--pre-init-hook'
  `use-package--foo--post-init-hook'
  `use-package--foo--pre-config-hook'
  `use-package--foo--post-config-hook'

This way, you can add to these hooks before evaluation of a
`use-package` declaration, and exercise some control over what
happens.

NOTE: These hooks are run even if the user does not specify an
`:init' or `:config' block, and they will happen at the regular
time when initialization and configuration would have been
performed.

NOTE: If the `pre-init' hook return a nil value, that block's
user-supplied configuration is not evaluated, so be certain to
return t if you only wish to add behavior to what the user had
specified."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-expand-minimally nil
  "If non-nil, make the expanded code as minimal as possible.
This disables:

  - Printing to the *Messages* buffer of slowly-evaluating forms
  - Capturing of load errors (normally redisplayed as warnings)
  - Conditional loading of packages (load failures become errors)

The main advantage to this variable is that, if you know your
configuration works, it will make the byte-compiled file as
minimal as possible.  It can also help with reading macro-expanded
definitions, to understand the main intent of what's happening."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-form-regexp-eval
  `(concat ,(eval-when-compile
              (concat "^\\s-*("
                      (regexp-opt '("use-package" "require") t)
                      "\\s-+\\("))
           (or (bound-and-true-p lisp-mode-symbol-regexp)
               "\\(?:\\sw\\|\\s_\\|\\\\.\\)+") "\\)")
  "Sexp providing regexp for finding `use-package' forms in user files.
This is used by `use-package-jump-to-package-form' and
`use-package-enable-imenu-support'."
  :type 'sexp
  :group 'use-package)

(defcustom use-package-enable-imenu-support nil
  "If non-nil, cause imenu to see `use-package' declarations.
This is done by adjusting `lisp-imenu-generic-expression' to
include support for finding `use-package' and `require' forms.

Must be set before loading `use-package'."
  :type 'boolean
  :set
  #'(lambda (_sym value)
      (eval-after-load 'lisp-mode
        (if value
            `(add-to-list 'lisp-imenu-generic-expression
                          (list "Packages" ,use-package-form-regexp-eval 2))
          `(setq lisp-imenu-generic-expression
                 (remove (list "Packages" ,use-package-form-regexp-eval 2)
                         lisp-imenu-generic-expression)))))
  :group 'use-package)

(defconst use-package-font-lock-keywords
  '(("(\\(use-package\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode use-package-font-lock-keywords)

(defcustom use-package-compute-statistics t
  "If non-nil, compute statistics concerned use-package declarations.
View the statistical report using `use-package-report'.  Note that
if this option is enabled, you must require `use-package' in your
user init file at loadup time, or you will see errors concerning
undefined variables."
  :type 'boolean
  :group 'use-package)

(defvar use-package-statistics (make-hash-table))

(defgroup use-package-ensure nil
  "Support for :ensure and :pin keywords in use-package declarations."
  :group 'use-package)

(eval-when-compile
  (declare-function package-installed-p "package")
  (declare-function package-read-all-archive-contents "package" ()))

(defcustom use-package-always-ensure nil
  "Treat every package as though it had specified using `:ensure SEXP'.
See also `use-package-defaults', which uses this value."
  :type 'sexp
  :group 'use-package-ensure)

(defcustom use-package-always-pin nil
  "Treat every package as though it had specified using `:pin SYM'.
See also `use-package-defaults', which uses this value."
  :type 'symbol
  :group 'use-package-ensure)

(defcustom use-package-ensure-function 'use-package-ensure-elpa
  "Function that ensures a package is installed.
This function is called with three arguments: the name of the
package declared in the `use-package' form; the arguments passed
to all `:ensure' keywords (always a list, even if only one); and
the current `state' plist created by previous handlers.

Note that this function is called whenever `:ensure' is provided,
even if it is nil.  It is up to the function to decide on the
semantics of the various values for `:ensure'.

This function should return non-nil if the package is installed.

The default value uses package.el to install the package."
  :type '(choice (const :tag "package.el" use-package-ensure-elpa)
                 (function :tag "Custom"))
  :group 'use-package-ensure)

(defgroup use-package-bind-key nil
  "A simple way to manage personal keybindings"
  :group 'use-package)

(defcustom use-package-bind-key-column-widths '(18 . 40)
  "Width of columns in `use-package-describe-personal-keybindings'."
  :type '(cons integer integer)
  :group 'use-package-bind-key)

(defcustom use-package-bind-key-segregation-regexp
  "\\`\\(\\(C-[chx] \\|M-[gso] \\)\\([CM]-\\)?\\|.+-\\)"
  "Regular expression used to divide key sets in the output from
\\[use-package-describe-personal-keybindings]."
  :type 'regexp
  :group 'use-package-bind-key)

(defcustom use-package-bind-key-describe-special-forms nil
  "If non-nil, extract docstrings from lambdas, closures and keymaps if possible."
  :type 'boolean
  :group 'use-package-bind-key)


;;; Utility functions

(defsubst use-package-error (msg)
  "Report MSG as an error, so the user knows it came from this package."
  (error "use-package: %s" msg))

(defsubst use-package-concat (&rest elems)
  "Delete all empty lists from ELEMS (nil or (list nil)), and append them."
  (apply #'append (delete nil (delete (list nil) elems))))

(defsubst use-package-non-nil-symbolp (sym)
  (and sym (symbolp sym)))

(defsubst use-package-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise
convert it to a symbol and return that."
  (if (symbolp string-or-symbol) string-or-symbol
    (intern string-or-symbol)))

(defsubst use-package-as-string (string-or-symbol)
  "If STRING-OR-SYMBOL is already a string, return it.  Otherwise
convert it to a string and return that."
  (if (stringp string-or-symbol) string-or-symbol
    (symbol-name string-or-symbol)))

(defsubst use-package-regex-p (re)
  "Return t if RE is some regexp-like thing."
  (or (and (listp re) (eq (car re) 'rx))
      (stringp re)))

(defun use-package-normalize-regex (re)
  "Given some regexp-like thing in RE, resolve to a regular expression."
  (cond
   ((and (listp re) (eq (car re) 'rx)) (eval re))
   ((stringp re) re)
   (t (error "Not recognized as regular expression: %s" re))))

(defsubst use-package-is-pair (x car-pred cdr-pred)
  "Return non-nil if X is a cons satisfying the given predicates.
CAR-PRED and CDR-PRED are applied to X's `car' and `cdr',
respectively."
  (and (consp x)
       (funcall car-pred (car x))
       (funcall cdr-pred (cdr x))))

(defun use-package-as-mode (string-or-symbol)
  "If STRING-OR-SYMBOL ends in `-mode' (or its name does), return
it as a symbol.  Otherwise, return it as a symbol with `-mode'
appended."
  (let ((string (use-package-as-string string-or-symbol)))
    (intern (if (string-match "-mode\\'" string)
                string
              (concat string "-mode")))))

(defsubst use-package-load-name (name &optional noerror)
  "Return a form which will load or require NAME.
It does the right thing no matter if NAME is a string or symbol.
Argument NOERROR means to indicate load failures as a warning."
  (if (stringp name)
      `(load ,name ,noerror)
    `(require ',name nil ,noerror)))

(defun use-package-hook-injector (name-string keyword body)
  "Wrap pre/post hook injections around the given BODY for KEYWORD.
The BODY is a list of forms, so `((foo))' if only `foo' is being called."
  (if (not use-package-inject-hooks)
      body
    (let ((keyword-name (substring (format "%s" keyword) 1)))
      `((when (run-hook-with-args-until-failure
               ',(intern (concat "use-package--" name-string
                                 "--pre-" keyword-name "-hook")))
          ,@body
          (run-hooks
           ',(intern (concat "use-package--" name-string
                             "--post-" keyword-name "-hook"))))))))

(defun use-package-with-elapsed-timer (text body)
  "BODY is a list of forms, so `((foo))' if only `foo' is being called."
  (declare (indent 1))
  (if use-package-expand-minimally
      body
    (let ((nowvar (make-symbol "now")))
      (if (bound-and-true-p use-package-verbose)
          `((let ((,nowvar (current-time)))
              (message "%s..." ,text)
              (prog1
                  ,(macroexp-progn body)
                (let ((elapsed
                       (float-time (time-subtract (current-time) ,nowvar))))
                  (if (> elapsed ,use-package-minimum-reported-time)
                      (message "%s...done (%.3fs)" ,text elapsed)
                    (message "%s...done" ,text))))))
        body))))

(put 'use-package-with-elapsed-timer 'lisp-indent-function 1)

(defun use-package-require (name &optional no-require body)
  (if use-package-expand-minimally
      (use-package-concat
       (unless no-require
         (list (use-package-load-name name)))
       body)
    (if no-require
        body
      (use-package-with-elapsed-timer
          (format "Loading package %s" name)
        `((if (not ,(use-package-load-name name t))
              (display-warning 'use-package
                               (format "Cannot load %s" ',name)
                               :error)
            ,@body))))))

;;;; Property lists

(defun use-package-plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun use-package-plist-delete-first (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (eq property (car plist))
          (setq p (nconc p (cddr plist))
                plist nil)
        (setq p (nconc p (list (car plist) (cadr plist)))
              plist (cddr plist))))
    p))

(defsubst use-package-plist-maybe-put (plist property value)
  "Add a VALUE for PROPERTY to PLIST, if it does not already exist."
  (if (plist-member plist property)
      plist
    (plist-put plist property value)))

(defsubst use-package-plist-cons (plist property value)
  "Cons VALUE onto the head of the list at PROPERTY in PLIST."
  (plist-put plist property (cons value (plist-get plist property))))

(defsubst use-package-plist-append (plist property value)
  "Append VALUE onto the front of the list at PROPERTY in PLIST."
  (plist-put plist property (append value (plist-get plist property))))

(defun use-package-split-list (pred xs)
  (let ((ys (list nil)) (zs (list nil)) flip)
    (cl-dolist (x xs)
      (if flip
          (nconc zs (list x))
        (if (funcall pred x)
            (progn
              (setq flip t)
              (nconc zs (list x)))
          (nconc ys (list x)))))
    (cons (cdr ys) (cdr zs))))

(defun use-package-split-list-at-keys (key lst)
  (and lst
       (let ((xs (use-package-split-list (apply-partially #'eq key) lst)))
         (cons (car xs) (use-package-split-list-at-keys key (cddr xs))))))

;;;; Keywords

(defun use-package-keyword-index (keyword)
  (cl-loop named outer
           with index = 0
           for k in use-package-keywords do
           (if (eq k keyword)
               (cl-return-from outer index))
           (cl-incf index)))

(defun use-package-normalize-plist (name input &optional plist merge-function)
  "Given a pseudo-plist, normalize it to a regular plist.
The normalized key/value pairs from input are added to PLIST,
extending any keys already present."
  (if (null input)
      plist
    (let* ((keyword (car input))
           (xs (use-package-split-list #'keywordp (cdr input)))
           (args (car xs))
           (tail (cdr xs))
           (normalizer
            (intern-soft (concat "use-package-normalize/"
                                 (symbol-name keyword))))
           (arg (and (functionp normalizer)
                     (funcall normalizer name keyword args)))
           (error-string (format "Unrecognized keyword: %s" keyword)))
      (if (memq keyword use-package-keywords)
          (progn
            (setq plist (use-package-normalize-plist
                         name tail plist merge-function))
            (plist-put plist keyword
                       (if (plist-member plist keyword)
                           (funcall merge-function keyword arg
                                    (plist-get plist keyword))
                         arg)))
        (if use-package-ignore-unknown-keywords
            (progn
              (display-warning 'use-package error-string)
              (use-package-normalize-plist
               name tail plist merge-function))
          (use-package-error error-string))))))

(defun use-package-unalias-keywords (_name args)
  (setq args (cl-nsubstitute :if :when args))
  (let (temp)
    (while (setq temp (plist-get args :unless))
      (setq args (use-package-plist-delete-first args :unless)
            args (append args `(:if (not ,temp))))))
  args)

(defun use-package-merge-keys (key new old)
  (let ((merger (assq key use-package-merge-key-alist)))
    (if merger
        (funcall (cdr merger) new old)
      (append new old))))

(defun use-package-sort-keywords (plist)
  (let (plist-grouped)
    (while plist
      (push (cons (car plist) (cadr plist))
            plist-grouped)
      (setq plist (cddr plist)))
    (let (result)
      (cl-dolist
          (x
           (nreverse
            (sort plist-grouped
                  #'(lambda (l r) (< (use-package-keyword-index (car l))
                                (use-package-keyword-index (car r)))))))
        (setq result (cons (car x) (cons (cdr x) result))))
      result)))

(defun use-package-normalize-keywords (name args)
  (let* ((name-symbol (if (stringp name) (intern name) name))
         (name-string (symbol-name name-symbol)))

    ;; The function `elisp--local-variables' inserts this unbound variable into
    ;; macro forms to determine the locally bound variables for
    ;; `elisp-completion-at-point'.  It ends up throwing a lot of errors since it
    ;; can occupy the position of a keyword (or look like a second argument to a
    ;; keyword that takes one).  Deleting it when it's at the top level should be
    ;; harmless since there should be no locally bound variables to discover
    ;; here anyway.
    (setq args (delq 'elisp--witness--lisp args))

    ;; Reduce the set of keywords down to its most fundamental expression.
    (setq args (use-package-unalias-keywords name-symbol args))

    ;; Normalize keyword values, coalescing multiple occurrences.
    (setq args (use-package-normalize-plist name-symbol args nil
                                            #'use-package-merge-keys))

    ;; Add default values for keywords not specified, when applicable.
    (cl-dolist (spec use-package-defaults)
      (when (let ((func (nth 2 spec)))
              (if (and func (functionp func))
                  (funcall func name args)
                (eval func)))
        (setq args (use-package-plist-maybe-put
                    args (nth 0 spec)
                    (let ((func (nth 1 spec)))
                      (if (and func (functionp func))
                          (funcall func name args)
                        (eval func)))))))

    ;; Determine any autoloads implied by the keywords used.
    (let ((iargs args)
          commands)
      (while iargs
        (when (keywordp (car iargs))
          (let ((autoloads
                 (intern-soft (concat "use-package-autoloads/"
                                      (symbol-name (car iargs))))))
            (when (functionp autoloads)
              (setq commands
                    ;; jww (2017-12-07): Right now we just ignored the type of
                    ;; the autoload being requested, and assume they are all
                    ;; `command'.
                    (append (mapcar
                             #'car
                             (funcall autoloads name-symbol (car iargs)
                                      (cadr iargs)))
                            commands)))))
        (setq iargs (cddr iargs)))
      (when commands
        (setq args
              ;; Like `use-package-plist-append', but removing duplicates.
              (plist-put args :commands
                         (delete-dups
                          (append commands (plist-get args :commands)))))))

    ;; If byte-compiling, pre-load the package so all its symbols are in
    ;; scope.  This is done by prepending statements to the :preface.
    (when (bound-and-true-p byte-compile-current-file)
      (setq args
            (use-package-plist-append
             args :preface
             (use-package-concat
              (mapcar #'(lambda (var) `(defvar ,var))
                      (plist-get args :defines))
              (mapcar #'(lambda (fn) `(declare-function ,fn ,name-string))
                      (plist-get args :functions))
              `((eval-when-compile
                  (with-demoted-errors
                      ,(format "Cannot load %s: %%S" name-string)
                    ,(when (eq use-package-verbose 'debug)
                       `(message ,(format "Compiling package %s" name-string)))
                    ,(unless (plist-get args :no-require)
                       `(unless (featurep ',name-symbol)
                          (load ,name-string nil t))))))))))

    ;; Certain keywords imply :defer, if :demand was not specified.
    (when (and (not (plist-member args :demand))
               (not (plist-member args :defer))
               (not (or (equal '(t) (plist-get args :load))
                        (equal (list (use-package-as-string name))
                               (mapcar #'use-package-as-string
                                       (plist-get args :load)))))
               (cl-some #'identity
                        (mapcar (apply-partially #'plist-member args)
                                use-package-deferring-keywords)))
      (setq args (append args '(:defer t))))

    ;; The :load keyword overrides :no-require
    (when (and (plist-member args :load)
               (plist-member args :no-require))
      (setq args (use-package-plist-delete args :no-require)))

    ;; If at this point no :load, :defer or :no-require has been seen, then
    ;; :load the package itself.
    (when (and (not (plist-member args :load))
               (not (plist-member args :defer))
               (not (plist-member args :no-require)))
      (setq args (append args `(:load (,name)))))

    ;; Sort the list of keywords based on the order of `use-package-keywords'.
    (use-package-sort-keywords args)))

(defun use-package-process-keywords (name plist &optional state)
  "Process the next keyword in the free-form property list PLIST.
The values in the PLIST have each been normalized by the function
use-package-normalize/KEYWORD (minus the colon).

STATE is a property list that the function may modify and/or
query.  This is useful if a package defines multiple keywords and
wishes them to have some kind of stateful interaction.

Unless the KEYWORD being processed intends to ignore remaining
keywords, it must call this function recursively, passing in the
plist with its keyword and argument removed, and passing in the
next value for the STATE."
  (declare (indent 1))
  (unless (null plist)
    (let* ((keyword (car plist))
           (arg (cadr plist))
           (rest (cddr plist)))
      (unless (keywordp keyword)
        (use-package-error (format "%s is not a keyword" keyword)))
      (let* ((handler (concat "use-package-handler/" (symbol-name keyword)))
             (handler-sym (intern handler)))
        (if (functionp handler-sym)
            (funcall handler-sym name keyword arg rest state)
          (use-package-error
           (format "Keyword handler not defined: %s" handler)))))))

(put 'use-package-process-keywords 'lisp-indent-function 'defun)

(defun use-package-list-insert (elem xs &optional anchor after test)
  "Insert ELEM into the list XS.
If ANCHOR is also a keyword, place the new KEYWORD before that
one.
If AFTER is non-nil, insert KEYWORD either at the end of the
keywords list, or after the ANCHOR if one has been provided.
If TEST is non-nil, it is the test used to compare ELEM to list
elements.  The default is `eq'.
The modified list is returned.  The original list is not modified."
  (let (result)
    (dolist (k xs)
      (if (funcall (or test #'eq) k anchor)
          (if after
              (setq result (cons k result)
                    result (cons elem result))
            (setq result (cons elem result)
                  result (cons k result)))
        (setq result (cons k result))))
    (if anchor
        (nreverse result)
      (if after
          (nreverse (cons elem result))
        (cons elem (nreverse result))))))

;;;; Argument Processing

(defun use-package-only-one (label args f)
  "Call F on the first member of ARGS if it has exactly one element."
  (declare (indent 1))
  (cond
   ((and (listp args) (listp (cdr args))
         (= (length args) 1))
    (funcall f label (car args)))
   (t
    (use-package-error
     (concat label " wants exactly one argument")))))

(put 'use-package-only-one 'lisp-indent-function 'defun)

(defun use-package-as-one (label args f &optional allow-empty)
  "Call F on the first element of ARGS if it has one element, or all of ARGS.
If ALLOW-EMPTY is non-nil, it's OK for ARGS to be an empty list."
  (declare (indent 1))
  (if (if args
          (and (listp args) (listp (cdr args)))
        allow-empty)
      (if (= (length args) 1)
          (funcall f label (car args))
        (funcall f label args))
    (use-package-error
     (concat label " wants a non-empty list"))))

(put 'use-package-as-one 'lisp-indent-function 'defun)

(defun use-package-memoize (f arg)
  "Ensure the macro-expansion of F applied to ARG evaluates ARG
no more than once."
  (let ((loaded (cl-gentemp "use-package--loaded"))
        (result (cl-gentemp "use-package--result"))
        (next   (cl-gentemp "use-package--next")))
    `((defvar ,loaded nil)
      (defvar ,result nil)
      (defvar ,next #'(lambda () (if ,loaded ,result
                              (setq ,loaded t ,result ,arg))))
      ,@(funcall f `((funcall ,next))))))

(defsubst use-package-normalize-value (_label arg)
  "Normalize the Lisp value given by ARG.
The argument LABEL is ignored."
  (cond ((null arg) nil)
        ((eq t arg) t)
        ((use-package-non-nil-symbolp arg)
         `(symbol-value ',arg))
        ((functionp arg)
         `(funcall #',arg))
        (t arg)))

(defun use-package-normalize-symbols (label arg &optional recursed)
  "Normalize a list of symbols."
  (cond
   ((use-package-non-nil-symbolp arg)
    (list arg))
   ((and (not recursed) (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x) (car (use-package-normalize-symbols label x t))) arg))
   (t
    (use-package-error
     (concat label " wants a symbol, or list of symbols")))))

(defun use-package-normalize-symlist (_name keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'use-package-normalize-symbols))

(defun use-package-normalize-recursive-symbols (label arg)
  "Normalize a list of symbols."
  (cond
   ((use-package-non-nil-symbolp arg)
    arg)
   ((and (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x) (use-package-normalize-recursive-symbols label x))
            arg))
   (t
    (use-package-error
     (concat label " wants a symbol, or nested list of symbols")))))

(defun use-package-normalize-recursive-symlist (_name keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'use-package-normalize-recursive-symbols))

(defun use-package-normalize-paths (label arg &optional recursed)
  "Normalize a list of filesystem paths."
  (cond
   ((and arg (or (use-package-non-nil-symbolp arg) (functionp arg)))
    (let ((value (use-package-normalize-value label arg)))
      (use-package-normalize-paths label (eval value))))
   ((stringp arg)
    (let ((path (if (file-name-absolute-p arg)
                    arg
                  (expand-file-name arg user-emacs-directory))))
      (list path)))
   ((and (not recursed) (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x)
                (car (use-package-normalize-paths label x t))) arg))
   (t
    (use-package-error
     (concat label " wants a directory path, or list of paths")))))

(defun use-package-normalize-predicate (_name keyword args)
  (if (null args)
      t
    (use-package-only-one (symbol-name keyword) args
      #'use-package-normalize-value)))

(defun use-package-normalize-form (label args)
  "Given a list of forms, return it wrapped in `progn'."
  (unless (listp (car args))
    (use-package-error (concat label " wants a sexp or list of sexps")))
  (mapcar #'(lambda (form)
              (if (and (consp form)
                       (memq (car form)
                             '(use-package
                                use-package-bind-key
                                use-package-bind-key*
                                use-package-unbind-key
                                use-package-bind-keys
                                use-package-bind-keys*)))
                  (macroexpand form)
                form)) args))

(defun use-package-normalize-forms (_name keyword args)
  (use-package-normalize-form (symbol-name keyword) args))

(defun use-package-normalize-pairs
    (key-pred val-pred name label arg &optional recursed)
  "Normalize a list of pairs.
KEY-PRED and VAL-PRED are predicates recognizing valid keys and
values, respectively.
If RECURSED is non-nil, recurse into sublists."
  (cond
   ((funcall key-pred arg)
    (list (cons arg (use-package-as-symbol name))))
   ((use-package-is-pair arg key-pred val-pred)
    (list arg))
   ((and (not recursed) (listp arg) (listp (cdr arg)))
    (let (last-item)
      (mapcar
       #'(lambda (x)
           (prog1
               (let ((ret (use-package-normalize-pairs
                           key-pred val-pred name label x t)))
                 (if (and (listp ret)
                          (not (keywordp last-item)))
                     (car ret)
                   ret))
             (setq last-item x))) arg)))
   (t arg)))

(defun use-package-recognize-function (v &optional binding additional-pred)
  "A predicate that recognizes functional constructions:
  nil
  sym
  'sym
  (quote sym)
  #'sym
  (function sym)
  (lambda () ...)
  '(lambda () ...)
  (quote (lambda () ...))
  #'(lambda () ...)
  (function (lambda () ...))"
  (or (if binding
          (symbolp v)
        (use-package-non-nil-symbolp v))
      (and (listp v)
           (memq (car v) '(quote function))
           (use-package-non-nil-symbolp (cadr v)))
      (if binding (commandp v) (functionp v))
      (and additional-pred
           (funcall additional-pred v))))

(defun use-package-normalize-function (v)
  "Reduce functional constructions to one of two normal forms:
  sym
  #'(lambda () ...)"
  (cond ((symbolp v) v)
        ((and (listp v)
              (memq (car v) '(quote function))
              (use-package-non-nil-symbolp (cadr v)))
         (cadr v))
        ((and (consp v)
              (eq 'lambda (car v)))
         v)
        ((and (listp v)
              (memq (car v) '(quote function))
              (eq 'lambda (car (cadr v))))
         (cadr v))
        (t v)))

(defun use-package-normalize-commands (args)
  "Map over ARGS of the form ((_ . F) ...), normalizing functional F's."
  (mapcar #'(lambda (x)
              (if (consp x)
                  (cons (car x) (use-package-normalize-function (cdr x)))
                x))
          args))

(defun use-package-normalize-mode (name keyword args)
  "Normalize arguments for keywords which add regexp/mode pairs to an alist."
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'use-package-normalize-pairs
                     #'use-package-regex-p
                     #'use-package-recognize-function
                     name)))

(defun use-package-autoloads-mode (_name _keyword args)
  (mapcar
   #'(lambda (x) (cons (cdr x) 'command))
   (cl-remove-if-not #'(lambda (x)
                         (and (consp x)
                              (use-package-non-nil-symbolp (cdr x))))
                     args)))

(defun use-package-handle-mode (name alist args rest state)
  "Handle keywords which add regexp/mode pairs to an alist."
  (use-package-concat
   (use-package-process-keywords name rest state)
   (mapcar
    #'(lambda (thing)
        `(add-to-list
          ',alist
          ',(cons (use-package-normalize-regex (car thing))
                  (cdr thing))))
    (use-package-normalize-commands args))))

;;;; Bind Key

;; Create override-global-mode to force key remappings
(defvar override-global-map (make-keymap)
  "override-global-mode keymap")

(define-minor-mode override-global-mode
  "A minor mode so that keymap settings override other modes."
  t "")

;; the keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists
             `((override-global-mode . ,override-global-map)))

(defvar use-package-describe--personal-keybindings nil
  "List of bindings performed by `use-package-bind-key'.

Elements have the form ((KEY . [MAP]) CMD ORIGINAL-CMD)")

;;;###autoload
(defmacro use-package-bind-key (key-name command &optional keymap predicate)
  "Bind KEY-NAME to COMMAND in KEYMAP (`global-map' if not passed).

KEY-NAME may be a vector, in which case it is passed straight to
`define-key'. Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\". See documentation of
`edmacro-mode' for details.

COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap variable or symbol.
For example:

  (use-package-bind-key \"M-h\" #'some-interactive-function my-mode-map)

  (use-package-bind-key \"M-h\" #'some-interactive-function 'my-mode-map)

If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time."
  (let ((namevar (make-symbol "name"))
        (keyvar (make-symbol "key"))
        (kmapvar (make-symbol "kmap"))
        (kdescvar (make-symbol "kdesc"))
        (bindingvar (make-symbol "binding")))
    `(let* ((,namevar ,key-name)
            (,keyvar (if (vectorp ,namevar) ,namevar
                       (read-kbd-macro ,namevar)))
            (,kmapvar (or (if (and ,keymap (symbolp ,keymap))
                              (symbol-value ,keymap) ,keymap)
                          global-map))
            (,kdescvar (cons (if (stringp ,namevar) ,namevar
                               (key-description ,namevar))
                             (if (symbolp ,keymap) ,keymap (quote ,keymap))))
            (,bindingvar (lookup-key ,kmapvar ,keyvar)))
       (let ((entry (assoc ,kdescvar use-package-describe--personal-keybindings))
             (details (list ,command
                            (unless (numberp ,bindingvar)
                              ,bindingvar))))
         (if entry
             (setcdr entry details)
           (add-to-list 'use-package-describe--personal-keybindings (cons ,kdescvar details))))
       ,(if predicate
            `(define-key ,kmapvar ,keyvar
               '(menu-item "" nil :filter (lambda (&optional _)
                                            (when ,predicate
                                              ,command))))
          `(define-key ,kmapvar ,keyvar ,command)))))

;;;###autoload
(defmacro use-package-unbind-key (key-name &optional keymap)
  "Unbind the given KEY-NAME, within the KEYMAP (if specified).
See `use-package-bind-key' for more details."
  (let ((namevar (make-symbol "name"))
        (kdescvar (make-symbol "kdesc")))
    `(let* ((,namevar ,key-name)
            (,kdescvar (cons (if (stringp ,namevar) ,namevar
                               (key-description ,namevar))
                             (if (symbolp ,keymap) ,keymap (quote ,keymap)))))
       (use-package-bind-key--remove (if (vectorp ,namevar) ,namevar
                                       (read-kbd-macro ,namevar))
                                     (or (if (and ,keymap (symbolp ,keymap))
                                             (symbol-value ,keymap) ,keymap)
                                         global-map))
       (setq use-package-describe--personal-keybindings
             (cl-delete-if (lambda (k) (equal (car k) ,kdescvar))
                           use-package-describe--personal-keybindings))
       nil)))

(defun use-package-bind-key--remove (key keymap)
  "Remove KEY from KEYMAP.

In contrast to `define-key', this function removes the binding from the keymap."
  (define-key keymap key nil)
  ;; Split M-key in ESC key
  (setq key (mapcan (lambda (k)
                      (if (and (integerp k) (/= (logand k ?\M-\0) 0))
                          (list ?\e (logxor k ?\M-\0))
                        (list k)))
                    key))
  ;; Delete single keys directly
  (if (= (length key) 1)
      (delete key keymap)
    ;; Lookup submap and delete key from there
    (let* ((prefix (vconcat (butlast key)))
           (submap (lookup-key keymap prefix)))
      (unless (keymapp submap)
        (error "Not a keymap for %s" key))
      (when (symbolp submap)
        (setq submap (symbol-function submap)))
      (delete (last key) submap)
      ;; Delete submap if it is empty
      (when (= 1 (length submap))
        (use-package-bind-key--remove prefix keymap)))))

;;;###autoload
(defmacro use-package-bind-key* (key-name command &optional predicate)
  "Similar to `use-package-bind-key', but overrides any mode-specific bindings."
  `(use-package-bind-key ,key-name ,command override-global-map ,predicate))

(defun use-package-bind-keys-form (args keymap)
  "Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted)."
  (let (map
        doc
        prefix-map
        prefix
        filter
        menu-name
        pkg)

    ;; Process any initial keyword arguments
    (let ((cont t))
      (while (and cont args)
        (if (cond ((and (eq :map (car args))
                        (not prefix-map))
                   (setq map (cadr args)))
                  ((eq :prefix-docstring (car args))
                   (setq doc (cadr args)))
                  ((and (eq :prefix-map (car args))
                        (not (memq map '(global-map
                                         override-global-map))))
                   (setq prefix-map (cadr args)))
                  ((eq :prefix (car args))
                   (setq prefix (cadr args)))
                  ((eq :filter (car args))
                   (setq filter (cadr args)) t)
                  ((eq :menu-name (car args))
                   (setq menu-name (cadr args)))
                  ((eq :package (car args))
                   (setq pkg (cadr args))))
            (setq args (cddr args))
          (setq cont nil))))

    (when (or (and prefix-map (not prefix))
              (and prefix (not prefix-map)))
      (error "Both :prefix-map and :prefix must be supplied"))

    (when (and menu-name (not prefix))
      (error "If :menu-name is supplied, :prefix must be too"))

    (unless map (setq map keymap))

    ;; Process key binding arguments
    (let (first next)
      (while args
        (if (keywordp (car args))
            (progn
              (setq next args)
              (setq args nil))
          (if first
              (nconc first (list (car args)))
            (setq first (list (car args))))
          (setq args (cdr args))))

      (cl-flet
          ((wrap (map bindings)
                 (if (and map pkg (not (memq map '(global-map
                                                   override-global-map))))
                     `((if (boundp ',map)
                           ,(macroexp-progn bindings)
                         (eval-after-load
                             ,(if (symbolp pkg) `',pkg pkg)
                           ',(macroexp-progn bindings))))
                   bindings)))

        (append
         (when prefix-map
           `((defvar ,prefix-map)
             ,@(when doc `((put ',prefix-map 'variable-documentation ,doc)))
             ,@(if menu-name
                   `((define-prefix-command ',prefix-map nil ,menu-name))
                 `((define-prefix-command ',prefix-map)))
             ,@(if (and map (not (eq map 'global-map)))
                   (wrap map `((use-package-bind-key ,prefix ',prefix-map ,map ,filter)))
                 `((use-package-bind-key ,prefix ',prefix-map nil ,filter)))))
         (wrap map
               (cl-mapcan
                (lambda (form)
                  (let ((fun (and (cdr form) (list 'function (cdr form)))))
                    (if prefix-map
                        `((use-package-bind-key ,(car form) ,fun ,prefix-map ,filter))
                      (if (and map (not (eq map 'global-map)))
                          `((use-package-bind-key ,(car form) ,fun ,map ,filter))
                        `((use-package-bind-key ,(car form) ,fun nil ,filter))))))
                first))
         (when next
           (use-package-bind-keys-form (if pkg
                                           (cons :package (cons pkg next))
                                         next) map)))))))

;;;###autoload
(defmacro use-package-bind-keys (&rest args)
  "Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted)."
  (macroexp-progn (use-package-bind-keys-form args nil)))

;;;###autoload
(defmacro use-package-bind-keys* (&rest args)
  (macroexp-progn (use-package-bind-keys-form args 'override-global-map)))

(defun get-binding-description (elem)
  (cond
   ((listp elem)
    (cond
     ((memq (car elem) '(lambda function))
      (if (and use-package-bind-key-describe-special-forms
               (stringp (nth 2 elem)))
          (nth 2 elem)
        "#<lambda>"))
     ((eq 'closure (car elem))
      (if (and use-package-bind-key-describe-special-forms
               (stringp (nth 3 elem)))
          (nth 3 elem)
        "#<closure>"))
     ((eq 'keymap (car elem))
      "#<keymap>")
     (t
      elem)))
   ;; must be a symbol, non-symbol keymap case covered above
   ((and use-package-bind-key-describe-special-forms (keymapp elem))
    (let ((doc (get elem 'variable-documentation)))
      (if (stringp doc) doc elem)))
   ((symbolp elem)
    elem)
   (t
    "#<byte-compiled lambda>")))

(defun compare-keybindings (l r)
  (let* ((regex use-package-bind-key-segregation-regexp)
         (lgroup (and (string-match regex (caar l))
                      (match-string 0 (caar l))))
         (rgroup (and (string-match regex (caar r))
                      (match-string 0 (caar r))))
         (lkeymap (cdar l))
         (rkeymap (cdar r)))
    (cond
     ((and (null lkeymap) rkeymap)
      (cons t t))
     ((and lkeymap (null rkeymap))
      (cons nil t))
     ((and lkeymap rkeymap
           (not (string= (symbol-name lkeymap) (symbol-name rkeymap))))
      (cons (string< (symbol-name lkeymap) (symbol-name rkeymap)) t))
     ((and (null lgroup) rgroup)
      (cons t t))
     ((and lgroup (null rgroup))
      (cons nil t))
     ((and lgroup rgroup)
      (if (string= lgroup rgroup)
          (cons (string< (caar l) (caar r)) nil)
        (cons (string< lgroup rgroup) t)))
     (t
      (cons (string< (caar l) (caar r)) nil)))))

;;;###autoload
(defun use-package-describe-personal-keybindings ()
  "Display all the personal keybindings defined by `use-package-bind-key'."
  (interactive)
  (with-output-to-temp-buffer "*Personal Keybindings*"
    (princ (format (concat "Key name%s Command%s Comments\n%s %s "
                           "---------------------\n")
                   (make-string (- (car use-package-bind-key-column-widths) 9) ? )
                   (make-string (- (cdr use-package-bind-key-column-widths) 8) ? )
                   (make-string (1- (car use-package-bind-key-column-widths)) ?-)
                   (make-string (1- (cdr use-package-bind-key-column-widths)) ?-)))
    (let (last-binding)
      (dolist (binding
               (setq use-package-describe--personal-keybindings
                     (sort use-package-describe--personal-keybindings
                           (lambda (l r)
                             (car (compare-keybindings l r))))))

        (if (not (eq (cdar last-binding) (cdar binding)))
            (princ (format "\n\n%s: %s\n%s\n\n"
                           (cdar binding) (caar binding)
                           (make-string (+ 21 (car use-package-bind-key-column-widths)
                                           (cdr use-package-bind-key-column-widths)) ?-)))
          (if (and last-binding
                   (cdr (compare-keybindings last-binding binding)))
              (princ "\n")))

        (let* ((key-name (caar binding))
               (at-present (lookup-key (or (symbol-value (cdar binding))
                                           (current-global-map))
                                       (read-kbd-macro key-name)))
               (command (nth 1 binding))
               (was-command (nth 2 binding))
               (command-desc (get-binding-description command))
               (was-command-desc (and was-command
                                      (get-binding-description was-command)))
               (at-present-desc (get-binding-description at-present))
               )
          (let ((line
                 (format
                  (format "%%-%ds%%-%ds%%s\n" (car use-package-bind-key-column-widths)
                          (cdr use-package-bind-key-column-widths))
                  key-name (format "`%s\'" command-desc)
                  (if (string= command-desc at-present-desc)
                      (if (or (null was-command)
                              (string= command-desc was-command-desc))
                          ""
                        (format "was `%s\'" was-command-desc))
                    (format "[now: `%s\']" at-present)))))
            (princ (if (string-match "[ \t]+\n" line)
                       (replace-match "\n" t t line)
                     line))))

        (setq last-binding binding)))))

;;;; Jump

(defun use-package-find-require (package)
  "Find file that required PACKAGE by searching `load-history'.
Returns an absolute file path or nil if none is found."
  (catch 'suspect
    (dolist (filespec load-history)
      (dolist (entry (cdr filespec))
        (when (equal entry (cons 'require package))
          (throw 'suspect (car filespec)))))))

;;;###autoload
(defun use-package-jump-to-package-form (package)
  "Attempt to find and jump to the `use-package' form that loaded
PACKAGE.  This will only find the form if that form actually
required PACKAGE.  If PACKAGE was previously required then this
function will jump to the file that originally required PACKAGE
instead."
  (interactive (list (completing-read "Package: " features)))
  (let* ((package (if (stringp package) (intern package) package))
         (requiring-file (use-package-find-require package))
         file location)
    (if (null requiring-file)
        (user-error "Can't find file requiring file; may have been autoloaded")
      (setq file (if (string= (file-name-extension requiring-file) "elc")
                     (concat (file-name-sans-extension requiring-file) ".el")
                   requiring-file))
      (when (file-exists-p file)
        (find-file-other-window file)
        (save-excursion
          (goto-char (point-min))
          (setq location
                (re-search-forward
                 (format (eval use-package-form-regexp-eval) package) nil t)))
        (if (null location)
            (message "No use-package form found.")
          (goto-char location)
          (beginning-of-line))))))

;;;; Statistics

(defun use-package-reset-statistics ()
  (interactive)
  (setq use-package-statistics (make-hash-table)))

(defun use-package-statistics-status (package)
  "Return loading configuration status of PACKAGE statistics."
  (cond ((gethash :config package)      "Configured")
        ((gethash :init package)        "Initialized")
        ((gethash :preface package)     "Prefaced")
        ((gethash :use-package package) "Declared")))

(defun use-package-statistics-last-event (package)
  "Return the date when PACKAGE's status last changed.
The date is returned as a string."
  (format-time-string "%Y-%m-%d %a %H:%M"
                      (or (gethash :config package)
                          (gethash :init package)
                          (gethash :preface package)
                          (gethash :use-package package))))

(defun use-package-statistics-time (package)
  "Return the time is took for PACKAGE to load."
  (+ (float-time (gethash :config-secs package '(0 0 0 0)))
     (float-time (gethash :init-secs package '(0 0 0 0)))
     (float-time (gethash :preface-secs package '(0 0 0 0)))
     (float-time (gethash :use-package-secs package '(0 0 0 0)))))

(defun use-package-statistics-convert (package)
  "Return information about PACKAGE.

The information is formatted in a way suitable for
`use-package-statistics-mode'."
  (let ((statistics (gethash package use-package-statistics)))
    (list
     package
     (vector
      (symbol-name package)
      (use-package-statistics-status statistics)
      (use-package-statistics-last-event statistics)
      (format "%.2f" (use-package-statistics-time statistics))))))

(defun use-package-report ()
  "Show current statistics gathered about use-package declarations.
In the table that's generated, the status field has the following
meaning:
  Configured        :config has been processed (the package is loaded!)
  Initialized       :init has been processed (load status unknown)
  Prefaced          :preface has been processed
  Declared          the use-package declaration was seen"
  (interactive)
  (with-current-buffer (get-buffer-create "*use-package statistics*")
    (setq tabulated-list-entries
          (mapcar #'use-package-statistics-convert
                  (hash-table-keys use-package-statistics)))
    (use-package-statistics-mode)
    (tabulated-list-print)
    (display-buffer (current-buffer))))

(define-derived-mode use-package-statistics-mode tabulated-list-mode
  "use-package statistics"
  "Show current statistics gathered about use-package declarations."
  (setq tabulated-list-format
        ;; The sum of column width is 80 characters:
        [("Package" 25 t)
         ("Status" 13 t)
         ("Last Event" 23 t)
         ("Time" 10 t)])
  (tabulated-list-init-header))

(defun use-package-statistics-gather (keyword name after)
  (let* ((hash (gethash name use-package-statistics
                        (make-hash-table)))
         (before (and after (gethash keyword hash (current-time)))))
    (puthash keyword (current-time) hash)
    (when after
      (puthash (intern (concat (symbol-name keyword) "-secs"))
               (time-subtract (current-time) before) hash))
    (puthash name hash use-package-statistics)))


;;; Handlers

;;;; :disabled

;; Don't alias this to `ignore', as that will cause the resulting
;; function to be interactive.
(defun use-package-normalize/:disabled (_name _keyword _arg)
  "Do nothing, return nil.")

(defun use-package-handler/:disabled (name _keyword _arg rest state)
  (use-package-process-keywords name rest state))

;;;; :if, :when and :unless

(defun use-package-normalize-test (_name keyword args)
  (use-package-only-one (symbol-name keyword) args
    #'use-package-normalize-value))

(defalias 'use-package-normalize/:if 'use-package-normalize-test)

(defun use-package-handler/:if (name _keyword pred rest state)
  (let ((body (use-package-process-keywords name rest state)))
    `((when ,pred ,@body))))

(defalias 'use-package-normalize/:when 'use-package-normalize-test)

(defalias 'use-package-handler/:when 'use-package-handler/:if)

(defalias 'use-package-normalize/:unless 'use-package-normalize-test)

(defun use-package-handler/:unless (name _keyword pred rest state)
  (let ((body (use-package-process-keywords name rest state)))
    `((unless ,pred ,@body))))

;;;; :requires

(defalias 'use-package-normalize/:requires 'use-package-normalize-symlist)

(defun use-package-handler/:requires (name _keyword requires rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (if (null requires)
        body
      `((when ,(if (> (length requires) 1)
                   `(not (member nil (mapcar #'featurep ',requires)))
                 `(featurep ',(car requires)))
          ,@body)))))

;;;; :pin

(defun use-package-normalize/:pin (_name keyword args)
  (use-package-only-one (symbol-name keyword) args
    #'(lambda (_label arg)
        (cond
         ((stringp arg) arg)
         ((use-package-non-nil-symbolp arg) (symbol-name arg))
         (t
          (use-package-error
           ":pin wants an archive name (a string)"))))))

(eval-when-compile
  (defvar package-pinned-packages)
  (defvar package-archives))

(defun use-package-archive-exists-p (archive)
  "Check if a given ARCHIVE is enabled.

ARCHIVE can be a string or a symbol or 'manual to indicate a
manually updated package."
  (if (member archive '(manual "manual"))
      't
    (let ((valid nil))
      (dolist (pa package-archives)
        (when (member archive (list (car pa) (intern (car pa))))
          (setq valid 't)))
      valid)))

(defun use-package-pin-package (package archive)
  "Pin PACKAGE to ARCHIVE."
  (unless (boundp 'package-pinned-packages)
    (setq package-pinned-packages ()))
  (let ((archive-symbol (if (symbolp archive) archive (intern archive)))
        (archive-name   (if (stringp archive) archive (symbol-name archive))))
    (if (use-package-archive-exists-p archive-symbol)
        (add-to-list 'package-pinned-packages (cons package archive-name))
      (error "Archive '%s' requested for package '%s' is not available"
             archive-name package))
    (unless (bound-and-true-p package--initialized)
      (package-initialize t))))

(defun use-package-handler/:pin (name _keyword archive-name rest state)
  (let ((body (use-package-process-keywords name rest state))
        (pin-form (if archive-name
                      `(use-package-pin-package ',(use-package-as-symbol name)
                                                ,archive-name))))
    ;; Pinning should occur just before ensuring
    ;; See `use-package-handler/:ensure'.
    (if (bound-and-true-p byte-compile-current-file)
        (eval pin-form)              ; Eval when byte-compiling,
      (push pin-form body))          ; or else wait until runtime.
    body))

;;;; :ensure

(defvar package-archive-contents)

;;;###autoload
(defun use-package-normalize/:ensure (_name keyword args)
  (if (null args)
      (list t)
    (use-package-only-one (symbol-name keyword) args
      #'(lambda (_label arg)
          (cond
           ((symbolp arg)
            (list arg))
           ((and (listp arg) (= 3 (length arg))
                 (symbolp (nth 0 arg))
                 (eq :pin (nth 1 arg))
                 (or (stringp (nth 2 arg))
                     (symbolp (nth 2 arg))))
            (list (cons (nth 0 arg) (nth 2 arg))))
           (t
            (use-package-error
             (concat ":ensure wants an optional package name "
                     "(an unquoted symbol name), or (<symbol> :pin <string>)"))))))))

(defun use-package-ensure-elpa (name args _state &optional _no-refresh)
  (dolist (ensure args)
    (let ((package
           (or (and (eq ensure t) (use-package-as-symbol name))
               ensure)))
      (when package
        (require 'package)
        (when (consp package)
          (use-package-pin-package (car package) (cdr package))
          (setq package (car package)))
        (unless (package-installed-p package)
          (condition-case-unless-debug err
              (progn
                (when (assoc package (bound-and-true-p
                                      package-pinned-packages))
                  (package-read-all-archive-contents))
                (if (assoc package package-archive-contents)
                    (package-install package)
                  (package-refresh-contents)
                  (when (assoc package (bound-and-true-p
                                        package-pinned-packages))
                    (package-read-all-archive-contents))
                  (package-install package))
                t)
            (error
             (display-warning 'use-package
                              (format "Failed to install %s: %s"
                                      name (error-message-string err))
                              :error))))))))

;;;###autoload
(defun use-package-handler/:ensure (name _keyword ensure rest state)
  (let* ((body (use-package-process-keywords name rest state)))
    ;; We want to avoid installing packages when the `use-package' macro is
    ;; being macro-expanded by elisp completion (see `lisp--local-variables'),
    ;; but still install packages when byte-compiling, to avoid requiring
    ;; `package' at runtime.
    (if (bound-and-true-p byte-compile-current-file)
        ;; Eval when byte-compiling,
        (funcall use-package-ensure-function name ensure state)
      ;;  or else wait until runtime.
      (push `(,use-package-ensure-function ',name ',ensure ',state)
            body))
    body))

;;;; :load-path

(defun use-package-normalize/:load-path (_name keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'use-package-normalize-paths))

(defun use-package-handler/:load-path (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (path)
                 `(eval-and-compile (add-to-list 'load-path ,path)))
             arg)
     body)))

;;;; :no-require

(defalias 'use-package-normalize/:no-require 'use-package-normalize-predicate)

(defun use-package-handler/:no-require (name _keyword _arg rest state)
  (use-package-process-keywords name rest state))

;;;; :defines

(defalias 'use-package-normalize/:defines 'use-package-normalize-symlist)

(defun use-package-handler/:defines (name _keyword _arg rest state)
  (use-package-process-keywords name rest state))

;;;; :functions

(defalias 'use-package-normalize/:functions 'use-package-normalize-symlist)

(defun use-package-handler/:functions (name _keyword _arg rest state)
  (use-package-process-keywords name rest state))

;;;; :preface

(defalias 'use-package-normalize/:preface 'use-package-normalize-forms)

(defun use-package-handler/:preface (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (when use-package-compute-statistics
       `((use-package-statistics-gather :preface ',name nil)))
     (when arg
       `((eval-and-compile ,@arg)))
     body
     (when use-package-compute-statistics
       `((use-package-statistics-gather :preface ',name t))))))

;;;; :catch

(defvar use-package--form)
(defvar use-package--hush-function #'(lambda (_keyword body) body))

(defsubst use-package-hush (context keyword body)
  `((condition-case-unless-debug err
        ,(macroexp-progn body)
      (error (funcall ,context ,keyword err)))))

(defun use-package-normalize/:catch (_name keyword args)
  (if (null args)
      t
    (use-package-only-one (symbol-name keyword) args
      use-package--hush-function)))

(defun use-package-handler/:catch (name keyword arg rest state)
  (let* ((context (cl-gentemp "use-package--warning")))
    (cond
     ((not arg)
      (use-package-process-keywords name rest state))
     ((eq arg t)
      `((defvar ,context
          #'(lambda (keyword err)
              (let ((msg (format "%s/%s: %s" ',name keyword
                                 (error-message-string err))))
                ,@(when (eq use-package-verbose 'debug)
                    `((with-current-buffer
                          (get-buffer-create "*use-package*")
                        (goto-char (point-max))
                        (insert "-----\n" msg ,use-package--form)
                        (emacs-lisp-mode))
                      (setq msg
                            (concat msg
                                    " (see the *use-package* buffer)"))))
                (display-warning 'use-package msg :error))))
        ,@(let ((use-package--hush-function
                 (apply-partially #'use-package-hush context)))
            (funcall use-package--hush-function keyword
                     (use-package-process-keywords name rest state)))))
     ((functionp arg)
      `((defvar ,context ,arg)
        ,@(let ((use-package--hush-function
                 (apply-partially #'use-package-hush context)))
            (funcall use-package--hush-function keyword
                     (use-package-process-keywords name rest state)))))
     (t
      (use-package-error "The :catch keyword expects 't' or a function")))))

;;;; :interpreter

(defalias 'use-package-normalize/:interpreter 'use-package-normalize-mode)
(defalias 'use-package-autoloads/:interpreter 'use-package-autoloads-mode)

(defun use-package-handler/:interpreter (name _keyword arg rest state)
  (use-package-handle-mode name 'interpreter-mode-alist arg rest state))

;;;; :mode

(defalias 'use-package-normalize/:mode 'use-package-normalize-mode)
(defalias 'use-package-autoloads/:mode 'use-package-autoloads-mode)

(defun use-package-handler/:mode (name _keyword arg rest state)
  (use-package-handle-mode name 'auto-mode-alist arg rest state))

;;;; :magic

(defalias 'use-package-normalize/:magic 'use-package-normalize-mode)
(defalias 'use-package-autoloads/:magic 'use-package-autoloads-mode)

(defun use-package-handler/:magic (name _keyword arg rest state)
  (use-package-handle-mode name 'magic-mode-alist arg rest state))

;;;; :magic-fallback

(defalias 'use-package-normalize/:magic-fallback 'use-package-normalize-mode)
(defalias 'use-package-autoloads/:magic-fallback 'use-package-autoloads-mode)

(defun use-package-handler/:magic-fallback (name _keyword arg rest state)
  (use-package-handle-mode name 'magic-fallback-mode-alist arg rest state))

;;;; :hook

(defun use-package-normalize/:hook (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'(lambda (label arg)
        (unless (or (use-package-non-nil-symbolp arg) (consp arg))
          (use-package-error
           (concat label " a <symbol> or (<symbol or list of symbols> . <symbol or function>)"
                   " or list of these")))
        (use-package-normalize-pairs
         #'(lambda (k)
             (or (use-package-non-nil-symbolp k)
                 (and k (let ((every t))
                          (while (and every k)
                            (if (and (consp k)
                                     (use-package-non-nil-symbolp (car k)))
                                (setq k (cdr k))
                              (setq every nil)))
                          every))))
         #'use-package-recognize-function
         name label arg))))

(defalias 'use-package-autoloads/:hook 'use-package-autoloads-mode)

(defun use-package-handler/:hook (name _keyword args rest state)
  "Generate use-package custom keyword code."
  (use-package-concat
   (use-package-process-keywords name rest state)
   (cl-mapcan
    #'(lambda (def)
        (let ((syms (car def))
              (fun (cdr def)))
          (when fun
            (mapcar
             #'(lambda (sym)
                 `(add-hook
                   (quote ,(intern
                            (concat (symbol-name sym)
                                    use-package-hook-name-suffix)))
                   (function ,fun)))
             (if (use-package-non-nil-symbolp syms) (list syms) syms)))))
    (use-package-normalize-commands args))))

;;;; :commands

(defalias 'use-package-normalize/:commands 'use-package-normalize-symlist)

(defun use-package-handler/:commands (name _keyword arg rest state)
  (use-package-concat
   ;; Since we deferring load, establish any necessary autoloads, and also
   ;; keep the byte-compiler happy.
   (let ((name-string (use-package-as-string name)))
     (cl-mapcan
      #'(lambda (command)
          (when (symbolp command)
            (append
             (unless (plist-get state :demand)
               `((unless (fboundp ',command)
                   (autoload #',command ,name-string nil t))))
             (when (bound-and-true-p byte-compile-current-file)
               `((eval-when-compile
                   (declare-function ,command ,name-string)))))))
      (delete-dups arg)))
   (use-package-process-keywords name rest state)))

;;;; :defer

(defalias 'use-package-normalize/:defer 'use-package-normalize-predicate)

(defun use-package-handler/:defer (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     ;; Load the package after a set amount of idle time, if the argument to
     ;; `:defer' was a number.
     (when (numberp arg)
       `((run-with-idle-timer ,arg nil #'require
                              ',(use-package-as-symbol name) nil t)))
     (if (or (not arg) (null body))
         body
       `((eval-after-load ',name ',(macroexp-progn body)))))))

;;;; :after

(defun use-package-normalize/:after (name keyword args)
  (setq args (use-package-normalize-recursive-symlist name keyword args))
  (if (consp args)
      args
    (list args)))

(defun use-package-after-count-uses (features*)
  "Count the number of time the body would appear in the result."
  (cond ((use-package-non-nil-symbolp features*)
         1)
        ((and (consp features*)
              (memq (car features*) '(:or :any)))
         (let ((num 0))
           (cl-dolist (next (cdr features*))
             (setq num (+ num (use-package-after-count-uses next))))
           num))
        ((and (consp features*)
              (memq (car features*) '(:and :all)))
         (apply #'max (mapcar #'use-package-after-count-uses
                              (cdr features*))))
        ((listp features*)
         (use-package-after-count-uses (cons :all features*)))))

(defun use-package-require-after-load (features* body)
  "Generate `eval-after-load' statements to represents FEATURES*.
FEATURES* is a list containing keywords `:and' and `:all', where
no keyword implies `:all'."
  (cond
   ((use-package-non-nil-symbolp features*)
    `((eval-after-load ',features* ',(macroexp-progn body))))
   ((and (consp features*)
         (memq (car features*) '(:or :any)))
    (cl-mapcan #'(lambda (x) (use-package-require-after-load x body))
               (cdr features*)))
   ((and (consp features*)
         (memq (car features*) '(:and :all)))
    (cl-dolist (next (cdr features*))
      (setq body (use-package-require-after-load next body)))
    body)
   ((listp features*)
    (use-package-require-after-load (cons :all features*) body))))

(defun use-package-handler/:after (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state))
        (uses (use-package-after-count-uses arg)))
    (if (or (null uses) (null body))
        body
      (if (<= uses 1)
          (use-package-require-after-load arg body)
        (use-package-memoize
         (apply-partially #'use-package-require-after-load arg)
         (macroexp-progn body))))))

;;;; :demand

(defalias 'use-package-normalize/:demand 'use-package-normalize-predicate)

(defun use-package-handler/:demand (name _keyword _arg rest state)
  (use-package-process-keywords name rest state))

;;;; :custom

(defun use-package-normalize/:custom (_name keyword args)
  "Normalize use-package custom keyword."
  (use-package-as-one (symbol-name keyword) args
    #'(lambda (label arg)
        (unless (listp arg)
          (use-package-error
           (concat label " a (<symbol> <value> [comment])"
                   " or list of these")))
        (if (use-package-non-nil-symbolp (car arg))
            (list arg)
          arg))))

(defun use-package-handler/:custom (name _keyword args rest state)
  "Generate use-package custom keyword code."
  (use-package-concat
   (if (bound-and-true-p use-package-use-theme)
       `((let ((custom--inhibit-theme-enable nil))
           ;; Declare the theme here so use-package can be required inside
           ;; eval-and-compile without warnings about unknown theme.
           (unless (memq 'use-package custom-known-themes)
             (deftheme use-package)
             (enable-theme 'use-package)
             (setq custom-enabled-themes (remq 'use-package custom-enabled-themes)))
           (custom-theme-set-variables
            'use-package
            ,@(mapcar
               #'(lambda (def)
                   (let ((variable (nth 0 def))
                         (value (nth 1 def))
                         (comment (nth 2 def)))
                     (unless (and comment (stringp comment))
                       (setq comment (format "Customized with use-package %s" name)))
                     `'(,variable ,value nil () ,comment)))
               args))))
     (mapcar
      #'(lambda (def)
          (let ((variable (nth 0 def))
                (value (nth 1 def))
                (comment (nth 2 def)))
            (unless (and comment (stringp comment))
              (setq comment (format "Customized with use-package %s" name)))
            `(customize-set-variable (quote ,variable) ,value ,comment)))
      args))
   (use-package-process-keywords name rest state)))

;;;; :custom-face

(defun use-package-normalize/:custom-face (name-symbol _keyword arg)
  "Normalize use-package custom-face keyword."
  (let ((error-msg
         (format "%s wants a (<symbol> <face-spec>) or list of these"
                 name-symbol)))
    (unless (listp arg)
      (use-package-error error-msg))
    (cl-dolist (def arg arg)
      (unless (listp def)
        (use-package-error error-msg))
      (let ((face (nth 0 def))
            (spec (nth 1 def)))
        (when (or (not face)
                  (not spec)
                  (> (length def) 2))
          (use-package-error error-msg))))))

(defun use-package-handler/:custom-face (name _keyword args rest state)
  "Generate use-package custom-face keyword code."
  (use-package-concat
   (mapcar #'(lambda (def) `(custom-set-faces (backquote ,def))) args)
   (use-package-process-keywords name rest state)))

;;;; Bind Key

;;;###autoload
(defun use-package-autoload-keymap (keymap-symbol package override)
  "Load PACKAGE and then binds the key sequence used to invoke
this function to KEYMAP-SYMBOL.  It then simulates pressing the
same key sequence a again, so that the next key pressed is routed
to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword.  It
works by binding the given key sequence to an invocation of this
function for a particular keymap.  The keymap is expected to be
defined by the package.  In this way, loading the package is
deferred until the prefix key sequence is pressed."
  (if (not (require package nil t))
      (use-package-error (format "Cannot load package.el: %s" package))
    (if (and (boundp keymap-symbol)
             (keymapp (symbol-value keymap-symbol)))
        (let* ((kv (this-command-keys-vector))
               (key (key-description kv))
               (keymap (symbol-value keymap-symbol)))
          (if override
              (use-package-bind-key* key keymap)
            (use-package-bind-key key keymap))
          (setq unread-command-events
                (mapcar (lambda (ev) (cons t ev))
                        (listify-key-sequence kv))))
      (use-package-error
       (format "package.el %s failed to define keymap %s"
               package keymap-symbol)))))

;;;###autoload
(defun use-package-normalize-binder (name keyword args)
  (let ((arg args)
        args*)
    (while arg
      (let ((x (car arg)))
        (cond
         ;; (KEY . COMMAND)
         ((and (consp x)
               (or (stringp (car x))
                   (vectorp (car x)))
               (or (use-package-recognize-function (cdr x) t #'stringp)))
          (setq args* (nconc args* (list x)))
          (setq arg (cdr arg)))
         ;; KEYWORD
         ;;   :map KEYMAP
         ;;   :prefix-docstring STRING
         ;;   :prefix-map SYMBOL
         ;;   :prefix STRING
         ;;   :filter SEXP
         ;;   :menu-name STRING
         ;;   :package SYMBOL
         ((or (and (eq x :map) (symbolp (cadr arg)))
              (and (eq x :prefix) (stringp (cadr arg)))
              (and (eq x :prefix-map) (symbolp (cadr arg)))
              (and (eq x :prefix-docstring) (stringp (cadr arg)))
              (eq x :filter)
              (and (eq x :menu-name) (stringp (cadr arg)))
              (and (eq x :package) (symbolp (cadr arg))))
          (setq args* (nconc args* (list x (cadr arg))))
          (setq arg (cddr arg)))
         ((listp x)
          (setq args*
                (nconc args* (use-package-normalize-binder name keyword x)))
          (setq arg (cdr arg)))
         (t
          ;; Error!
          (use-package-error
           (concat (symbol-name name)
                   " wants arguments acceptable to the `use-package-bind-keys' macro,"
                   " or a list of such values"))))))
    args*))

;;;;; :bind, :bind*

;;;###autoload
(defalias 'use-package-normalize/:bind 'use-package-normalize-binder)
;;;###autoload
(defalias 'use-package-normalize/:bind* 'use-package-normalize-binder)

;; jww (2017-12-07): This is too simplistic.  It will fail to determine
;; autoloads in this situation:
;;   (use-package foo
;;     :bind (:map foo-map (("C-a" . func))))
;;;###autoload
(defalias 'use-package-autoloads/:bind 'use-package-autoloads-mode)
;;;###autoload
(defalias 'use-package-autoloads/:bind* 'use-package-autoloads-mode)

;;;###autoload
(defun use-package-handler/:bind
    (name _keyword args rest state &optional bind-macro)
  (use-package-concat
   (use-package-process-keywords name rest state)
   `(,@(mapcar
        #'(lambda (xs)
            `(,(if bind-macro bind-macro 'use-package-bind-keys)
              :package ,name ,@(use-package-normalize-commands xs)))
        (use-package-split-list-at-keys :break args)))))

(defun use-package-handler/:bind* (name keyword arg rest state)
  (use-package-handler/:bind name keyword arg rest state 'use-package-bind-keys*))

;;;;; :bind-keymap, :bind-keymap*

;;;###autoload
(defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder)
;;;###autoload
(defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder)

;;;###autoload
(defun use-package-handler/:bind-keymap
    (name _keyword args rest state &optional override)
  (use-package-concat
   (use-package-process-keywords name rest state)
   (mapcar
    #'(lambda (binding)
        `(,(if override 'use-package-bind-key* 'use-package-bind-key)
          ,(car binding)
          #'(lambda ()
              (interactive)
              (use-package-autoload-keymap
               ',(cdr binding) ',(use-package-as-symbol name)
               ,override))))
    args)))

;;;###autoload
(defun use-package-handler/:bind-keymap* (name keyword arg rest state)
  (use-package-handler/:bind-keymap name keyword arg rest state t))

;;;; :init

(defalias 'use-package-normalize/:init 'use-package-normalize-forms)

(defun use-package-handler/:init (name _keyword arg rest state)
  (use-package-concat
   (when use-package-compute-statistics
     `((use-package-statistics-gather :init ',name nil)))
   (let ((init-body
          (use-package-hook-injector (use-package-as-string name)
                                     :init arg)))
     (when init-body
       (funcall use-package--hush-function :init
                (if use-package-check-before-init
                    `((when (locate-library ,(use-package-as-string name))
                        ,@init-body))
                  init-body))))
   (use-package-process-keywords name rest state)
   (when use-package-compute-statistics
     `((use-package-statistics-gather :init ',name t)))))

;;;; :load

(defun use-package-normalize/:load (name keyword args)
  (setq args (use-package-normalize-recursive-symlist name keyword args))
  (if (consp args)
      args
    (list args)))

(defun use-package-handler/:load (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (cl-dolist (pkg arg)
      (setq body (use-package-require (if (eq t pkg) name pkg) nil body)))
    body))

;;;; :delight

(defun use-package-normalize-delight (name label arg &optional recursed)
  "Normalize the arguments to delight down to a list of one of two forms:
     SYMBOL
     (SYMBOL . STRING)"
  (cond
   ((not arg)
    (list (use-package-as-mode name)))
   ((use-package-non-nil-symbolp arg)
    (list arg))
   ((stringp arg)
    (list (cons (use-package-as-mode name) arg)))
   ((and (consp arg) (stringp (cdr arg)))
    (list arg))
   ((and (not recursed) (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x) (car (use-package-normalize-delight
                           name label x t))) arg))
   (t
    (use-package-error
     (concat label " wants a string, symbol, "
             "(symbol . string) or list of these")))))

;;;###autoload
(defun use-package-normalize/:delight (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'use-package-normalize-delight name) t))

;;;###autoload
(defun use-package-handler/:delight (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (var)
                 `(if (fboundp 'delight)
                      ,(if (consp var)
                           `(delight ',(car var) ,(cdr var))
                         `(delight ',var))))
             arg)
     body)))

;;;; :config

(defalias 'use-package-normalize/:config 'use-package-normalize-forms)

(defun use-package-handler/:config (name _keyword arg rest state)
  (let* ((body (use-package-process-keywords name rest state))
         (name-symbol (use-package-as-symbol name)))
    (use-package-concat
     (when use-package-compute-statistics
       `((use-package-statistics-gather :config ',name nil)))
     (if (and (or (null arg) (equal arg '(t))) (not use-package-inject-hooks))
         body
       (use-package-with-elapsed-timer
           (format "Configuring package %s" name-symbol)
         (funcall use-package--hush-function :config
                  (use-package-concat
                   (use-package-hook-injector
                    (symbol-name name-symbol) :config arg)
                   body
                   (list t)))))
     (when use-package-compute-statistics
       `((use-package-statistics-gather :config ',name t))))))


;;; The real world

(defmacro use-package-core (name args)
  (declare (indent defun))
  `(let* ((args* (use-package-normalize-keywords ,name ,args))
          (use-package--form
           (if (eq use-package-verbose 'debug)
               (concat "\n\n"
                       (pp-to-string `(use-package ,name ,@,args))
                       "\n  -->\n\n"
                       (pp-to-string `(use-package ,name ,@args*))
                       "\n  ==>\n\n"
                       (pp-to-string
                        (macroexp-progn
                         (let ((use-package-verbose 'errors)
                               (use-package-expand-minimally t))
                           (use-package-process-keywords name args*
                             (and (plist-get args* :demand)
                                  (list :demand t)))))))
             "")))
     (use-package-process-keywords name args*
       (and (plist-get args* :demand)
            (list :demand t)))))

;;;###autoload
(defmacro use-package (name &rest args)
  "Declare an Emacs package by specifying a group of configuration options.

For full documentation, please see the README file that came with
this file.  Usage:

  (use-package package-name
     [:keyword [option]]...)

:init            Code to run before PACKAGE-NAME has been loaded.
:config          Code to run after PACKAGE-NAME has been loaded.  Note that
                 if loading is deferred for any reason, this code does not
                 execute until the lazy load has occurred.
:preface         Code to be run before everything except `:disabled'; this
                 can be used to define functions for use in `:if', or that
                 should be seen by the byte-compiler.

:mode            Form to be added to `auto-mode-alist'.
:magic           Form to be added to `magic-mode-alist'.
:magic-fallback  Form to be added to `magic-fallback-mode-alist'.
:interpreter     Form to be added to `interpreter-mode-alist'.

:commands        Define autoloads for commands that will be defined by the
                 package.  This is useful if the package is being lazily
                 loaded, and you wish to conditionally call functions in your
                 `:init' block that are defined in the package.
:hook            Specify hook(s) to attach this package to.

:bind            Bind keys, and define autoloads for the bound commands.
:bind*           Bind keys, and define autoloads for the bound commands,
                 *overriding all minor mode bindings*.
:bind-keymap     Bind a key prefix to an auto-loaded keymap defined in the
                 package.  This is like `:bind', but for keymaps.
:bind-keymap*    Like `:bind-keymap', but overrides all minor mode bindings

:defer           Defer loading of a package -- this is implied when using
                 `:commands', `:bind', `:bind*', `:mode', `:magic', `:hook',
                 `:magic-fallback', or `:interpreter'.  This can be an integer,
                 to force loading after N seconds of idle time, if the package
                 has not already been loaded.
:after           Delay the use-package declaration until after the named modules
                 have loaded.  Once load, it will be as though the use-package
                 declaration (without `:after') had been seen at that moment.
:demand          Prevent the automatic deferred loading introduced by constructs
                 such as `:bind' (see `:defer' for the complete list).

:if EXPR         Initialize and load only if EXPR evaluates to a non-nil value.
:disabled        The package is ignored completely if this keyword is present.
:defines         Declare certain variables to silence the byte-compiler.
:functions       Declare certain functions to silence the byte-compiler.
:load-path       Add to the `load-path' before attempting to load the package.

:custom          Call `custom-set' or `set-default' with each variable
                 definition without modifying the Emacs `custom-file'.
                 (compare with `custom-set-variables').
:custom-face     Call `customize-set-faces' with each face definition.

:delight         Delight the mode line by remove distracting lighter.

:ensure          Load the package using package.el if necessary.
:pin             Pin the package to an archive."
  (declare (indent 1))
  (unless (memq :disabled args)
    (macroexp-progn
     (use-package-concat
      (when use-package-compute-statistics
        `((use-package-statistics-gather :use-package ',name nil)))
      (if (eq use-package-verbose 'errors)
          (use-package-core name args)
        (condition-case-unless-debug err
            (use-package-core name args)
          (error
           (ignore
            (display-warning
             'use-package
             (format "Failed to parse package %s: %s"
                     name (error-message-string err)) :error)))))
      (when use-package-compute-statistics
        `((use-package-statistics-gather :use-package ',name t)))))))

(put 'use-package 'lisp-indent-function 'defun)

(provide 'use-package)

;; Local Variables:
;; coding: utf-8
;; End:

;;; use-package.el ends here
