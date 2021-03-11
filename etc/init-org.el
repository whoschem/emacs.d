;;; init-org.el --- Org-mode configuration  -*- lexical-binding: t  -*-

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

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :custom
  ((org-log-done t)
   (org-edit-timestamp-down-means-later t)
   (org-hide-emphasis-markers t)
   (org-catch-invisible-edits 'show)
   (org-export-coding-system 'utf-8)
   (org-fast-tag-selection-single-key 'expert)
   (org-support-shift-select t)
   (org-html-validation-link nil)
   (org-export-kill-product-buffer-when-displayed t)
   (org-tags-column 80)

   (org-archive-location "%s_archive::* Archive")
   (org-archive-mark-done nil)

   (org-capture-templates '(("t" "todo" entry (file "")
                             "* NEXT %?\n%U\n" :clock-resume t)
                            ("n" "note" entry (file "")
                             "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
                            ))

   (org-outline-path-complete-in-steps nil)
   (org-refile-allow-creating-parent-nodes 'confirm)
   (org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
   (org-refile-use-cache nil)
   (org-refile-use-outline-path t))

  :config
  (org-clock-persistence-insinuate)

  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  (defun org-refile-anywhere (&optional goto default-buffer rfloc msg)
    "A version of `org-refile' which allows refiling to any subtree."
    (interactive "P")
    (let ((org-refile-target-verify-function))
      (org-refile goto default-buffer rfloc msg)))

  (defun org-agenda-refile-anywhere (&optional goto rfloc no-update)
    "A version of `org-agenda-refile' which allows refiling to any subtree."
    (interactive "P")
    (let ((org-refile-target-verify-function))
      (org-agenda-refile goto rfloc no-update)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   `((C . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (latex . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (shell . t)
     (sqlite . t)))

  ;; Export widget implemented in Hydra

  (defhydradio hydra-ox ()
    (body-only "Export only the body.")
    (export-scope "Export scope." [buffer subtree])
    (async-export "When non-nil, export async.")
    (visible-only "When non-nil, export visible only")
    (force-publishing "Toggle force publishing"))

  (defhydra +hydra-ox-html (:color blue)
    "ox-html"
    ("H" (org-html-export-as-html
          hydra-ox/async-export
          (eq hydra-ox/export-scope 'subtree)
          hydra-ox/visible-only
          hydra-ox/body-only)
     "As HTML buffer")
    ("h" (org-html-export-to-html
          hydra-ox/async-export
          (eq hydra-ox/export-scope 'subtree)
          hydra-ox/visible-only
          hydra-ox/body-only) "As HTML file")
    ("o" (org-open-file
          (org-html-export-to-html
           hydra-ox/async-export
           (eq hydra-ox/export-scope 'subtree)
           hydra-ox/visible-only
           hydra-ox/body-only)) "As HTML file and open")
    ("b" +hydra-ox/body "back")
    ("q" nil "quit"))

  (defhydra +hydra-ox-latex (:color blue)
    "ox-latex"
    ("L" org-latex-export-as-latex "As LaTeX buffer")
    ("l" org-latex-export-to-latex "As LaTeX file")
    ("p" org-latex-export-to-pdf "As PDF file")
    ("o" (org-open-file (org-latex-export-to-pdf)) "As PDF file and open")
    ("b" +hydra-ox/body "back")
    ("q" nil "quit"))

  (defhydra +hydra-ox-text (:color blue)
    "ox-text"
    ("A" (org-ascii-export-as-ascii
          nil nil nil nil
          '(:ascii-charset ascii))
     "As ASCII buffer")

    ("a" (org-ascii-export-to-ascii
          nil nil nil nil
          '(:ascii-charset ascii))
     "As ASCII file")
    ("L" (org-ascii-export-as-ascii
          nil nil nil nil
          '(:ascii-charset latin1))
     "As Latin1 buffer")
    ("l" (org-ascii-export-to-ascii
          nil nil nil nil
          '(:ascii-charset latin1))
     "As Latin1 file")
    ("U" (org-ascii-export-as-ascii
          nil nil nil nil
          '(:ascii-charset utf-8))
     "As UTF-8 buffer")
    ("u" (org-ascii-export-to-ascii
          nil nil nil nil
          '(:ascii-charset utf-8))
     "As UTF-8 file")
    ("b" +hydra-ox/body "back")
    ("q" nil "quit"))

  (defhydra +hydra-ox ()
    "
_C-b_ Body only:    % -15`hydra-ox/body-only^^^ _C-v_ Visible only:     %`hydra-ox/visible-only
_C-s_ Export scope: % -15`hydra-ox/export-scope _C-f_ Force publishing: %`hydra-ox/force-publishing
_C-a_ Async export: %`hydra-ox/async-export
"
    ("C-b" (hydra-ox/body-only) nil)
    ("C-v" (hydra-ox/visible-only) nil)
    ("C-s" (hydra-ox/export-scope) nil)
    ("C-f" (hydra-ox/force-publishing) nil)
    ("C-a" (hydra-ox/async-export) nil)
    ("h" +hydra-ox-html/body "Export to HTML" :exit t)
    ("l" +hydra-ox-latex/body "Export to LaTeX" :exit t)
    ("t" +hydra-ox-text/body "Export to Plain Text" :exit t)
    ("q" nil "quit"))

  (define-key org-mode-map (kbd "C-c C-,") '+hydra-ox/body))


;;; Agenda views

(use-package org-agenda
  :after org
  :custom
  ((org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
   (org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                              (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))
   (org-todo-repeat-to-state "NEXT")
   (org-todo-keyword-faces (quote (("NEXT" :inherit warning)
                                   ("PROJECT" :inherit font-lock-string-face)))))
  :hook (org-agenda-mode . hl-line-mode)
  :config
  (let ((active-project-match "-INBOX/PROJECT"))
    (setq org-stuck-projects
          `(,active-project-match ("NEXT")))
    (setq org-agenda-compact-blocks t
          org-agenda-sticky t
          org-agenda-start-on-weekday nil
          org-agenda-span 'day
          org-agenda-include-diary nil
          org-agenda-sorting-strategy
          '((agenda habit-down time-up user-defined-up effort-up category-keep)
            (todo category-up effort-up)
            (tags category-up effort-up)
            (search category-up))
          org-agenda-window-setup 'current-window
          org-agenda-custom-commands
          `(("N" "Notes" tags "NOTE"
             ((org-agenda-overriding-header "Notes")
              (org-tags-match-list-sublevels t)))
            ("g" "GTD"
             ((agenda "" nil)
              (tags "INBOX"
                    ((org-agenda-overriding-header "Inbox")
                     (org-tags-match-list-sublevels nil)))
              (stuck ""
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled 'future)))
              (tags-todo "-INBOX"
                         ((org-agenda-overriding-header "Next Actions")
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-skip-function
                           '(lambda ()
                              (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                  (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                          (org-tags-match-list-sublevels t)
                          (org-agenda-sorting-strategy
                           '(todo-state-down effort-up category-keep))))
              (tags-todo ,active-project-match
                         ((org-agenda-overriding-header "Projects")
                          (org-tags-match-list-sublevels t)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-INBOX/-NEXT"
                         ((org-agenda-overriding-header "Orphaned Tasks")
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-skip-function
                           '(lambda ()
                              (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING"))
                                  (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                          (org-tags-match-list-sublevels t)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "/WAITING"
                         ((org-agenda-overriding-header "Waiting")
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-INBOX"
                         ((org-agenda-overriding-header "On Hold")
                          (org-agenda-skip-function
                           '(lambda ()
                              (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                  (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                          (org-tags-match-list-sublevels nil)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              )))))

  (add-to-list 'org-agenda-after-show-hook 'org-show-entry)

  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t)))

  ;; Org agenda view
  (defun org-agenda-cts ()
    (and (eq major-mode 'org-agenda-mode)
         (let ((args (get-text-property
                      (min (1- (point-max)) (point))
                      'org-last-args)))
           (nth 2 args))))

  (defhydra +hydra-org-agenda-view (:hint none)
    "
_d_: ?d? day        _g_: time grid=?g?  _a_: arch-trees
_w_: ?w? week       _[_: inactive       _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?     _r_: clock report=?r?
_m_: ?m? month      _e_: entry text=?e? _D_: include diary=?D?
_y_: ?y? year       _q_: quit           _L__l__c_: log = ?l?"
    ("SPC" org-agenda-reset-view)
    ("d" org-agenda-day-view (if (eq 'day (org-agenda-cts)) "[x]" "[ ]"))
    ("w" org-agenda-week-view (if (eq 'week (org-agenda-cts)) "[x]" "[ ]"))
    ("t" org-agenda-fortnight-view (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]"))
    ("m" org-agenda-month-view (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
    ("y" org-agenda-year-view (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
    ("l" org-agenda-log-mode (format "% -3S" org-agenda-show-log))
    ("L" (org-agenda-log-mode '(4)))
    ("c" (org-agenda-log-mode 'clockcheck))
    ("f" org-agenda-follow-mode (format "% -3S" org-agenda-follow-mode))
    ("a" org-agenda-archives-mode)
    ("A" (org-agenda-archives-mode 'files))
    ("r" org-agenda-clockreport-mode (format "% -3S" org-agenda-clockreport-mode))
    ("e" org-agenda-entry-text-mode (format "% -3S" org-agenda-entry-text-mode))
    ("g" org-agenda-toggle-time-grid (format "% -3S" org-agenda-use-time-grid))
    ("D" org-agenda-toggle-diary (format "% -3S" org-agenda-include-diary))
    ("!" org-agenda-toggle-deadlines)
    ("[" (let ((org-agenda-include-inactive-timestamps t))
           (org-agenda-check-type t 'timeline 'agenda)
           (org-agenda-redo)
           (message "Display now includes inactive timestamps as well")))
    ("q" (message "Abort") :exit t)
    ("v" nil))

  (define-key org-agenda-mode-map "v" '+hydra-org-agenda-view/body))


;;; Org clock

(use-package org-clock
  :after org
  :bind ((:map org-clock-mode-line-map
               ([header-line mouse-2] . org-clock-goto)
               ([header-line mouse-1] . org-clock-menu)))
  :custom
  ((org-clock-in-resume t)
   (org-clock-into-drawer t)
   (org-clock-out-remove-zero-time-clocks t)
   (org-clock-persist t)
   (org-clock-persist-file ,(expand-file-name ".org-clock-save" user-emacs-directory))
   (org-log-into-drawer t)
   (org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
  :config
  (defun show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))

  (defun hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))

  (add-hook 'org-clock-in-hook 'show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook 'hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook 'hide-org-clock-from-header-line))

(provide 'init-org)

;;; init-org.el ends here
