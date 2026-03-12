;;; org-linker-edna.el --- Link things in orgmode          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (org "9.0") (org-linker "0.1") (org-edna "1.0"))
;; URL: https://github.com/toshism/org-linker-edna
;; Keywords: convenience, hypermedia

;; This program is free software; you can redistribute it and/or modify
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

;; Link things in org using org-edna for task dependencies.
;; Creates BLOCKER and TRIGGER properties for automatic state changes.

;;; Code:

(require 'org-linker)

;; Silence byte compiler — these are available at runtime via org
(declare-function org-entry-get "ext:org")
(declare-function org-entry-put "ext:org")
(declare-function org-read-date "ext:org")
(declare-function org-id-get-create "ext:org-id")
(defvar org-todo-keywords-1)

(defcustom org-linker-edna-id-function #'org-id-get-create
  "Function to get or create an ID for an Org heading.
Called with no arguments at the heading."
  :type 'function
  :group 'org-linker)

(defvar org-linker-edna-actions '("scheduled" "deadline" "todo")
  "Available trigger actions for org-linker-edna.")


(defun org-linker-edna-ids (s)
  "Return a list of ids found in S.
S is a string formatted as org edna ids property value."
  (when s
    (when (string-match "ids(\\([^\\)]*\\)).*" s)
      (split-string (match-string 1 s)))))


(defun org-linker-edna-get-or-create-id-for-marker (m)
  "Get or create an ID for the heading at marker M."
  (with-current-buffer (marker-buffer m)
    (save-excursion
      (goto-char (marker-position m))
      (funcall org-linker-edna-id-function))))


(defun org-linker-edna-set-prop (source target property)
  "Return an ID list string for PROPERTY, appending SOURCE's ID to TARGET's existing IDs."
  (let* ((existing-ids (org-linker-edna-ids
                        (org-entry-get (marker-position target) property)))
         (source-id (concat "\"id:" (org-linker-edna-get-or-create-id-for-marker source) "\""))
         (all-ids (if (member source-id existing-ids)
                      existing-ids
                    (append existing-ids (list source-id)))))
    (concat "(" (mapconcat #'identity all-ids " ") ")")))


(defun org-linker-edna-set-blocker (source target)
  "Set BLOCKER property on TARGET with SOURCE's ID."
  (org-entry-put (marker-position target) "BLOCKER"
                 (format "ids%s" (org-linker-edna-set-prop source target "BLOCKER"))))


(defun org-linker-edna-date-selector (type)
  "Prompt for a date using the Org date picker for TYPE (e.g. \"scheduled\")."
  (org-read-date nil nil nil (format "%s: " (capitalize type))))


(defun org-linker-edna-state-selector ()
  "Select a TODO state using `completing-read'."
  (completing-read "TODO state: " org-todo-keywords-1 nil t))


(defun org-linker-edna-action-dispatcher (candidate)
  "Dispatch a single action CANDIDATE, prompting for its value."
  (cond ((string= "scheduled" candidate) `(:scheduled ,(org-linker-edna-date-selector "scheduled")))
	((string= "deadline" candidate) `(:deadline ,(org-linker-edna-date-selector "deadline")))
	((string= "todo" candidate) `(:todo ,(org-linker-edna-state-selector)))))


(defun org-linker-edna-actions-dispatcher ()
  "Select trigger actions using `completing-read-multiple' and collect their values."
  (let ((selected (completing-read-multiple "Trigger actions: " org-linker-edna-actions)))
    (mapcan #'org-linker-edna-action-dispatcher selected)))


(defun org-linker-edna-set-trigger (source target)
  "Set TRIGGER property on TARGET, adding SOURCE as ID with selected actions."
  (let* ((actions (org-linker-edna-actions-dispatcher))
         (todo (plist-get actions :todo))
         (scheduled (plist-get actions :scheduled))
         (deadline (plist-get actions :deadline))
         (todo-state (when todo (concat " todo!(" todo ")")))
         (scheduled-date (when scheduled (concat " scheduled!(\"" scheduled "\")")))
         (deadline-date (when deadline (concat " deadline!(\"" deadline "\")")))
         (existing-trigger (org-entry-get (marker-position target) "TRIGGER"))
         (id-string (org-linker-edna-set-prop source target "TRIGGER")))
    (org-entry-put (marker-position target) "TRIGGER"
                   (string-trim
                    (concat existing-trigger
                            (format " ids%s" id-string)
                            todo-state scheduled-date deadline-date)))))


(defun org-linker-edna-callback (source target)
  "Set blocker on TARGET and trigger on SOURCE for edna dependency."
  (org-linker-edna-set-blocker source target)
  (org-linker-edna-set-trigger target source))


;;;###autoload
(defun org-linker-edna ()
  "Interactively create an org-edna dependency between two headings."
  (interactive)
  (org-linker 'org-linker-edna-callback))


(provide 'org-linker-edna)

;;; org-linker-edna.el ends here
