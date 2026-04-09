;;; org-linker-edna.el --- Link things in orgmode          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (org "9.0") (org-linker "0.1") (org-edna "1.0") (transient "0.4"))
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
;; Provides a transient menu (C-c e) for dependency management.

;;; Code:

(require 'org-linker)
(require 'cl-lib)

;; Silence byte compiler — available at runtime via org
(declare-function org-entry-get "ext:org")
(declare-function org-entry-put "ext:org")
(declare-function org-entry-delete "ext:org")
(declare-function org-read-date "ext:org")
(declare-function org-id-get-create "ext:org-id")
(declare-function org-id-find "ext:org-id")
(declare-function org-back-to-heading "ext:org")
(declare-function org-get-heading "ext:org")
(declare-function org-agenda-files "ext:org")
(declare-function org-reveal "ext:org")
(declare-function org-current-level "ext:org")
(declare-function org-map-entries "ext:org")
(declare-function org-ql-select "ext:org-ql")
(eval-when-compile (require 'org-macs))
(defvar org-todo-keywords-1)
(defvar embark-org-heading-map)


;;; Customization

(defcustom org-linker-edna-id-function #'org-id-get-create
  "Function to get or create an ID for an Org heading.
Called with no arguments at the heading."
  :type 'function
  :group 'org-linker)


;;; Action definitions

(defvar org-linker-edna-actions
  '("todo" "scheduled" "deadline"
    "tag" "set-property" "delete-property"
    "set-priority" "set-effort"
    "clock-in" "clock-out" "archive" "chain")
  "All available org-edna trigger actions.
Common actions are listed first.")


;;; Finder definitions

(defvar org-linker-edna-finders
  '(;; Relative — tree navigation (no args needed)
    ("self"                  :type relative)
    ("parent"                :type relative)
    ("children"              :type relative)
    ("first-child"           :type relative)
    ("siblings"              :type relative)
    ("next-sibling"          :type relative)
    ("previous-sibling"      :type relative)
    ("next-sibling-wrap"     :type relative)
    ("previous-sibling-wrap" :type relative)
    ("rest-of-siblings"      :type relative)
    ("rest-of-siblings-wrap" :type relative)
    ("ancestors"             :type relative)
    ("descendants"           :type relative)
    ;; Absolute — need arguments
    ("match"    :type absolute :prompt match)
    ("olp"      :type absolute :prompt olp)
    ("file"     :type absolute :prompt file)
    ("org-file" :type absolute :prompt org-file))
  "Available org-edna finders with their types.")

(defvar org-linker-edna-finder-options
  '("todo-only" "todo-and-done-only" "no-comment" "no-archive")
  "Filter options available for relative finders.")

(defvar org-linker-edna-blocker-conditions
  '("(default: all DONE)" "done?" "!done?"
    "todo-state?" "!todo-state?"
    "has-property?" "!has-property?"
    "has-tags?" "!has-tags?")
  "Available blocker condition keywords.")


;;; Core helpers

(defun org-linker-edna-ids (s)
  "Return a list of ids found in S.
S is a string formatted as org edna ids property value."
  (when s
    (when (string-match "ids(\\([^)]*\\)).*" s)
      (split-string (match-string 1 s)))))

(defun org-linker-edna-get-or-create-id-for-marker (m)
  "Get or create an ID for the heading at marker M."
  (with-current-buffer (marker-buffer m)
    (save-excursion
      (goto-char (marker-position m))
      (funcall org-linker-edna-id-function))))

(defun org-linker-edna--heading-at-marker (m)
  "Return the heading text at marker M."
  (with-current-buffer (marker-buffer m)
    (save-excursion
      (goto-char (marker-position m))
      (org-get-heading t t t t))))

(defun org-linker-edna--collect-ids-with-headings (property)
  "Collect ID references from PROPERTY at point with heading text.
Returns alist of (DISPLAY-STRING . ID-STRING) for completing-read."
  (let* ((prop-val (org-entry-get (point) property))
         (ids (org-linker-edna-ids prop-val))
         result)
    (dolist (id-str ids (nreverse result))
      (let* ((bare-id (replace-regexp-in-string
                       "^\"id:\\|\"$" "" id-str))
             (marker (org-id-find bare-id 'marker))
             (heading (if marker
                         (with-current-buffer (marker-buffer marker)
                           (save-excursion
                             (goto-char (marker-position marker))
                             (org-get-heading t t t t)))
                       (format "[missing: %s]" bare-id))))
        (push (cons (format "%s  (%s)" heading bare-id) id-str) result)))))


;;; Property setters

(defun org-linker-edna-set-prop (source target property)
  "Return an ID list string for PROPERTY.
Appends SOURCE's ID to TARGET's existing IDs, deduplicating."
  (let* ((existing-ids (org-linker-edna-ids
                        (org-entry-get target property)))
         (source-id (concat "\"id:" (org-linker-edna-get-or-create-id-for-marker source) "\""))
         (all-ids (if (member source-id existing-ids)
                      existing-ids
                    (append existing-ids (list source-id)))))
    (concat "(" (mapconcat #'identity all-ids " ") ")")))

(defun org-linker-edna-set-blocker (source target)
  "Append SOURCE's ID to BLOCKER on TARGET, preserving all existing tokens.
Deduplicates: if the ID is already present it is not added again."
  (let* ((existing (org-entry-get target "BLOCKER"))
         (existing-ids (org-linker-edna-ids existing))
         (source-id (concat "\"id:" (org-linker-edna-get-or-create-id-for-marker source) "\""))
         (new-ids (if (member source-id existing-ids)
                      existing-ids
                    (append existing-ids (list source-id))))
         (ids-block (concat "ids(" (mapconcat #'identity new-ids " ") ")"))
         (rest (when existing
                 (string-trim
                  (replace-regexp-in-string "ids([^)]*)" "" existing))))
         (parts (delq nil (list ids-block
                                (when (and rest (not (string-blank-p rest))) rest))))
         (new-val (string-trim (mapconcat #'identity parts " "))))
    (org-entry-put target "BLOCKER" new-val)))


;;; Finder selection helpers

(defun org-linker-edna--select-finder ()
  "Prompt user to select an org-edna finder.
Returns a plist (:finder NAME :args-str STRING :type TYPE)."
  (let* ((finder-names (mapcar #'car org-linker-edna-finders))
         (finder (completing-read "Finder: " finder-names nil t))
         (finder-def (assoc finder org-linker-edna-finders))
         (type (plist-get (cdr finder-def) :type))
         (prompt-type (plist-get (cdr finder-def) :prompt))
         (args-str (org-linker-edna--prompt-finder-args prompt-type)))
    (list :finder finder :args-str args-str :type type)))

(defun org-linker-edna--prompt-finder-args (prompt-type)
  "Prompt for finder arguments based on PROMPT-TYPE.
Returns formatted argument string, or empty string if none needed."
  (pcase prompt-type
    ('match
     (let ((spec (read-string
                  "Match spec (e.g. TODO=\"TODO\"+project): ")))
       (format "(\"%s\")" spec)))
    ('olp
     (let ((file (read-file-name "Org file: "))
           (path (read-string "Outline path (e.g. Projects/Sub): ")))
       (format "(\"%s\" \"%s\")" file path)))
    ('file
     (let ((file (read-file-name "File: ")))
       (format "(\"%s\")" file)))
    ('org-file
     (let ((file (read-string "File (relative to org-directory): ")))
       (format "(\"%s\")" file)))
    (_ "")))

(defun org-linker-edna--select-finder-options ()
  "Prompt for optional filter/sort options for relative finders.
Returns formatted options string (with leading space), or empty."
  (let* ((opts (completing-read-multiple
                "Filter options (optional, comma-separated): "
                org-linker-edna-finder-options))
         (tag-input (read-string
                     "Tag filter (+tag or -tag, empty to skip): "))
         (all-opts (append opts
                          (when (not (string-blank-p tag-input))
                            (list tag-input)))))
    (if all-opts
        (concat " " (mapconcat #'identity all-opts " "))
      "")))

(defun org-linker-edna--select-blocker-condition ()
  "Prompt for an optional blocker condition.
Returns formatted condition string (with leading space), or empty."
  (let ((condition (completing-read "Blocker condition: "
                                    org-linker-edna-blocker-conditions
                                    nil t)))
    (pcase condition
      ("(default: all DONE)" "")
      ((or "todo-state?" "!todo-state?")
       (let ((state (completing-read "State: "
                                     org-todo-keywords-1 nil t)))
         (format " %s(%s)" condition state)))
      ((or "has-property?" "!has-property?")
       (let ((prop (read-string "Property: "))
             (val (read-string "Value: ")))
         (format " %s(\"%s\" \"%s\")" condition prop val)))
      ((or "has-tags?" "!has-tags?")
       (let ((tags (read-string "Tags (space-separated): ")))
         (format " %s(%s)" condition tags)))
      (_ (format " %s" condition)))))

(defun org-linker-edna--select-consideration ()
  "Prompt for optional consideration modifier for blockers.
Returns formatted string (with leading space), or empty."
  (let ((choice (completing-read
                 "Consideration: "
                 '("(default)" "any" "all" "number") nil t)))
    (pcase choice
      ("(default)" "")
      ("number"
       (let ((n (read-number "Minimum count: ")))
         (format " consider(%d)" n)))
      (_ (format " consider(%s)" choice)))))

(defun org-linker-edna--build-finder-string (finder-info
                                             &optional options-str)
  "Build the finder portion of an edna property string.
FINDER-INFO is a plist from `org-linker-edna--select-finder'.
OPTIONS-STR is an optional filter/sort options string."
  (let ((finder (plist-get finder-info :finder))
        (args (plist-get finder-info :args-str)))
    (concat finder args (or options-str ""))))


;;; Relatives builder

(defvar org-linker-edna--rel-selections
  '("from-top" "from-bottom" "from-current"
    "forward-no-wrap" "backward-no-wrap" "backward-wrap"
    "walk-up" "walk-up-with-self"
    "walk-down" "walk-down-with-self" "step-down")
  "Selection modes for the relatives finder.")

(defvar org-linker-edna--rel-sorts
  '("(none)" "random-sort"
    "priority-up" "priority-down"
    "effort-up" "effort-down"
    "scheduled-up" "scheduled-down"
    "deadline-up" "deadline-down"
    "timestamp-up" "timestamp-down")
  "Sort options for the relatives finder.")

(defun org-linker-edna--build-relatives-string ()
  "Interactively build a relatives() finder string.
Guides through selection, filtering, and sorting phases.
Returns the complete relatives(...) string."
  ;; Phase 1: Selection mode
  (let* ((selection (completing-read "Selection mode: "
                                     org-linker-edna--rel-selections nil t))
         ;; Phase 2: Filters
         (filters (completing-read-multiple
                   "Filters (optional): "
                   org-linker-edna-finder-options))
         (tag-input (read-string
                     "Tag filter (+tag or -tag, empty to skip): "))
         (regex-input (read-string
                       "Heading regex (empty to skip): "))
         (limit-input (read-string
                       "Result limit (number, empty for all): "))
         ;; Phase 3: Sort
         (sort-choice (completing-read "Sort: "
                                       org-linker-edna--rel-sorts nil t))
         (reverse-p (when (not (string= sort-choice "(none)"))
                      (y-or-n-p "Reverse sort? ")))
         ;; Assemble options
         (opts (list selection)))
    (setq opts (append opts filters))
    (when (not (string-blank-p tag-input))
      (setq opts (append opts (list (format "\"%s\"" tag-input)))))
    (when (not (string-blank-p regex-input))
      (setq opts (append opts (list (format "\"%s\"" regex-input)))))
    (when (not (string-blank-p limit-input))
      (setq opts (append opts (list limit-input))))
    (unless (string= sort-choice "(none)")
      (setq opts (append opts (list sort-choice))))
    (when reverse-p
      (setq opts (append opts (list "reverse-sort"))))
    (format "relatives(%s)" (mapconcat #'identity opts " "))))

;;;###autoload
(defun org-linker-edna-relatives-builder ()
  "Build a relatives() finder and set it as TRIGGER or BLOCKER.
The relatives finder is org-edna's most powerful finder, with
composable selection, filtering, and sorting phases."
  (interactive)
  (org-back-to-heading)
  (let* ((property (completing-read "Set property: "
                                    '("TRIGGER" "BLOCKER") nil t))
         (finder-str (org-linker-edna--build-relatives-string)))
    (if (string= property "TRIGGER")
        (let* ((actions-plist (org-linker-edna-actions-dispatcher))
               (action-str (cl-loop for (key value) on actions-plist
                                    by #'cddr
                                    concat (org-linker-edna--format-action
                                            key value)))
               (existing (org-entry-get (point) property))
               (new-value (string-trim
                           (concat (or existing "")
                                   " " finder-str action-str))))
          (org-entry-put (point) property new-value)
          (message "TRIGGER: %s%s" finder-str action-str))
      (let* ((consideration (org-linker-edna--select-consideration))
             (condition (org-linker-edna--select-blocker-condition))
             (existing (org-entry-get (point) property))
             (new-value (string-trim
                         (concat (or existing "")
                                 consideration
                                 " " finder-str condition))))
        (org-entry-put (point) property new-value)
        (message "BLOCKER:%s %s%s" consideration finder-str condition)))))


;;; Action dispatching — all 12 org-edna trigger actions

(defun org-linker-edna-date-selector (type)
  "Prompt for a date using the Org date picker for TYPE."
  (org-read-date nil nil nil (format "%s: " (capitalize type))))

(defun org-linker-edna-state-selector ()
  "Select a TODO state using `completing-read'."
  (completing-read "TODO state: " org-todo-keywords-1 nil t))

(defun org-linker-edna-action-dispatcher (candidate)
  "Dispatch a single action CANDIDATE, prompting for its value."
  (pcase candidate
    ("todo"
     `(:todo ,(org-linker-edna-state-selector)))
    ("scheduled"
     `(:scheduled ,(org-linker-edna-date-selector "scheduled")))
    ("deadline"
     `(:deadline ,(org-linker-edna-date-selector "deadline")))
    ("tag"
     `(:tag ,(read-string "Tags (e.g. :work:urgent:): ")))
    ("set-property"
     `(:set-property ,(cons (read-string "Property name: ")
                            (read-string "Property value: "))))
    ("delete-property"
     `(:delete-property ,(read-string "Property to delete: ")))
    ("set-priority"
     `(:set-priority ,(completing-read "Priority: "
                                       '("A" "B" "C" "up" "down") nil t)))
    ("set-effort"
     `(:set-effort ,(read-string "Effort (e.g. 1:00): ")))
    ("clock-in" '(:clock-in t))
    ("clock-out" '(:clock-out t))
    ("archive" '(:archive t))
    ("chain"
     `(:chain ,(read-string "Property to chain: ")))))

(defun org-linker-edna--format-action (key value)
  "Format a single edna action KEY with VALUE for TRIGGER property."
  (pcase key
    (:todo (format " todo!(%s)" value))
    (:scheduled (format " scheduled!(\"%s\")" value))
    (:deadline (format " deadline!(\"%s\")" value))
    (:tag (format " tag!(%s)" value))
    (:set-property
     (format " set-property!(\"%s\" \"%s\")" (car value) (cdr value)))
    (:delete-property (format " delete-property!(\"%s\")" value))
    (:set-priority (format " set-priority!(%s)" value))
    (:set-effort (format " set-effort!(\"%s\")" value))
    (:clock-in " clock-in!()")
    (:clock-out " clock-out!()")
    (:archive " archive!()")
    (:chain (format " chain!(\"%s\")" value))))

(defun org-linker-edna-actions-dispatcher ()
  "Select trigger actions and collect their values.
Uses `completing-read-multiple' with all available actions."
  (let ((selected (completing-read-multiple
                   "Trigger actions (comma-separated): "
                   org-linker-edna-actions)))
    (mapcan #'org-linker-edna-action-dispatcher selected)))

(defun org-linker-edna-set-trigger (source target)
  "Append SOURCE's ID and actions to TRIGGER on TARGET, preserving all existing tokens.
Deduplicates IDs: if already present, skips adding it but still appends actions."
  (let* ((existing (org-entry-get target "TRIGGER"))
         (existing-ids (org-linker-edna-ids existing))
         (source-id (concat "\"id:" (org-linker-edna-get-or-create-id-for-marker source) "\""))
         (new-ids (if (member source-id existing-ids)
                      existing-ids
                    (append existing-ids (list source-id))))
         (ids-block (concat "ids(" (mapconcat #'identity new-ids " ") ")"))
         (rest (when existing
                 (string-trim
                  (replace-regexp-in-string "ids([^)]*)" "" existing))))
         (actions-plist (org-linker-edna-actions-dispatcher))
         (action-str (cl-loop for (key value) on actions-plist by #'cddr
                              concat (org-linker-edna--format-action key value)))
         (parts (delq nil (list ids-block
                                (when (and rest (not (string-blank-p rest))) rest)
                                (when (not (string-blank-p action-str)) action-str))))
         (new-val (string-trim (mapconcat #'identity parts " "))))
    (org-entry-put target "TRIGGER" new-val)))


;;; Callbacks

(defun org-linker-edna-callback (source target)
  "Set blocker on TARGET and trigger on SOURCE for edna dependency.
SOURCE is the current heading; TARGET is the selected heading.
Creates: BLOCKER on TARGET (blocked by SOURCE), TRIGGER on SOURCE
\(when SOURCE is done, fire actions on TARGET)."
  (org-linker-edna-set-blocker source target)
  (org-linker-edna-set-trigger target source)
  (message "Dependency: %s blocks %s"
           (org-linker-edna--heading-at-marker source)
           (org-linker-edna--heading-at-marker target)))

(defun org-linker-edna-blocker-callback (source target)
  "Set blocker on SOURCE (current heading) and trigger on TARGET.
Reverse direction: select a heading that blocks the current one.
Creates: BLOCKER on SOURCE (blocked by TARGET), TRIGGER on TARGET
\(when TARGET is done, fire actions on SOURCE)."
  (org-linker-edna-set-blocker target source)
  (org-linker-edna-set-trigger source target)
  (message "Dependency: %s is blocked by %s"
           (org-linker-edna--heading-at-marker source)
           (org-linker-edna--heading-at-marker target)))


;;; Interactive commands — Create

;;;###autoload
(defun org-linker-edna ()
  "Create a dependency: current heading triggers actions on target.
The selected target will be blocked by the current heading.

With \\[universal-argument], search only the current file.
With \\[universal-argument] \\[universal-argument], search narrowed buffer."
  (interactive)
  (org-linker 'org-linker-edna-callback))

;;;###autoload
(defun org-linker-edna-blocker ()
  "Create a dependency: selected heading blocks the current one.
When the selected blocker is marked DONE, trigger actions on the
current heading.

With \\[universal-argument], search only the current file.
With \\[universal-argument] \\[universal-argument], search narrowed buffer."
  (interactive)
  (org-linker 'org-linker-edna-blocker-callback))


;;;###autoload
(defun org-linker-edna-trigger-finder ()
  "Append a finder-based TRIGGER on heading at point.
Prompts for a finder and actions, then appends them to any existing TRIGGER."
  (interactive)
  (org-back-to-heading)
  (let* ((finder-info (org-linker-edna--select-finder))
         (options (when (eq (plist-get finder-info :type) 'relative)
                   (org-linker-edna--select-finder-options)))
         (finder-str (org-linker-edna--build-finder-string finder-info options))
         (actions-plist (org-linker-edna-actions-dispatcher))
         (action-str (cl-loop for (key value) on actions-plist by #'cddr
                              concat (org-linker-edna--format-action key value)))
         (existing (org-entry-get (point) "TRIGGER"))
         (new-value (string-trim
                     (concat (or existing "")
                             (format " %s%s" finder-str action-str)))))
    (org-entry-put (point) "TRIGGER" new-value)
    (message "TRIGGER: %s%s" finder-str action-str)))

;;;###autoload
(defun org-linker-edna-blocker-finder ()
  "Append a finder-based BLOCKER on heading at point.
Prompts for a finder, consideration, and condition, then appends to any existing BLOCKER."
  (interactive)
  (org-back-to-heading)
  (let* ((finder-info (org-linker-edna--select-finder))
         (options (when (eq (plist-get finder-info :type) 'relative)
                   (org-linker-edna--select-finder-options)))
         (finder-str (org-linker-edna--build-finder-string finder-info options))
         (consideration (org-linker-edna--select-consideration))
         (condition (org-linker-edna--select-blocker-condition))
         (existing (org-entry-get (point) "BLOCKER"))
         (new-value (string-trim
                     (concat (or existing "")
                             consideration
                             (format " %s%s" finder-str condition)))))
    (org-entry-put (point) "BLOCKER" new-value)
    (message "BLOCKER:%s %s%s" consideration finder-str condition)))


;;; Interactive commands — Navigate

;;;###autoload
(defun org-linker-edna-goto ()
  "Jump to a BLOCKER or TRIGGER dependency of heading at point."
  (interactive)
  (org-back-to-heading)
  (let* ((blocker-cands
          (org-linker-edna--collect-ids-with-headings "BLOCKER"))
         (trigger-cands
          (org-linker-edna--collect-ids-with-headings "TRIGGER"))
         (all (append
               (mapcar (lambda (c)
                         (cons (format "[BLOCKER] %s" (car c)) (cdr c)))
                       blocker-cands)
               (mapcar (lambda (c)
                         (cons (format "[TRIGGER] %s" (car c)) (cdr c)))
                       trigger-cands))))
    (unless all
      (user-error "No dependencies on this heading"))
    (let* ((choice (completing-read "Goto dependency: " all nil t))
           (id-str (cdr (assoc choice all)))
           (bare-id (replace-regexp-in-string "^\"id:\\|\"$" "" id-str))
           (marker (org-id-find bare-id 'marker)))
      (unless marker
        (user-error "Cannot find heading for ID %s" bare-id))
      (switch-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker))
      (org-reveal))))

;;;###autoload
(defun org-linker-edna-show-deps ()
  "Display BLOCKER and TRIGGER dependencies for heading at point."
  (interactive)
  (org-back-to-heading)
  (let ((heading (org-get-heading t t t t))
        (blockers (org-linker-edna--collect-ids-with-headings "BLOCKER"))
        (triggers (org-linker-edna--collect-ids-with-headings "TRIGGER"))
        (trigger-raw (org-entry-get (point) "TRIGGER")))
    (with-help-window "*Org Edna Dependencies*"
      (with-current-buffer "*Org Edna Dependencies*"
        (insert (format "Dependencies for: %s\n" heading))
        (insert (make-string 60 ?-) "\n\n")
        (insert "BLOCKERS (this task is blocked by):\n")
        (if blockers
            (dolist (b blockers)
              (insert (format "  - %s\n" (car b))))
          (insert "  (none)\n"))
        (insert "\nTRIGGERS (when this task is done, fire):\n")
        (if triggers
            (progn
              (dolist (tr triggers)
                (insert (format "  - %s\n" (car tr))))
              (when trigger-raw
                (let ((actions (replace-regexp-in-string
                                "ids([^)]*)" "" trigger-raw)))
                  (when (not (string-blank-p (string-trim actions)))
                    (insert (format "\n  Actions: %s\n"
                                    (string-trim actions)))))))
          (insert "  (none)\n"))))))


;;; Interactive commands — Manage

(defun org-linker-edna--split-edna-tokens (s)
  "Split edna property string S into top-level tokens.
Tokens are split on whitespace except inside parentheses.
Returns a list of non-empty token strings."
  (let ((tokens nil)
        (current "")
        (depth 0))
    (dolist (ch (string-to-list s))
      (cond
       ((= ch ?\() (cl-incf depth) (setq current (concat current (string ch))))
       ((= ch ?\)) (cl-decf depth) (setq current (concat current (string ch))))
       ((and (= depth 0) (memq ch '(?\s ?\t ?\n)))
        (when (not (string-blank-p current))
          (push current tokens))
        (setq current ""))
       (t (setq current (concat current (string ch))))))
    (when (not (string-blank-p current))
      (push current tokens))
    (nreverse tokens)))

(defun org-linker-edna--collect-all-tokens (property)
  "Collect all removable tokens from PROPERTY at point.
Returns an alist of (DISPLAY-STRING . TOKEN-STRING).
ID-based tokens show the heading text; predicate/action tokens show as-is."
  (let* ((prop-val (org-entry-get (point) property))
         result)
    (when prop-val
      ;; Collect ID-based tokens (individual IDs within ids(...))
      (let ((ids (org-linker-edna-ids prop-val)))
        (dolist (id-str ids)
          (let* ((bare-id (replace-regexp-in-string "^\"id:\\|\"$" "" id-str))
                 (marker (org-id-find bare-id 'marker))
                 (heading (if marker
                              (with-current-buffer (marker-buffer marker)
                                (save-excursion
                                  (goto-char (marker-position marker))
                                  (org-get-heading t t t t)))
                            (format "[missing: %s]" bare-id))))
            (push (cons (format "[id] %s  (%s)" heading bare-id) id-str)
                  result))))
      ;; Collect non-ID tokens: strip ids(...) blocks, tokenize the rest
      ;; Each top-level whitespace-separated token (respecting parens) is removable
      (let* ((stripped (replace-regexp-in-string "ids([^)]*)" "" prop-val))
             (words (org-linker-edna--split-edna-tokens (string-trim stripped))))
        (dolist (w words)
          (push (cons (format "[pred] %s" w) w) result))))
    (nreverse result)))

;;;###autoload
(defun org-linker-edna-remove ()
  "Remove one or more dependency tokens from the heading at point.
Shows all BLOCKER or TRIGGER tokens — both ID-based and predicate-based
(e.g. previous-sibling, children) — and accepts comma-separated selection
for removing multiple tokens at once."
  (interactive)
  (org-back-to-heading)
  (let* ((property (completing-read "Remove from property: "
                                    '("BLOCKER" "TRIGGER") nil t))
         (prop-val (org-entry-get (point) property)))
    (unless prop-val
      (user-error "No %s property on this heading" property))
    (let* ((candidates (org-linker-edna--collect-all-tokens property)))
      (unless candidates
        (user-error "No removable tokens in %s" property))
      (let* ((selected-list (completing-read-multiple
                             (format "Remove %s tokens (comma-separated): " property)
                             candidates nil t))
             (tokens-to-remove (mapcar (lambda (s) (cdr (assoc s candidates)))
                                       selected-list)))
        (unless tokens-to-remove
          (user-error "No tokens selected"))
        ;; Partition into ID tokens and predicate tokens
        (let* ((id-tokens (cl-remove-if-not (lambda (tok) (string-prefix-p "\"id:" tok))
                                             tokens-to-remove))
               (pred-tokens (cl-remove-if (lambda (tok) (string-prefix-p "\"id:" tok))
                                          tokens-to-remove))
               ;; Rebuild IDs: keep those not in id-tokens
               (all-ids (org-linker-edna-ids prop-val))
               (remaining-ids (cl-remove-if (lambda (id) (member id id-tokens)) all-ids))
               ;; Rebuild predicates: keep those not in pred-tokens
               (rest (string-trim (replace-regexp-in-string "ids([^)]*)" "" prop-val)))
               (all-pred-tokens (org-linker-edna--split-edna-tokens rest))
               (remaining-preds (cl-remove-if (lambda (tok) (member tok pred-tokens))
                                              all-pred-tokens))
               ;; Assemble new value
               (ids-block (when remaining-ids
                            (concat "ids(" (mapconcat #'identity remaining-ids " ") ")")))
               (parts (delq nil (list ids-block
                                      (when remaining-preds
                                        (mapconcat #'identity remaining-preds " ")))))
               (new-val (string-trim (mapconcat #'identity parts " "))))
          (if (string-blank-p new-val)
              (org-entry-delete (point) property)
            (org-entry-put (point) property new-val))
          (message "Removed %d token(s) from %s" (length tokens-to-remove) property))))))

;;;###autoload
(defun org-linker-edna-edit-raw ()
  "Edit BLOCKER or TRIGGER property at point as raw text.
For advanced users who know org-edna syntax."
  (interactive)
  (org-back-to-heading)
  (let* ((property (completing-read "Edit property: "
                                    '("BLOCKER" "TRIGGER") nil t))
         (current (or (org-entry-get (point) property) ""))
         (new-value (read-string (format "%s: " property) current)))
    (if (string-blank-p new-value)
        (org-entry-delete (point) property)
      (org-entry-put (point) property new-value))))

;;;###autoload
(defun org-linker-edna-validate ()
  "Scan agenda files for malformed BLOCKER/TRIGGER properties.
Reports missing IDs, malformed syntax, and orphaned references."
  (interactive)
  (let ((files (org-agenda-files))
        problems)
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward
                 "^[ \t]*:\\(BLOCKER\\|TRIGGER\\):[ \t]+\\(.*\\)$" nil t)
           (let* ((property (match-string 1))
                  (value (match-string 2))
                  (heading (save-excursion
                             (org-back-to-heading)
                             (org-get-heading t t t t)))
                  (ids (org-linker-edna-ids value)))
             (unless (or ids (not (string-match-p "ids" value)))
               (push (format "%s:%d — %s has malformed ids syntax: %s"
                             (file-name-nondirectory file)
                             (line-number-at-pos) heading value)
                     problems))
             (dolist (id-str ids)
               (let ((bare-id (replace-regexp-in-string
                                "^\"id:\\|\"$" "" id-str)))
                 (unless (org-id-find bare-id)
                   (push (format "%s:%d — %s %s references missing ID: %s"
                                 (file-name-nondirectory file)
                                 (line-number-at-pos) heading property
                                 bare-id)
                         problems)))))))))
    (if problems
        (with-help-window "*Org Edna Validation*"
          (with-current-buffer "*Org Edna Validation*"
            (insert (format "Found %d issue(s):\n\n" (length problems)))
            (dolist (p (nreverse problems))
              (insert (format "  - %s\n" p)))))
      (message "All BLOCKER/TRIGGER properties are valid."))))


;;; Presets — common dependency patterns

;;;###autoload
(defun org-linker-edna-preset-sequential ()
  "Toggle sequential-sibling dependency on the heading at point.
Toggles previous-sibling in BLOCKER and next-sibling todo!(TODO) in TRIGGER.
If both are already present, removes them; otherwise adds them."
  (interactive)
  (org-back-to-heading)
  (let* ((blocker (org-entry-get (point) "BLOCKER"))
         (trigger (org-entry-get (point) "TRIGGER"))
         (b-tokens (when blocker (org-linker-edna--split-edna-tokens (string-trim blocker))))
         (t-tokens (when trigger (org-linker-edna--split-edna-tokens (string-trim trigger))))
         (has-blocker (member "previous-sibling" b-tokens))
         (has-trigger-finder (member "next-sibling" t-tokens))
         (removing (and has-blocker has-trigger-finder)))
    (if removing
        (progn
          (let* ((rb (delete "previous-sibling" (copy-sequence b-tokens)))
                 (new-b (string-trim (mapconcat #'identity rb " "))))
            (if (string-blank-p new-b)
                (org-entry-delete (point) "BLOCKER")
              (org-entry-put (point) "BLOCKER" new-b)))
          (let* ((rt (cl-remove-if (lambda (tok)
                                     (member tok '("next-sibling" "todo!(TODO)")))
                                   (copy-sequence t-tokens)))
                 (new-t (string-trim (mapconcat #'identity rt " "))))
            (if (string-blank-p new-t)
                (org-entry-delete (point) "TRIGGER")
              (org-entry-put (point) "TRIGGER" new-t)))
          (message "Removed sequential-sibling dependency"))
      (progn
        (org-entry-put (point) "BLOCKER"
                       (string-trim (concat (or blocker "") " previous-sibling")))
        (org-entry-put (point) "TRIGGER"
                       (string-trim (concat (or trigger "") " next-sibling todo!(TODO)")))
        (message "Added sequential-sibling dependency")))))

;;;###autoload
(defun org-linker-edna-preset-parent-gates ()
  "Toggle children todo!(TODO) in the TRIGGER property at point.
If both tokens are already present, removes them; otherwise appends them."
  (interactive)
  (org-back-to-heading)
  (let* ((existing (org-entry-get (point) "TRIGGER"))
         (tokens (when existing (org-linker-edna--split-edna-tokens (string-trim existing))))
         (has-finder (member "children" tokens))
         (has-action (member "todo!(TODO)" tokens)))
    (if (and has-finder has-action)
        (let* ((remaining (cl-remove-if (lambda (tok)
                                          (member tok '("children" "todo!(TODO)")))
                                        (copy-sequence tokens)))
               (new-val (string-trim (mapconcat #'identity remaining " "))))
          (if (string-blank-p new-val)
              (org-entry-delete (point) "TRIGGER")
            (org-entry-put (point) "TRIGGER" new-val))
          (message "Removed parent-gates trigger"))
      (org-entry-put (point) "TRIGGER"
                     (string-trim (concat (or existing "") " children todo!(TODO)")))
      (message "Added parent-gates trigger"))))

;;;###autoload
(defun org-linker-edna-preset-children-gate ()
  "Toggle children in the BLOCKER property at point.
If already present, removes it; otherwise appends it."
  (interactive)
  (org-back-to-heading)
  (let* ((existing (org-entry-get (point) "BLOCKER"))
         (tokens (when existing (org-linker-edna--split-edna-tokens (string-trim existing)))))
    (if (member "children" tokens)
        (let* ((remaining (delete "children" (copy-sequence tokens)))
               (new-val (string-trim (mapconcat #'identity remaining " "))))
          (if (string-blank-p new-val)
              (org-entry-delete (point) "BLOCKER")
            (org-entry-put (point) "BLOCKER" new-val))
          (message "Removed children blocker"))
      (org-entry-put (point) "BLOCKER"
                     (string-trim (concat (or existing "") " children")))
      (message "Added children blocker"))))

;;;###autoload
(defun org-linker-edna-preset-previous-sibling ()
  "Toggle previous-sibling in the BLOCKER property at point.
If previous-sibling is already present, removes it.
Otherwise appends it, preserving any existing blocker tokens."
  (interactive)
  (org-back-to-heading)
  (let* ((existing (org-entry-get (point) "BLOCKER"))
         (tokens (when existing
                   (org-linker-edna--split-edna-tokens (string-trim existing))))
         (already-p (member "previous-sibling" tokens)))
    (if already-p
        (let* ((remaining (delete "previous-sibling" (copy-sequence tokens)))
               (new-val (string-trim (mapconcat #'identity remaining " "))))
          (if (string-blank-p new-val)
              (org-entry-delete (point) "BLOCKER")
            (org-entry-put (point) "BLOCKER" new-val))
          (message "Removed previous-sibling from BLOCKER"))
      (let ((new-val (string-trim
                      (concat (or existing "") " previous-sibling"))))
        (org-entry-put (point) "BLOCKER" new-val)
        (message "Added previous-sibling to BLOCKER")))))


;;; Batch operations — apply patterns to subtrees

;;;###autoload
(defun org-linker-edna-batch-sequential ()
  "Make all direct children of heading at point sequential.
Each child (except the first) gets BLOCKER: previous-sibling.
Each child (except the last) gets TRIGGER: next-sibling todo!(TODO)."
  (interactive)
  (org-back-to-heading)
  (let ((parent-level (org-current-level))
        (first t)
        (count 0)
        last-child-pos)
    (save-excursion
      (org-map-entries
       (lambda ()
         (when (= (org-current-level) (1+ parent-level))
           (cl-incf count)
           (setq last-child-pos (point))
           (if first
               (progn
                 (org-entry-put (point) "TRIGGER"
                                "next-sibling todo!(TODO)")
                 (setq first nil))
             (org-entry-put (point) "BLOCKER" "previous-sibling")
             (org-entry-put (point) "TRIGGER"
                            "next-sibling todo!(TODO)"))))
       nil 'tree))
    ;; Last child has no next sibling to trigger
    (when last-child-pos
      (save-excursion
        (goto-char last-child-pos)
        (org-entry-delete (point) "TRIGGER")))
    (message "Made %d children sequential" count)))

;;;###autoload
(defun org-linker-edna-batch-clear ()
  "Remove all BLOCKER and TRIGGER properties from this subtree.
Includes the heading at point and all descendants."
  (interactive)
  (org-back-to-heading)
  (when (y-or-n-p "Remove all BLOCKER/TRIGGER from this subtree? ")
    (let ((count 0))
      (save-excursion
        (org-map-entries
         (lambda ()
           (when (or (org-entry-get (point) "BLOCKER")
                     (org-entry-get (point) "TRIGGER"))
             (cl-incf count)
             (org-entry-delete (point) "BLOCKER")
             (org-entry-delete (point) "TRIGGER")))
         nil 'tree))
      (message "Cleared dependencies from %d headings" count))))


;;; Help

;;;###autoload
(defun org-linker-edna-action-help ()
  "Display a reference of all org-edna trigger action keywords."
  (interactive)
  (with-help-window "*Org Edna Actions*"
    (with-current-buffer "*Org Edna Actions*"
      (insert "Org-Edna Trigger Actions Reference\n")
      (insert (make-string 40 ?=) "\n\n")
      (insert "COMMON\n")
      (insert "  todo!(STATE)                 Set TODO state\n")
      (insert "  scheduled!(\"DATE\")           Set scheduled date\n")
      (insert "  deadline!(\"DATE\")            Set deadline date\n\n")
      (insert "TAGS & PROPERTIES\n")
      (insert "  tag!(TAGS)                   Set tags (:work:urgent:)\n")
      (insert "  set-property!(\"P\" \"V\")       Set property P to V\n")
      (insert "  delete-property!(\"P\")        Delete property P\n\n")
      (insert "PRIORITY & EFFORT\n")
      (insert "  set-priority!(A|B|C|up|down) Set/adjust priority\n")
      (insert "  set-effort!(\"HH:MM\")         Set effort estimate\n\n")
      (insert "CLOCKING & ARCHIVING\n")
      (insert "  clock-in!()                  Clock into heading\n")
      (insert "  clock-out!()                 Clock out\n")
      (insert "  archive!()                   Archive heading\n")
      (insert "  chain!(\"PROPERTY\")           Copy property from source\n\n")
      (insert (make-string 40 ?-) "\n")
      (insert "BLOCKER CONDITIONS  (end with ?)\n")
      (insert "  done?            Is target in DONE state?\n")
      (insert "  todo-state?(S)   Does target have state S?\n")
      (insert "  has-property?(P V)  Does heading have P=V?\n")
      (insert "  has-tags?(T...)  Does heading have tags?\n")
      (insert "  Negate with !:   !done?  !todo-state?(S)\n\n")
      (insert "CONSIDERATION  (blocker modifier)\n")
      (insert "  consider(any)    Block if ANY match (default)\n")
      (insert "  consider(all)    Block only if ALL match\n")
      (insert "  consider(N)      Block if >= N match\n"))))


;;;###autoload
(defun org-linker-edna-finder-help ()
  "Display a reference of all org-edna finder and filter keywords."
  (interactive)
  (with-help-window "*Org Edna Finders*"
    (with-current-buffer "*Org Edna Finders*"
      (insert "Org-Edna Finders Reference\n")
      (insert (make-string 44 ?=) "\n\n")
      (insert "RELATIVE FINDERS  (no arguments)\n")
      (insert "  self                       The heading itself\n")
      (insert "  parent                     Direct parent\n")
      (insert "  children                   All direct children\n")
      (insert "  first-child                First child only\n")
      (insert "  siblings                   All siblings (excl. self)\n")
      (insert "  next-sibling               Next sibling\n")
      (insert "  previous-sibling           Previous sibling\n")
      (insert "  next-sibling-wrap          Next sibling, wraps\n")
      (insert "  previous-sibling-wrap      Previous sibling, wraps\n")
      (insert "  rest-of-siblings           All following siblings\n")
      (insert "  rest-of-siblings-wrap      (wrapping)\n")
      (insert "  ancestors                  All ancestor headings\n")
      (insert "  descendants                All descendant headings\n\n")
      (insert "ABSOLUTE FINDERS  (with arguments)\n")
      (insert "  ids(\"id:UUID\" ...)         By org ID(s)\n")
      (insert "  match(\"TAGS/PROP\")         By tag/property match\n")
      (insert "  olp(\"file\" \"h1/h2\")        By outline path\n")
      (insert "  file(\"path\")               All headings in file\n")
      (insert "  org-file(\"name\")           File in org-directory\n\n")
      (insert "RELATIVES FINDER  (composable — use R to build)\n")
      (insert "  relatives(SELECTION FILTERS SORT)\n")
      (insert "  Selections: from-top  from-bottom  from-current\n")
      (insert "    forward-no-wrap  backward-no-wrap  backward-wrap\n")
      (insert "    walk-up  walk-up-with-self\n")
      (insert "    walk-down  walk-down-with-self  step-down\n\n")
      (insert (make-string 44 ?-) "\n")
      (insert "FILTER OPTIONS  (append after a relative finder)\n")
      (insert "  todo-only              Only TODO-state entries\n")
      (insert "  todo-and-done-only     TODO + DONE, skip unset\n")
      (insert "  no-comment             Skip COMMENT headings\n")
      (insert "  no-archive             Skip archived headings\n")
      (insert "  +tag  -tag             Require / exclude a tag\n\n")
      (insert "COMMON EXAMPLES\n")
      (insert "  BLOCKER: children\n")
      (insert "  BLOCKER: previous-sibling\n")
      (insert "  BLOCKER: ids(\"id:abc-123\")\n")
      (insert "  TRIGGER: children todo!(TODO)\n")
      (insert "  TRIGGER: next-sibling todo!(TODO)\n")
      (insert "  TRIGGER: rest-of-siblings todo!(HOLD)\n"))))


;;; Transient menu

;;;###autoload
(defun org-linker-edna-menu ()
  "Org-Edna dependency management transient menu."
  (interactive)
  (require 'transient)
  (org-linker-edna-menu--transient))

(transient-define-prefix org-linker-edna-menu--transient ()
  "Org-Edna dependency management."
  [["Create (ID-based)"
    ("t" "Trigger  (current -> target)" org-linker-edna)
    ("b" "Blocker  (target -> current)" org-linker-edna-blocker)]
   ["Create (finder-based)"
    ("T" "Trigger with finder" org-linker-edna-trigger-finder)
    ("B" "Blocker with finder" org-linker-edna-blocker-finder)
    ("R" "Relatives builder" org-linker-edna-relatives-builder)]]
  [["Navigate"
    ("g" "Goto dependency" org-linker-edna-goto)
    ("s" "Show dependencies" org-linker-edna-show-deps)]
   ["Manage"
    ("r" "Remove dependency" org-linker-edna-remove)
    ("e" "Edit raw property" org-linker-edna-edit-raw)
    ("v" "Validate all" org-linker-edna-validate)
    ("?" "Action reference" org-linker-edna-action-help)
    ("/" "Finder reference" org-linker-edna-finder-help)]]
  [["Presets"
    ("1" "Sequential sibling" org-linker-edna-preset-sequential)
    ("2" "Parent gates children" org-linker-edna-preset-parent-gates)
    ("3" "Children gate parent" org-linker-edna-preset-children-gate)
    ("4" "Previous sibling blocker" org-linker-edna-preset-previous-sibling)]
   ["Batch"
    ("!" "Make children sequential" org-linker-edna-batch-sequential)
    ("X" "Clear subtree deps" org-linker-edna-batch-clear)]])


;;; Embark integration

(defun org-linker-edna--resolve-target (heading targets)
  "Resolve HEADING to a single marker from TARGETS list.
If multiple matches, prompt to disambiguate."
  (cond
   ((null targets) (user-error "No heading found: %s" heading))
   ((= 1 (length targets)) (car targets))
   (t (let* ((choices
              (mapcar (lambda (m)
                        (cons (format "%s — %s"
                                      (org-linker-edna--heading-at-marker m)
                                      (buffer-name (marker-buffer m)))
                              m))
                      targets))
             (choice (completing-read "Multiple matches: " choices nil t)))
        (cdr (assoc choice choices))))))

(defun org-linker-edna-embark-link (candidate)
  "Create edna dependency from heading at point to CANDIDATE.
When called from an Embark action on an org-heading:
  - Source is the heading at point (before minibuffer, or current).
  - Target is resolved from CANDIDATE text.
If CANDIDATE matches the heading at point, falls back to the
interactive `org-linker-edna' flow instead."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (save-excursion
    (org-back-to-heading)
    (let ((current-heading (org-get-heading t t t t))
          (heading (substring-no-properties candidate)))
      (if (string= heading current-heading)
          ;; At-point context: candidate is the heading we're on.
          ;; Launch the full flow to pick a *different* target.
          (org-linker-edna)
        ;; Minibuffer context: we came back from org-ql-find or similar.
        ;; Source = heading at point, target = candidate heading.
        (let* ((source (point-marker))
               (targets (org-ql-select (org-agenda-files)
                          `(heading ,heading)
                          :action (lambda () (point-marker))))
               (target (org-linker-edna--resolve-target heading targets)))
          (org-linker-edna-callback source target))))))

(with-eval-after-load 'embark
  (when (boundp 'embark-org-heading-map)
    (define-key embark-org-heading-map "e" #'org-linker-edna-embark-link)))


(provide 'org-linker-edna)

;;; org-linker-edna.el ends here
