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


;;; Core helpers

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
                       "\"id:\\|\"" "" id-str))
             (loc (org-id-find bare-id))
             (heading (if loc
                         (with-current-buffer (find-file-noselect (car loc))
                           (save-excursion
                             (goto-char (cdr loc))
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
  "Set BLOCKER property on TARGET with SOURCE's ID."
  (let ((ids-str (format "ids%s"
                        (org-linker-edna-set-prop source target "BLOCKER"))))
    (org-entry-put target "BLOCKER" ids-str)))


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
  "Set TRIGGER property on TARGET with SOURCE's ID and actions."
  (let* ((actions-plist (org-linker-edna-actions-dispatcher))
         (action-str (cl-loop for (key value) on actions-plist by #'cddr
                              concat (org-linker-edna--format-action
                                      key value)))
         (id-string (org-linker-edna-set-prop source target "TRIGGER"))
         (existing-trigger (org-entry-get target "TRIGGER")))
    (org-entry-put target "TRIGGER"
                   (string-trim
                    (concat existing-trigger
                            (format " ids%s" id-string)
                            action-str)))))


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
           (bare-id (replace-regexp-in-string "\"id:\\|\"" "" id-str))
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

;;;###autoload
(defun org-linker-edna-remove ()
  "Remove a dependency from the heading at point.
Prompts to select a BLOCKER or TRIGGER ID to remove."
  (interactive)
  (org-back-to-heading)
  (let* ((property (completing-read "Remove from property: "
                                    '("BLOCKER" "TRIGGER") nil t))
         (candidates (org-linker-edna--collect-ids-with-headings property)))
    (unless candidates
      (user-error "No %s IDs on this heading" property))
    (let* ((selected (completing-read
                      (format "Remove %s: " property) candidates nil t))
           (id-str (cdr (assoc selected candidates)))
           (prop-val (org-entry-get (point) property))
           (all-ids (org-linker-edna-ids prop-val))
           (remaining (remove id-str all-ids)))
      (if remaining
          (let ((new-ids (concat "ids("
                                 (mapconcat #'identity remaining " ")
                                 ")")))
            (if (string= property "TRIGGER")
                ;; Preserve action portion (todo!, scheduled!, etc.)
                (let ((actions (replace-regexp-in-string
                                "ids([^)]*)" "" prop-val)))
                  (org-entry-put (point) property
                                 (string-trim (concat new-ids actions))))
              (org-entry-put (point) property new-ids)))
        (org-entry-delete (point) property))
      (message "Removed %s from %s" id-str property))))

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
                                "\"id:\\|\"" "" id-str)))
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


;;; Transient menu

;;;###autoload
(defun org-linker-edna-menu ()
  "Org-Edna dependency management transient menu."
  (interactive)
  (require 'transient)
  ;; Define the transient on first call, then invoke it
  (declare-function org-linker-edna-menu--transient "org-linker-edna")
  (unless (fboundp 'org-linker-edna-menu--transient)
    (transient-define-prefix org-linker-edna-menu--transient ()
      "Org-Edna dependency management."
      [["Create"
        ("t" "Trigger  (current -> target)" org-linker-edna)
        ("b" "Blocker  (target -> current)" org-linker-edna-blocker)]
       ["Navigate"
        ("g" "Goto dependency" org-linker-edna-goto)
        ("s" "Show dependencies" org-linker-edna-show-deps)]]
      [["Manage"
        ("r" "Remove dependency" org-linker-edna-remove)
        ("e" "Edit raw property" org-linker-edna-edit-raw)
        ("v" "Validate all" org-linker-edna-validate)]
       ["Help"
        ("?" "Action reference" org-linker-edna-action-help)]]))
  (org-linker-edna-menu--transient))


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
