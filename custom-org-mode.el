(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Setup key bindings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f6>") 'bh/org-todo)
(global-set-key (kbd "<S-f6>") 'bh/widen)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;;  GOD mode binding.
(global-set-key (kbd "C-c C-a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "x" nil) 
     (define-key org-agenda-mode-map "W" 'bh/widen)
     (define-key org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow)
     (define-key org-agenda-mode-map "N" 'bh/narrow-to-subtree)
     (define-key org-agenda-mode-map "U" 'bh/narrow-up-one-level)
     (define-key org-agenda-mode-map "P" 'bh/narrow-to-project)
     (define-key org-agenda-mode-map "V" 'bh/view-next-project)
     (define-key org-agenda-mode-map "q" 'bury-buffer)

     ;; Undefine C-c [ and C-c ] since this breaks my
     ;; org-agenda files when directories are include It
     ;; expands the files in the directories individually
     (define-key org-mode-map "\C-c[" 'undefined)
     (define-key org-mode-map "\C-c]" 'undefined)
     (define-key org-mode-map "\C-c;" 'undefined)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ORG-MODE hooks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
	  '(lambda () (hl-line-mode 1))
	  'append)

;;  Set files that should be used as part of the adgenda.
(setq org-agenda-files (quote ("~/emacs/Org/refile.org"
			       "~/emacs/Org/rtdn.org"
			       "~/emacs/Org/meetings.org"
			       "~/emacs/Org/das.org"
			       "~/emacs/Org/dmv.org"
			       "~/emacs/Org/warrants.org"
			       "~/emacs/Org/email.org"
			       "~/emacs/Org/bcms.org")))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "MIGRATED(m)" "|" "DONE(d)")
	      (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("NEXT" :foreground "blue" :weight bold)
	      ("MIGRATED" :foreground "yellow" :weight bold)
	      ("DONE" :foreground "forest green" :weight bold)
	      ("WAITING" :foreground "orange" :weight bold)
	      ("HOLD" :foreground "magenta" :weight bold)
	      ("CANCELLED" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
	      ("WAITING" ("WAITING" . t))
	      ("HOLD" ("WAITING" . t) ("HOLD" . t))
	      (done ("WAITING") ("HOLD"))
	      ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
	      ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
	      ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-directory "~/emacs/Org")
(setq org-default-notes-file "~/emacs/Org/refile.org")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/emacs/Org/refile.org")
	       "* TODO %?\n  %U\n" :clock-in t :clock-resume t)
	      ("r" "respond" entry (file "~/emacs/Org/refile.org")
	       "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
	      ("n" "note" entry (file "~/emacs/Org/refile.org")
	       "* %? :NOTE:\n  %U\n%a\n" :clock-in t :clock-resume t)
	      ("w" "org-protocol" entry (file "~/emacs/Org/refile.org")
	       "* TODO Review %c\n%U\n" :immediate-finish t)
	      ("h" "Habit" entry (file "~/emacs/Org/refile.org")
	       "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
	      ("m" "Meeting" entry (file "~/emacs/Org/refile.org")
	       "* TODO %?\nSCHEDULED: %t\n%U\n- Room :: \n- Time :: \n- Attendees ::\n  - [ ] \n- Background :: \n  -\n- Meeting notes :: \n  -"))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
				 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
	       ((org-agenda-overriding-header "Notes")
		(org-tags-match-list-sublevels t)))
	      ("h" "Habits" tags-todo "STYLE=\"habit\""
	       ((org-agenda-overriding-header "Habits")
		(org-agenda-sorting-strategy
		 '(todo-state-down effort-up category-keep))))
	      (" " "Agenda"
	       ((agenda "" nil)
		(tags "REFILE"
		      ((org-agenda-overriding-header "Tasks to Refile")
		       (org-tags-match-list-sublevels nil)))
		(tags-todo "-HOLD-CANCELLED/!"
			   ((org-agenda-overriding-header "Projects")
			    (org-agenda-skip-function 'bh/skip-non-projects)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "-CANCELLED/!"
			   ((org-agenda-overriding-header "Stuck Projects")
			    (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
		(tags-todo "-WAITING-CANCELLED/!NEXT"
			   ((org-agenda-overriding-header "Next Tasks")
			    (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
			    (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-todo-ignore-deadlines t)
			    (org-agenda-todo-ignore-with-date t)
			    (org-tags-match-list-sublevels t)
			    (org-agenda-sorting-strategy
			     '(todo-state-down effort-up category-keep))))
		(tags-todo "-WAITING-REFILE-CANCELLED-KRANTHI/!-HOLD-WAITING-NEXT-MIGRATED-KRANTHI"
			   ((org-agenda-overriding-header "Tasks")
			    (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
			    (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-todo-ignore-deadlines t)
			    (org-agenda-todo-ignore-with-date t)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "KRANTHI/-MIGRATED"
			   ((org-agenda-overriding-header "Kranthi")
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(todo "MIGRATED"
			   ((org-agenda-overriding-header "Migrated")
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "-CANCELLED+WAITING/!"
			   ((org-agenda-overriding-header "Waiting and Postponed Tasks")
			    (org-agenda-skip-function 'bh/skip-stuck-projects)
			    (org-tags-match-list-sublevels nil)
			    (org-agenda-todo-ignore-scheduled 'future)
			    (org-agenda-todo-ignore-deadlines 'future)))
		(tags "-REFILE/"
		      ((org-agenda-overriding-header "Tasks to Archive")
		       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
		       (org-tags-match-list-sublevels nil)))
		nil))
	      ("r" "Tasks to Refile" tags "REFILE"
	       ((org-agenda-overriding-header "Tasks to Refile")
		(org-tags-match-list-sublevels nil)))
	      ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
	       ((org-agenda-overriding-header "Stuck Projects")
		(org-agenda-skip-function 'bh/skip-non-stuck-projects)))
	      ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
	       ((org-agenda-overriding-header "Next Tasks")
		(org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
		(org-agenda-todo-ignore-scheduled t)
		(org-agenda-todo-ignore-deadlines t)
		(org-agenda-todo-ignore-with-date t)
		(org-tags-match-list-sublevels t)
		(org-agenda-sorting-strategy
		 '(todo-state-down effort-up category-keep))))
	      ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
	       ((org-agenda-overriding-header "Tasks")
		(org-agenda-skip-function 'bh/skip-project-tasks-maybe)
		(org-agenda-sorting-strategy
		 '(category-keep))))
	      ("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
	       ((org-agenda-overriding-header "Projects")
		(org-agenda-skip-function 'bh/skip-non-projects)
		(org-agenda-sorting-strategy
		 '(category-keep))))
	      ("w" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
	       ((org-agenda-overriding-header "Waiting and Postponed tasks"))
	       (org-tags-match-list-sublevels nil))
	      ("A" "Tasks to Archive" tags "-REFILE/"
	       ((org-agenda-overriding-header "Tasks to Archive")
		(org-agenda-skip-function 'bh/skip-non-archivable-tasks)
		(org-tags-match-list-sublevels nil))))))

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

(setq org-time-stamp-rounding-minutes (quote (1 1)))

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state)))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

(setq org-agenda-span 'day)

(setq org-stuck-projects (quote ("" nil nil "")))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(setq org-ditaa-jar-path "~/emacs/Java/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/emacs/Java/plantuml.jar")

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
	 (dot . t)
	 (ditaa . t)
	 (gnuplot . t)
	 (clojure . t)
	 (sh . t)
	 (org . t)
	 (plantuml . t))))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;; Don't enable this because it breaks access to emacs from my Android phone
(setq org-startup-with-inline-images nil)

; Inline images in HTML instead of producting links to the image
(setq org-html-inline-images t)

; Use org.css from the norang website for export document stylesheets
(setq org-html-style-extra "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\" />")
(setq org-html-style-include-default nil)

; Do not generate internal css formatting for HTML exports
(setq org-htmlize-output-type (quote css))

; Export with LaTeX fragments
(setq org-with-LaTeX-fragments t)

; Increase default number of headings to export
(setq org-headline-levels 6)

(setq org-allow-BIND t)

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

(setq org-show-entry-below (quote ((default))))

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
	      (todo category-up priority-down effort-up)
	      (tags category-up priority-down effort-up)
	      (search category-up))))

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
				   #("----------------" 0 16 (org-heading t))
				   (0900 1100 1300 1500 1700))))

;; Display tags farther right
(setq org-agenda-tags-column -130)

(setq org-agenda-cmp-user-defined 'bh/agenda-sort)

(setq org-enforce-todo-dependencies t)

(setq org-hide-leading-stars nil)

(setq org-cycle-separator-lines 0)

(setq org-blank-before-new-entry (quote ((heading)
					 (plain-list-item . auto))))

(setq org-insert-heading-respect-content nil)

(setq org-reverse-note-order nil)

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(setq org-id-method (quote uuidgen))

(setq org-deadline-warning-days 30)

(setq org-table-export-default-format "orgtbl-to-csv")

					; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)

(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

					; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (org-id
			  org-info
			  org-habit
			  org-inlinetask
			  org-protocol)))

					; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(global-auto-revert-mode t)

(setq require-final-newline t)

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

(setq org-with-timestamps nil)

(setq org-return-follows-link t)

(setq org-remove-highlights-with-change nil)

(setq org-read-date-prefer-future 'time)

(setq org-list-demote-modify-bullet (quote (("+" . "-")
					    ("*" . "-")
					    ("1." . "-")
					    ("1)" . "-"))))

(setq org-tags-match-list-sublevels t)

(setq org-agenda-persistent-filter t)

(setq org-agenda-skip-additional-timestamps-same-entry t)

(setq org-table-use-standard-references (quote from))

(setq org-file-apps (quote ((auto-mode . emacs)
			    ("\\.mm\\'" . system)
			    ("\\.x?html?\\'" . system)
			    ("\\.pdf\\'" . system))))

;; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

(setq org-clone-delete-id t)

(setq org-cycle-include-plain-lists t)

(setq org-src-fontify-natively t)
(setq org-fontify-whole-heading-line t)

(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)

(setq org-startup-folded t)

(setq org-list-allow-alphabetical t)

(setq org-enable-priority-commands t)
(setq org-default-priority ?E)
(setq org-lowest-priority ?E)

(setq org-coding-system 'utf-8)

(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

(setq org-catch-invisible-edits 'error)

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
				 ("/" italic "<i>" "</i>")
				 ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
				 ("=" org-code "<code>" "</code>" verbatim)
				 ("~" org-verbatim "<code>" "</code>" verbatim))))

(setq org-use-sub-superscripts nil)

(setq org-odd-levels-only nil)

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Functions related to ORG-MODE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bh/remove-empty-drawer-on-clock-out ()
  "Remove empty LOGBOOK drawers on clock out."
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
	(when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
	  (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
	  (subtree-end (save-excursion (org-end-of-subtree t)))
	  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
	(forward-line 1)
	(while (and (not has-subtask)
		    (< (point) subtree-end)
		    (re-search-forward "^\*+ " subtree-end t))
	  (when (member (org-get-todo-state) org-todo-keywords-1)
	    (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
			      (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
	  nil
	t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
	  (subtree-end (save-excursion (org-end-of-subtree t)))
	  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
	(forward-line 1)
	(while (and (not has-subtask)
		    (< (point) subtree-end)
		    (re-search-forward "^\*+ " subtree-end t))
	  (when (member (org-get-todo-state) org-todo-keywords-1)
	    (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
	(is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
	(when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
	  (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
	  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
		 (has-next ))
	    (save-excursion
	      (forward-line 1)
	      (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
		(unless (member "WAITING" (org-get-tags-at))
		  (setq has-next t))))
	    (if has-next
		nil
	      next-headline)) ; a stuck project, has subtasks but no next task
	nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
	  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
		 (has-next ))
	    (save-excursion
	      (forward-line 1)
	      (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
		(unless (member "WAITING" (org-get-tags-at))
		  (setq has-next t))))
	    (if has-next
		next-headline
	      nil)) ; a stuck project, has subtasks but no next task
	next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
	(widen)
	(let ((subtree-end (save-excursion (org-end-of-subtree t))))
	  (cond
	   ((and (bh/is-project-p)
		 (marker-buffer org-agenda-restrict-begin))
	    nil)
	   ((and (bh/is-project-p)
		 (not (marker-buffer org-agenda-restrict-begin))
		 (not (bh/is-project-subtree-p)))
	    nil)
	   (t
	    subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
	subtree-end)
       ((org-is-habit-p)
	subtree-end)
       (t
	nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
	next-headline)
       ((bh/is-project-p)
	next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
	next-headline)
       (t
	nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
	   (next-headline (save-excursion (or (outline-next-heading) (point-max))))
	   (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
	next-headline)
       ((org-is-habit-p)
	subtree-end)
       ((and (not limit-to-project)
	     (bh/is-project-subtree-p))
	subtree-end)
       ((and limit-to-project
	     (bh/is-project-subtree-p)
	     (member (org-get-todo-state) (list "NEXT")))
	subtree-end)
       (t
	nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
	subtree-end)
       ((org-is-habit-p)
	subtree-end)
       (t
	nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
	nil
      next-headline)))

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; Consider only tasks with done todo headings as archivable candidates
      (if (member (org-get-todo-state) org-done-keywords)
	  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
		 (daynr (string-to-number (format-time-string "%d" (current-time))))
		 (a-month-ago (* 60 60 24 (+ daynr 1)))
		 (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
		 (this-month (format-time-string "%Y-%m-" (current-time)))
		 (subtree-is-current (save-excursion
				       (forward-line 1)
				       (and (< (point) subtree-end)
					    (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
	    (if subtree-is-current
		next-headline ; Has a date in this month or last month, skip it
	      nil))  ; available to archive
	(or next-headline (point-max))))))

(defun bh/org-agenda-to-appt ()
  "Erase all reminders and rebuilt reminders for today from the agenda."
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
	(bh/narrow-to-org-subtree)
	(org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-agenda-remove-restriction-lock)
    (widen)
    (org-agenda-remove-restriction-lock)))

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (if (equal major-mode 'org-agenda-mode)
	(bh/set-agenda-restriction-lock 4)
      (widen))))

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
	(bh/narrow-to-org-subtree)
	(save-restriction
	  (org-agenda-set-restriction-lock)))
    (bh/narrow-to-org-subtree)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
	(bh/narrow-up-one-org-level))
    (bh/narrow-up-one-org-level)))

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
	(bh/narrow-to-org-project)
	(save-restriction
	  (org-agenda-set-restriction-lock)))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(defvar bh/current-view-project nil)

(defun bh/view-next-project ()
  (interactive)
  (unless (marker-position org-agenda-restrict-begin)
    (goto-char (point-min))
    (setq bh/current-view-project (point)))
  (bh/widen)
  (goto-char bh/current-view-project)
  (forward-visible-line 1)
  (while (and (< (point) (point-max))
	      (or (not (org-get-at-bol 'org-hd-marker))
		  (org-with-point-at (org-get-at-bol 'org-hd-marker)
		    (or (not (bh/is-project-p))
			(bh/is-project-subtree-p)))))
    (forward-visible-line 1))
  (setq bh/current-view-project (point))
  (if (org-get-at-bol 'org-hd-marker)
      (progn
	(bh/narrow-to-project)
	(org-agenda-redo)
	;;(beginning-of-buffer))
	(goto-char (point-min)))
    ;;(beginning-of-buffer)
	(goto-char (point-min))
    (error "All projects viewed.")))

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
	 (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
	(cond
	 ((and (equal major-mode 'org-agenda-mode) pom)
	  (org-with-point-at pom
	    (org-agenda-set-restriction-lock restriction-type)))
	 ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
	  (org-agenda-set-restriction-lock 'file))
	 (pom
	  (org-with-point-at pom
	    (org-agenda-set-restriction-lock restriction-type))))))))

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
					; time specific items are already sorted first by org-agenda-sorting-strategy
					; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

					; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

					; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '< a b))

					; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

					; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

					; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

					; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
					; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
	  (apply ,fn (list ,b)))
     (setq result nil))
					; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
					; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
					; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
	 (progn
	   (setq num-b (string-to-number (match-string 1 ,b)))
	   (setq result (if (apply ,compfn (list num-a num-b))
			    -1
			  1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "In *\\(-.*\\)d\.:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (org-return)
    (org-cycle)
    (bh/insert-inactive-timestamp)))

(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
			  state)
		     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
	(while (org-up-heading-safe)
	  (when (member (nth 2 (org-heading-components)) (list "NEXT"))
	    (org-todo "TODO")))))))

(defun jump-to-org-agenda ()
  "Jump to the org-mode agenda."
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
	wind)
    (if buf
	(if (setq wind (get-buffer-window buf))
	    (select-window wind)
	  (if (called-interactively-p)
	      (progn
		(select-window (display-buffer buf t t))
		(org-fit-window-to-buffer))
	    (with-selected-window (display-buffer buf)
	      (org-fit-window-to-buffer))))
      (call-interactively 'org-agenda-list))))

(defun bh/show-org-agenda ()
  (interactive)
  (switch-to-buffer "*Org Agenda*")
  (delete-other-windows))
