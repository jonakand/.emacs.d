(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(require 'org-habit)
(require 's)

;;  Use yasnippets for templates instead.
(setq org-structure-template-alist nil)

;;  Clock continuously.
(setq org-clock-continuously t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Setup key bindings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "<f10>") 'org-capture)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> r") 'org-capture)
(global-set-key (kbd "<f9> i") 'org-clock-in)
(global-set-key (kbd "<f9> o") 'org-clock-out)
(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)
(global-set-key (kbd "<f9> g") 'org-clock-goto)
(global-set-key (kbd "C-c C-x C-b") 'ry/clock-in-email)
(global-set-key (kbd "C-c C-x C-t") 'ry/clock-in-lats)

(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "x" nil) 
     (define-key org-agenda-mode-map "q" 'bury-buffer)

     ;; Undefine C-c [ and C-c ] since this breaks my
     ;; org-agenda files when directories are include It
     ;; expands the files in the directories individually
     (define-key org-mode-map "\C-c[" 'undefined)
     (define-key org-mode-map "\C-c]" 'undefined)
     (define-key org-mode-map "\C-c;" 'undefined)))

(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . ignore)
                                      ("1" . ignore)
                                      ("2" . ignore)
                                      ("3" . ignore)
                                      ("4" . ignore)
                                      ("5" . ignore)
                                      ("6" . ignore)
                                      ("7" . ignore)
                                      ("8" . ignore)
                                      ("9" . ignore)

                                      ("a" . ignore)
                                      ("d" . ignore)
                                      ("h" . ignore)
                                      ("i" . ignore)
                                      ("k" . ignore)
                                      ("l" . ignore)
                                      ("m" . ignore)
                                      ("q" . ignore)
                                      ("r" . ignore)
                                      ("s" . org-save-all-org-buffers)
                                      ("w" . org-refile)
                                      ("x" . ignore)
                                      ("y" . ignore)
                                      ("z" . org-add-note)

                                      ("A" . ignore)
                                      ("B" . ignore)
                                      ("E" . ignore)
                                      ("F" . ignore)
                                      ("G" . ignore)
                                      ("H" . ignore)
                                      ("J" . org-clock-goto)
                                      ("K" . ignore)
                                      ("L" . ignore)
                                      ("M" . ignore)
                                      ("N" . ignore)
                                      ("P" . ignore)
                                      ("Q" . ignore)                                      
                                      ("S" . ignore)
                                      ("T" . ignore)
                                      ("U" . ignore)
                                      ("V" . ignore)
                                      ("W" . ignore)
                                      ("X" . ignore)
                                      ("Y" . ignore)
                                      ("Z" . ignore))))

;; (defun ry/org-mode-hook ()
;;   (local-set-key (kbd "<return>") 'org-return-indent))
;; (add-hook 'org-mode-hook 'ry/org-mode-hook)

;;  Set the org ellipsis to something nicer.
(setq org-ellipsis " ▾")

(defun ry/org-agenda-after-show-hook ()
  "Show the subtree when viewing it from the agenda."
  (org-show-subtree)
  (org-narrow-to-subtree))
(setq org-agenda-after-show-hook 'ry/org-agenda-after-show-hook)

(setq org-clock-goto-hook 'ry/org-agenda-after-show-hook)

(setq org-agenda-span 2)
(setq org-agenda-time-grid
      '((daily today require-timed)
       "----------------"
       (800 1000 1200 1400 1600 1800)))
(setq org-columns-default-format "%50ITEM %12SCHEDULED %TODO %3PRIORITY %Effort{:} %TAGS")

;;  Show recently logged tasks in the time grid.
(setq org-agenda-show-log t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Clock table formatting.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Used for removing unwanted strings from headline strings.


;;  Use the updated function insead.
(setq org-clock-clocktable-formatter 'ry/org-clocktable-write)

(defun ry/org-clocktable-indent-string (level next-level last-p)
  "Return indentation string according to LEVEL.
LEVEL is an integer.  Indent by two dashes per level above 1.
This is a copy of `ry/org-clocktable-indent-string' with the
only change being the usage of UTF-8 characters as arrows."
  (cond ((= level 1) " ■ ")
        ((and (= level 2) (eq nil next-level)) " └──► ")
        ((eq nil next-level) (concat (make-string (1- level) 32) "└" "► "))        
        ((> level next-level) (concat  " │" (make-string (1- level) 32) "└─► "))
        ((and (= level 2) (< level next-level)) (concat  " ├──┬► "))
        ((and (not(= level 2))(< level next-level)) (concat  " │" (make-string (1- level) 32) "└┬► "))
        ((< level next-level) (concat  " │" (make-string (1- level) 32) "┬► "))
        (t (eq nil next-level) (concat " │" (make-string (1- level) 32) "├─► "))))

(defun ry/org-clocktable-write (ipos tables params)
  "Write out a clock table at position IPOS in the current buffer.
TABLES is a list of tables with clocking data as produced by
`org-clock-get-table-data'.  PARAMS is the parameter property list obtained
from the dynamic block definition.  This is an almost exact copy of 
`org-clocktable-write'.  The only additions are to change the indenting 
style to use 'fancy' UTF-8 characters and to remove the TODO and NEXT
keywords from the headlines in the table."
  ;; This function looks quite complicated, mainly because there are a
  ;; lot of options which can add or remove columns.  I have massively
  ;; commented this function, the I hope it is understandable.  If
  ;; someone wants to write their own special formatter, this maybe
  ;; much easier because there can be a fixed format with a
  ;; well-defined number of columns...
  (let* ((hlchars '((1 . "*") (2 . "/")))
	 (lwords (assoc (or (plist-get params :lang)
			    (org-bound-and-true-p org-export-default-language)
			    "en")
			org-clock-clocktable-language-setup))
	 (multifile (plist-get params :multifile))
	 (block (plist-get params :block))
	 (sort (plist-get params :sort))
	 (ts (plist-get params :tstart))
	 (te (plist-get params :tend))
	 (header (plist-get  params :header))
	 (narrow (plist-get params :narrow))
	 (ws (or (plist-get params :wstart) 1))
	 (ms (or (plist-get params :mstart) 1))
	 (link (plist-get params :link))
	 (maxlevel (or (plist-get params :maxlevel) 3))
	 (emph (plist-get params :emphasize))
	 (level-p (plist-get params :level))
	 (org-time-clocksum-use-effort-durations
	  (plist-get params :effort-durations))
	 (timestamp (plist-get params :timestamp))
	 (properties (plist-get params :properties))
	 (ntcol (max 1 (or (plist-get params :tcolumns) 100)))
	 (rm-file-column (plist-get params :one-file-with-archives))
	 (indent (plist-get params :indent))
	 (case-fold-search t)
	 range-text total-time tbl level hlc formula pcol
	 file-time entries entry headline
	 recalc content narrow-cut-p tcol)

    ;; Implement abbreviations
    (when (plist-get params :compact)
      (setq level nil indent t narrow (or narrow '40!) ntcol 1))

    ;; Some consistency test for parameters
    (unless (integerp ntcol)
      (setq params (plist-put params :tcolumns (setq ntcol 100))))

    (when (and narrow (integerp narrow) link)
      ;; We cannot have both integer narrow and link
      (message
       "Using hard narrowing in clocktable to allow for links")
      (setq narrow (intern (format "%d!" narrow))))

    (when narrow
      (cond
       ((integerp narrow))
       ((and (symbolp narrow)
	     (string-match "\\`[0-9]+!\\'" (symbol-name narrow)))
	(setq narrow-cut-p t
	      narrow (string-to-number (substring (symbol-name narrow)
						  0 -1))))
       (t
	(error "Invalid value %s of :narrow property in clock table"
	       narrow))))

    (when block
      ;; Get the range text for the header
      (setq range-text (nth 2 (org-clock-special-range block nil t ws ms))))

    ;; Compute the total time
    (setq total-time (apply '+ (mapcar 'cadr tables)))

    ;; Now we need to output this tsuff
    (goto-char ipos)

    ;; Insert the text *before* the actual table
    (insert-before-markers
     (or header
	 ;; Format the standard header
	 (concat
	  "#+CAPTION: "
	  (nth 9 lwords) " ["
	  (substring
	   (format-time-string (cdr org-time-stamp-formats))
	   1 -1)
	  "]"
	  (if block (concat ", for " range-text ".") "")
	  "\n")))

    ;; Insert the narrowing line
    (when (and narrow (integerp narrow) (not narrow-cut-p))
      (insert-before-markers
       "|"                            ; table line starter
       (if multifile "|" "")          ; file column, maybe
       (if level-p   "|" "")          ; level column, maybe
       (if timestamp "|" "")          ; timestamp column, maybe
       (if properties (make-string (length properties) ?|) "")  ;properties columns, maybe
       (format "<%d>| |\n" narrow)))  ; headline and time columns

    ;; Insert the table header line
    (insert-before-markers
     "|"                              ; table line starter
     (if multifile (concat (nth 1 lwords) "|") "")  ; file column, maybe
     (if level-p   (concat (nth 2 lwords) "|") "")  ; level column, maybe
     (if timestamp (concat (nth 3 lwords) "|") "")  ; timestamp column, maybe
     (if properties (concat (mapconcat 'identity properties "|") "|") "") ;properties columns, maybe
     (concat (nth 4 lwords) "|"
	     (nth 5 lwords) "|\n"))                 ; headline and time columns

    ;; Insert the total time in the table
    (insert-before-markers
     "|-\n"                            ; a hline
     "|"                               ; table line starter
     (if multifile (concat "| " (nth 6 lwords) " ") "")
					; file column, maybe
     (if level-p   "|"      "")        ; level column, maybe
     (if timestamp "|"      "")        ; timestamp column, maybe
     (if properties (make-string (length properties) ?|) "")  ; properties columns, maybe
     (concat (format org-clock-total-time-cell-format (nth 7 lwords))  "| ") ; instead of a headline
     (format org-clock-total-time-cell-format
	     (org-minutes-to-clocksum-string (or total-time 0))) ; the time
     "|\n")                          ; close line

    ;; Now iterate over the tables and insert the data
    ;; but only if any time has been collected
    (when (and total-time (> total-time 0))

      (while (setq tbl (pop tables))
	;; now tbl is the table resulting from one file.
	(setq file-time (nth 1 tbl))
	(when (or (and file-time (> file-time 0))
		  (not (plist-get params :fileskip0)))
	  (insert-before-markers "|-\n")  ; a hline because a new file starts
	  ;; First the file time, if we have multiple files
	  (when multifile
	    ;; Summarize the time collected from this file
	    (insert-before-markers
	     (format (concat "| %s %s | %s%s"
			     (format org-clock-file-time-cell-format (nth 8 lwords))
			     " | *%s*|\n")
		     (file-name-nondirectory (car tbl))
		     (if level-p   "| " "") ; level column, maybe
		     (if timestamp "| " "") ; timestamp column, maybe
		     (if properties (make-string (length properties) ?|) "")  ;properties columns, maybe
		     (org-minutes-to-clocksum-string (nth 1 tbl))))) ; the time

	  ;; Get the list of node entries and iterate over it
	  (setq entries (nth 2 tbl))
	  (while (setq entry (pop entries))
	    (setq level (car entry)
		  headline (s-replace-all '(("TODO ". "") ("NEXT " . "") ("DONE " . "")) (nth 1 entry))
		  hlc (if emph (or (cdr (assoc level hlchars)) "") ""))
	    (when narrow-cut-p
	      (if (and (string-match (concat "\\`" org-bracket-link-regexp
					     "\\'")
				     headline)
		       (match-end 3))
		  (setq headline
			(format "[[%s][%s]]"
				(match-string 1 headline)
				(org-shorten-string (match-string 3 headline)
						    narrow)))
		(setq headline (org-shorten-string headline narrow))))
	    (insert-before-markers
	     "|"                      ; start the table line
	     (if multifile "|" "")    ; free space for file name column?
	     (if level-p (format "%d|" (car entry)) "")   ; level, maybe
	     (if timestamp (concat (nth 2 entry) "|") "") ; timestamp, maybe
	     (if properties
		 (concat
		  (mapconcat
		   (lambda (p) (or (cdr (assoc p (nth 4 entry))) ""))
		   properties "|") "|") "")  ;properties columns, maybe
	     (if indent
                 (progn
                   (setq next-entry (car entries))
                   (setq next-level (car next-entry))
                   (ry/org-clocktable-indent-string level next-level nil))
                 ;; (if (= (length entries) 0)
                 ;;     (ry/org-clocktable-indent-string level t)
                 ;;   (ry/org-clocktable-indent-string level nil))
               "") ; indentation
	     hlc headline hlc "|"                                ; headline
	     (make-string (min (1- ntcol) (or (- level 1))) ?|)
					; empty fields for higher levels
	     hlc (org-minutes-to-clocksum-string (nth 3 entry)) hlc ; time
	     "|\n"                                               ; close line
	     )))))
    ;; When exporting subtrees or regions the region might be
    ;; activated, so let's disable ̀delete-active-region'
    (let ((delete-active-region nil)) (backward-delete-char 1))
    (if (setq formula (plist-get params :formula))
	(cond
	 ((eq formula '%)
	  ;; compute the column where the % numbers need to go
	  (setq pcol (+ 2
			(if multifile 1 0)
			(if level-p 1 0)
			(if timestamp 1 0)
			(min maxlevel (or ntcol 100))))
	  ;; compute the column where the total time is
	  (setq tcol (+ 2
			(if multifile 1 0)
			(if level-p 1 0)
			(if timestamp 1 0)))
	  (insert
	   (format
	    "\n#+TBLFM: $%d='(org-clock-time%% @%d$%d $%d..$%d);%%.1f"
	    pcol            ; the column where the % numbers should go
	    (if (and narrow (not narrow-cut-p)) 3 2) ; row of the total time
	    tcol            ; column of the total time
	    tcol (1- pcol)  ; range of columns where times can be found
	    ))
	  (setq recalc t))
	 ((stringp formula)
	  (insert "\n#+TBLFM: " formula)
	  (setq recalc t))
	 (t (error "Invalid formula in clocktable")))
      ;; Should we rescue an old formula?
      (when (stringp (setq content (plist-get params :content)))
	(when (string-match "^\\([ \t]*#\\+tblfm:.*\\)" content)
	  (setq recalc t)
	  (insert "\n" (match-string 1 (plist-get params :content)))
	  (beginning-of-line 0))))
    ;; Back to beginning, align the table, recalculate if necessary
    (goto-char ipos)
    (skip-chars-forward "^|")
    (org-table-align)
    (when org-hide-emphasis-markers
      ;; we need to align a second time
      (org-table-align))
    (when sort
      (save-excursion
	(org-table-goto-line 3)
	(org-table-goto-column (car sort))
	(org-table-sort-lines nil (cdr sort))))
    (when recalc
      (if (eq formula '%)
	  (save-excursion
	    (if (and narrow (not narrow-cut-p)) (beginning-of-line 2))
	    (org-table-goto-column pcol nil 'force)
	    (insert "%")))
      (org-table-recalculate 'all))
    (when rm-file-column
      ;; The file column is actually not wanted
      (forward-char 1)
      (org-table-delete-column))
    total-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Only load the work related outlook capture handler if I am at work.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (s-contains? "RAD" (system-name))
  (load "~/.emacs.d/org-outlook-capture.el"))

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

(org-clock-persistence-insinuate)

;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)

;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))

;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

;; Check for clocking longer than four hours.
(setq org-agenda-clock-consistency-checks
      '(:max-duration "4:00"
        :min-duration 0
        :max-gap 0
        :gap-ok-around ("4:00")))

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

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

 (defun bh/punch-in (arg)
   "Start continuous clocking and set the default task to the
 selected task.  If no task is selected set the Organization task
 as the default task."
   (interactive "p")
   (setq bh/keep-clock-running t)
   (if (equal major-mode 'org-agenda-mode)
      
      ;; We're in the agenda
      
       (let* ((marker (org-get-at-bol 'org-hd-marker))
              (tags (org-with-point-at marker (org-get-tags-at))))
         (if (and (eq arg 4) tags)
             (org-agenda-clock-in '(16))
           (bh/clock-in-organization-task-as-default)))
    
    ;; We are not in the agenda
    
     (save-restriction
       (widen)
       ; Find the tags on the current task
       (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
           (org-clock-in '(16))
         (bh/clock-in-organization-task-as-default))))      
   )

;; (defun bh/punch-in (arg)
;;   "Start continuous clocking and set the Organization task
;; as the default task."
;;   (interactive "p")
;;   (setq bh/keep-clock-running t)
;;   (bh/clock-in-organization-task-as-default))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")
(defvar ry/email-task-id "b86a72cd-99f1-435f-969c-dacd4a54b6b7")
(defvar ry/lats-task-id "81a916f6-59b6-4b5d-a381-211ec3388b14")

;; (org-id-find-id-file "8d2cd2aa-b608-4466-837f-c537cbaa14df")
;; (org-id-find-id-in-file "8d2cd2aa-b608-4466-837f-c537cbaa14df" "~/emacs/org/refile.org" 'marker)

;; (setq org-clock-default-task (org-id-find bh/organization-task-id 'marker))
;; (setq org-clock-default-task (org-id-find-id-in-file "8d2cd2aa-b608-4466-837f-c537cbaa14df" "~/emacs/org/refile.org" 'markerp))

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(defun ry/clock-in-email ()
  "Clock in the email task."
  (interactive)
  (org-with-point-at (org-id-find ry/email-task-id 'marker)
    (org-clock-in '(16))))

(defun ry/clock-in-lats ()
  "Clock in the email task."
  (interactive)
  (org-with-point-at (org-id-find ry/lats-task-id 'marker)
    (org-clock-in '(16))))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))

;;  Set files that should be used as part of the adgenda.
(setq org-agenda-files (quote ("~/emacs/Org/refile.org"                             
			       "~/emacs/Org/das.org")))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "MIGRATED(m)" "|" "DONE(d)" "RESCHEDULED(r)")
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
      (quote (("t" "todo" entry (file+headline "~/emacs/Org/refile.org" "Refile")
	       "* TODO %?\n  %U\n" :clock-in t :clock-resume t)
	      ("r" "respond" entry (file+headline "~/emacs/Org/refile.org" "Refile")
	       "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
	      ("n" "note" entry (file+headline "~/emacs/Org/refile.org" "Refile")
	       "* %?\n  %U\n" :clock-in t :clock-resume t)
	      ("w" "org-protocol" entry (file+headline "~/emacs/Org/refile.org" "Refile")
	       "* TODO Review %c\n%U\n" :immediate-finish t)
	      ("h" "Habit" entry (file+headline "~/emacs/Org/refile.org" "Refile")
	       "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
	      ("m" "Meeting" entry (file+headline "~/emacs/Org/refile.org" "Refile")
	       "* TODO %?\nSCHEDULED: %t\n%U\n- Room :: \n- Time :: \n- Attendees ::\n  - [ ] \n- Background :: \n  -\n- Meeting notes :: \n  -"))))

;;  Remove this on 09/04/2015 as it was causing problems with a new version of org-mode.
;;(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

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
		(tags-todo "REFILE"
		      ((org-agenda-overriding-header "Tasks to Refile")
		       (org-tags-match-list-sublevels nil)))
		(tags-todo "-HOLD-CANCELLED/!"
			   ((org-agenda-overriding-header "Projects")
			    (org-agenda-skip-function 'bh/skip-non-projects)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "-HOLD-CANCELLED/!"
			   ((org-agenda-overriding-header "Stuck Projects")
			    (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
		(tags-todo "-WAITING-CANCELLED-KRANTHI-RITA-ESWARA-TIFFANY-NICK-REFILE/!NEXT"
			   ((org-agenda-overriding-header "Next Tasks")
			    (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
			    (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-todo-ignore-deadlines t)
			    (org-agenda-todo-ignore-with-date t)
			    (org-tags-match-list-sublevels t)
			    (org-agenda-sorting-strategy
			     '(todo-state-down effort-up category-keep))))
		(tags-todo "-WAITING-REFILE-CANCELLED-KRANTHI-RITA-ESWARA-TIFFANY-NICK/!-HOLD-WAITING-NEXT-MIGRATED-KRANTHI-RITA-ESWARA-TIFFANY-NICK"
			   ((org-agenda-overriding-header "Tasks")
			    (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
			    (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-todo-ignore-deadlines t)
			    (org-agenda-todo-ignore-with-date t)
			    (org-agenda-sorting-strategy
			     '(category-keep))))		
                (tags-todo "TIFFANY/-MIGRATED"
			   ((org-agenda-overriding-header "Tiffany H.")
                            (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
                (tags-todo "NICK/-MIGRATED"
			   ((org-agenda-overriding-header "Nick H.")
                            (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-sorting-strategy
			     '(category-keep))))                                
                (tags-todo "RITA/-MIGRATED"
			   ((org-agenda-overriding-header "Rita J.")
                            (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
                (tags-todo "KRANTHI/-MIGRATED"
			   ((org-agenda-overriding-header "Kranthi G.")
                            (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
                (tags-todo "Eswara/-MIGRATED"
			   ((org-agenda-overriding-header "Eswara M.")
                            (org-agenda-todo-ignore-scheduled t)
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
         (java . t)
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
;; (setq org-agenda-tags-column -130)
(setq org-agenda-tags-column -80)

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
(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree))

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
       ;; ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
       ;;  next-headline)
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

(defvar bh/current-view-project nil)

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
