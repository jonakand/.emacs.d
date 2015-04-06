;;; emacs-config.el --- Emacs configuration
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Setup the mode-line with a condensed amount of information.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default mode-line-format
             '("%e" ; print error message about full memory.
               mode-line-front-space
               ; mode-line-mule-info
               ; mode-line-client
               ; mode-line-modified
               ; mode-line-remote
               ; mode-line-frame-identification
               "%b" ;;mode-line-buffer-identification
               "   "
               mode-line-position
               (vc-mode vc-mode)
               "  "
               mode-line-modes
               "   "
               ; mode-line-misc-info
               display-time-string
               "   "
               ;battery-mode-line-string
               mode-line-end-spaces))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Enable some functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(put 'narrow-to-region 'disabled nil) 
(put 'narrow-to-page 'disabled nil) 
(put 'narrow-to-defun 'disabled nil) 

(put 'erase-buffer 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Make apropo do more.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq apropos-do-all t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Only GC every 20 MB
;;  From: https://github.com/CQQL/dotfiles/blob/master/src/.emacs.d/lisp/globals.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gc-cons-threshold 20000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Modify how scrolling happens.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scroll-margin 5)
(setq scroll-preserve-screen-position 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Change the way recenter behaves so that TOP is the first one instead of
;;  middle.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq recenter-positions '(top middle bottom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show the function the cursor is currently in in the status line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-function-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete the selection with a key press.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(delete-selection-mode t) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove message when killing a buffer in server mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable file dialog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq use-file-dialog nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "y or n" instead of "yes or no"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight regions and add special behaviors to regions.
;; "C-h d transient" for more info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq transient-mark-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make it so that slections work like in other editors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(delete-selection-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show-paren-mode: subtle highlighting of matching parens (global-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn off cursor blink
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(blink-cursor-mode 0) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Set the font to something nice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :font "Consolas-11:antialias=subpixel"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Set the exec-path so Emacs can find programs not on the windows path.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (eq system-type 'windows-nt)
    (setq exec-path
          (append exec-path
                  '("~/emacs/Graphviz/bin"
                    "~/emacs/Hunspell/bin/"
                    "~/emacs/Gnutls/bin"
                    "~/emacs/Leiningen"
                    "~/Git/bin"
                    "C:/IBM/SDP"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set a couple environment values so that ispell can automatically load the
;; correct dictionary.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (eq system-type 'windows-nt)
    (progn
      (setenv "DICTIONARY" "en_US")
      (setenv "DICPATH" "~/emacs/Hunspell/share/hunspell")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphviz needs an environment value set so it can locate dot.exe as PATH is
;; not used.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (eq system-type 'windows-nt)
    (setenv "GRAPHVIZ_DOT" "~/emacs/Graphviz/bin/dot.exe"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  So gnutls can find trustfiles on windows simply setting the path for the
;;  trust files doesn't seem to work.  This needs to be the full path or it will
;;  not work.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (eq system-type 'windows-nt)
    (eval-after-load "gnutls" 
      '(progn 
         (setq gnutls-trustfiles '("h:/emacs/cacert.pem")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  DB2 related setup
;;  Taken from http://www.ibm.com/developerworks/data/library/techarticle/0206mathew/0206mathew.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (eq system-type 'windows-nt)
    (setq sql-db2-program "C:/PROGRA~2/IBM/SQLLIB/BIN/db2cmd.exe"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;  -t - ';' (semicolon) is treated as the command line terminator. 
;;  +ec - Print SQLCODE.
;;  +m - Print number of rows affected by statement.
;;  Taken from http://www.ibm.com/developerworks/data/library/techarticle/0206mathew/0206mathew.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (eq system-type 'windows-nt)
    (setq sql-db2-options '("-c" "-i" "-w" "db2setcp.bat" "db2" "-tv" "-ec" "-m")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Turn off line wrapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-default 'truncate-lines t)
;; (global-visual-line-mode)
;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Increase the threshold size to 100MB.  Need this as log files and test files
;;  tend to be large.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-default 'large-file-warning-threshold 1000000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save temp files in c:/temp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/BACKUP")))
(setq auto-save-file-name-transforms
      `((".*" , "~/.emacs.d/BACKUP" t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Indicate empty lines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq indicate-empty-lines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Use four spaces inplace of tab characters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Auto revert the buffer if the underlying file has changed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-auto-revert-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Hunspell
;; rw-hunspell is no longer used as Emacs 24.4 made Hunspell integration very
;; easy.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ispell-personal-dictionary "~/emacs/Config/en_US_personal")
(setq ispell-silently-savep t)
(setq ispell-quietly t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Add hook to Java and emacs lisp mode to spellcheck comments.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'java-mode-hook
          (lambda ()
            (flyspell-prog-mode)))
            
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (flyspell-prog-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This suggestion came from the emacse wiki as a way to speedup flyspell.
;;  http://www.emacswiki.org/emacs/FlySpell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq flyspell-issue-message-flag nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Setup URL browsing based on the system type.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (if (eq system-type 'windows-nt)
                                     "C:/Program Files/Internet Explorer/iexplore.exe"))

(global-set-key (kbd "C-c B") 'browse-url-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fix what the return key does to work the way that I think it should.  When
;;  it is pressed move to the next line and also indent.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "RET") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Bindings for changing window sizes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Key bindingd for custom functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x r M-w") 'my-copy-rectangle)
(global-set-key (kbd "C-x r C-y") 'yank-replace-rectangle)
(global-set-key (kbd "<f12>") 'ry/open-temp-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SQL related bindings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f5>") 'ry/sql-send-paragraph)
(global-set-key (kbd "<S-f5>") 'ry/sql-open-database)
(global-set-key (kbd "<C-f5>") 'ry/sql-send-export-paragraph)
(global-set-key (kbd "<M-f5>") 'ry/sql-connect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  I dont use tags so this was poached from those keys.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-.") 'find-function-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Spell checking keys.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f1>") 'ry/flyspell-check-previous-highlighted-word)
(global-set-key (kbd "<f2>") 'flyspell-correct-word-before-point)
(global-set-key (kbd "<f3>") 'ry/flyspell-check-next-highlighted-word)
(global-set-key (kbd "<f4>") 'ispell-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  In my workflow only the current buffer is killed, so this mapping is done
;;  instead of the default one.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Remap move-beginning-of-line to prelude-move-beginning-of-line so that it
;;  puts the cursor to the first non whitepace character or beginning of the
;;  line if pressed again.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [remap move-beginning-of-line] 'prelude-move-beginning-of-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Kill buffers for dired mode and package menu mode instead of burying them.
;;  Taken from : https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "dired"
  (progn
    '(bind-keys :map dired-mode-map
                ("q" . kill-this-buffer))))

(bind-keys :map package-menu-mode-map
           ("q" . kill-this-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Small function to remove all ^M characters from a file.  Taken from:
;;  http://www.archivum.info/comp.emacs/2007-06/00348/Re-Ignore-%5EM-in-mixed-%28LF-and-CR+LF%29-line-ended-textfiles.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun ry/replace-character-at-point (new-char)
  "Replace the character at point in the same way that the command works in vim"
  (interactive "c")
  (delete-char 1)
  (insert new-char)
  (backward-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Helper for copying a rectangle.
;;  Taken from http://www.emacswiki.org/emacs/RectangleCommands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-copy-rectangle (start end)
  "Copy the region-rectangle instead of `kill-rectangle'."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Helper for replacing/yanking one rectangle with another.
;;  Taken from http://www.emacswiki.org/emacs/RectangleCommands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yank-replace-rectangle (start end)
  "Similar like yank-rectangle, but deletes selected rectangle first."
  (interactive "r")
  (delete-rectangle start end)
  (pop-to-mark-command)
  (yank-rectangle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Function to open a *TEMP#* buffer based on the numeric argument passed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ry/open-temp-buffer (&optional num)
  "Open a numbered *TEMP#* buffer based on argument."
  (interactive "p")
  (switch-to-buffer
   (format "*TEMP%d*" num))
  (god-local-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Open a DB2 SQLI window.  This probably isn't a super way of doing this as
;;  the SQLi hook is not being called due to the way the sql-buffer is being set
;;  but for now it works.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ry/sql-open-database (database username password)
  "Open a SQLI process and name the SQL statement window with the name provided."
  (interactive (list
                (read-string "Database: ")
                (read-string "Username: ")
                (read-passwd "Password: ")))
  (switch-to-buffer "*DB_HELPER*")
  (god-local-mode)
  (insert-file-contents "H:/emacs/Config/DB_INFO.TXT")
  (setq sql-set-product "db2")
  
  (sql-db2 (upcase database))
  (sql-rename-buffer (upcase database))
  (setq sql-buffer (current-buffer))
  (sql-send-string (concat "CONNECT TO " database " USER " username " USING " password ";"))
  
  (other-window 1)
  (switch-to-buffer (concat "*DB:" (upcase database) "*"))
  (sql-mode)
  (sql-set-product "db2")
  (setq sql-buffer (concat "*SQL: " (upcase database) "*"))
  (auto-complete-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Connect to a DB2 database.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ry/sql-connect (database username password)
  "Custom SQL connect"
  (interactive (list
                (read-string "Database: ")
                (read-string "Username: ")
                (read-passwd "Password: ")))
  (sql-send-string (concat "CONNECT TO " database " USER " username " USING " password ";")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Sense I have a hard time remembering to limit my queries this method was
;;  created to do it for me.  Simply append the FETCH FIRST clause to the SQL
;;  statement prior to sending the paragraph.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ry/sql-send-paragraph ()
  "Add FETCH FIRST clause to the SQL statement prior to sending"
  (interactive)
  (let ((start (save-excursion
                 (backward-paragraph)
                 (point)))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    (save-restriction
      (narrow-to-region start end)
      (if (not (search-forward-regexp "select" nil t))
          (if (not (search-forward-regexp "fetch" nil t))
              (sql-send-string (buffer-substring-no-properties start end))
            (sql-send-string (concat (buffer-substring-no-properties start (1- end)) " FETCH FIRST 50 ROWS ONLY WITH UR;")))
        (sql-send-string (buffer-substring-no-properties start end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Prefix the region with an EXPORT command and send it to the DB2 process.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ry/sql-send-export-paragraph ()
  "Prefix the current paragraph with an EXPORT command and 
send the paragraph to the SQL process."
  (interactive)
  (let ((start (save-excursion
                 (backward-paragraph)
                 (point)))
        (end (save-excursion
               (forward-paragraph)
               (point)))
        (temp-file
         (make-temp-file "DB2-EXPORT-" nil)))
    (sql-send-string (concat "EXPORT TO " temp-file " OF DEL MODIFIED BY COLDEL0x09 " (buffer-substring-no-properties start end)))
    (switch-to-buffer "*EXPORT*")
    (sleep-for 1)
    (insert-file-contents-literally (concat temp-file ".001.xml"))
    (goto-char (point-min))
    (while (re-search-forward "<\\?xml version=\"1.0\" encoding=\"UTF-8\" \\?>" nil t)
      (replace-match "\n  <?xml version=\"1.0\" encoding=\"UTF-8\" ?>" nil nil))
    (goto-char (point-min))
    (kill-line)
    (goto-longest-line (point-min) (point-max))
    (let ((max-length (- (line-end-position) (line-beginning-position))))
      (goto-char (point-min))
      (setq more-lines t)
      (while more-lines
     	(setq cur-length (- (line-end-position) (line-beginning-position)))
     	(if (< cur-length max-length)
     	    (progn 
     	      (goto-char (line-end-position))
     	      (insert-char 32 (- max-length cur-length))))
     	(setq more-lines (= 0 (forward-line 1)))))
    (kill-rectangle (point-min) (point-max))
    (erase-buffer)
    (insert-file-contents-literally temp-file)
    (while (re-search-forward "\"<XDS\.\*\$" nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))
    (while (re-search-forward "\"" nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))
    (goto-char (line-end-position))
    (yank-rectangle)
    (god-local-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Taken from Prelude.  Make the point move to the beginning of the line in the
;;  same way Eclipse does.  Really move back-and-forth between the hard
;;  beginning of the line and the first non-space character.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Functions for formatting XML documents.  These will call an external Java
;;  program to handle the formatting as that is my bread-and-butter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ry/xml-format (beg end)
  "Call an external Java program to format the current region
as an XML document.  Region needs to contain a valid XML document."
  (interactive "*r")
  (save-excursion
    (shell-command-on-region beg end "java -jar H:/emacs/Java/XMLFormatter.jar --pretty" (current-buffer) t)))

(defun ry/xml-linearlize (beg end)
  "Call an external Java program to linearlize the current region.  
Region needs to contain a valid XML document."
  (interactive "*r")
  (save-excursion
    (shell-command-on-region beg end "java -jar H:/emacs/Java/XMLFormatter.jar " (current-buffer) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Perform an XQUERY on the selected buffer/region.
;;  This function was taken from http://donnieknows.com/blog/hacking-xquery-emacs-berkeley-db-xml
;;  and modified to work with Saxon instead of Berkley DB.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ry/xquery ()
  "Perform Xquery using Saxon with the current buffer."
  (interactive "")
  (let ((beg (point-min))
        (end (point-max)))
    (ry/xquery-with-region beg end)))

(defun ry/xquery-with-region (beg end)
  "Perform Xquery using Saxon with the current region."
  (interactive "*r")
  (let ((newbuffer nil)
        (buffer (get-buffer "xquery-result"))
        (xquery (buffer-substring beg end)))
    (setq xquery-result
          (cond
           ((buffer-live-p buffer) buffer)
           (t (setq newbuffer t) (generate-new-buffer "xquery-result"))))
    (with-current-buffer xquery-result
      (with-timeout
          (10 (insert "Gave up because query was taking too long."))
        (erase-buffer)
        (insert (ry/perform-xquery xquery t)))
      (nxml-mode)
      (goto-char (point-min)))
    (switch-to-buffer-other-window xquery-result)
    (other-window -1)))

(defun ry/perform-xquery (xquery &optional timed)
  "Perform the selected Xquery using Saxon."
  (setq file (make-temp-file "elisp-dbxml-"))
  (write-region xquery nil file)
  (setq result (shell-command-to-string
                (concat "java -cp H:/emacs/Java/saxon9he.jar net.sf.saxon.Query -q:\"" file "\" !indent=yes\n")))
  (delete-file file)
  (concat "" result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Simple little function for switching buffers.  First enable god-mode for the
;;  buffer being left so that it is on when coming back.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ry/switch-buffer ()
  "Function to switch the current buffer to God-mode
 prior to switching buffers."
  (interactive)
  (god-local-mode 1)
  (helm-mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Make window splitting split with the previous buffer instead of using the
;;  same buffer for the new window.
;;  Taken from http://pages.sachachua.com/.emacs.d/Sacha.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sacha/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (unless prefix
    (switch-to-next-buffer)))

(defun sacha/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (unless prefix (switch-to-next-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Kill buffers other than the current one.
;;  http://emacsredux.com/blog/2013/03/30/kill-other-buffers/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Kill the current buffer without prompting.
;; http://www.masteringemacs.org/articles/2014/02/28/my-emacs-keybindings/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-this-buffer () 
  "Kill the current buffer without prompting."  
  (interactive) 
  (kill-buffer (current-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This is again from the emacs wiki.  This will cause a popup to be used
;;  instead of having a new window to be opened when checking the current word.
;;  Typically I will correct words as they are found instead of doing a whole
;;  buffer at a time so this will allow me to do that without having the
;;  display thrash with.  
;;  http://www.emacswiki.org/emacs/FlySpell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flyspell-emacs-popup-textual (event poss word)
  "A textual flyspell popup menu."
  (require 'popup)
  (let* ((corrects (if flyspell-sort-corrections
                       (sort (car (cdr (cdr poss))) 'string<)
                     (car (cdr (cdr poss)))))
         (cor-menu (if (consp corrects)
                       (mapcar (lambda (correct)
                                 (list correct correct))
                               corrects)
                     '()))
         (affix (car (cdr (cdr (cdr poss)))))
         show-affix-info
         (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                     (list
                                      (list (concat "Save affix: " (car affix))
                                            'save)
                                      '("Accept (session)" session)
                                      '("Accept (buffer)" buffer))
                                   '(("Save word" save)
                                     ("Accept (session)" session)
                                     ("Accept (buffer)" buffer)))))
                       (if (consp cor-menu)
                           (append cor-menu (cons "" save))
                         save)))
         (menu (mapcar
                (lambda (arg) (if (consp arg) (car arg) arg))
                base-menu)))
    (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))

(eval-after-load "flyspell"
  '(progn
     (fset 'flyspell-emacs-popup 'flyspell-emacs-popup-textual)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Check the next misspelled word and display the popup instead of splitting
;;  the window.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ry/flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (flyspell-correct-word-before-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This function is taken from the flyspell code with only a minor modification
;;  made.  It calls the flyspell correct word before point instead of ispell
;;  word.  Doing this will display the corrections in a popup instead of by
;;  splitting the window.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ry/flyspell-check-previous-highlighted-word (&optional arg)
  "Correct the closer misspelled word.
This function scans a mis-spelled word before the cursor. If it finds one
it proposes replacement for that word. With prefix arg, count that many
misspelled words backwards."
  (interactive)
  (let ((pos1 (point))
	(pos  (point))
	(arg  (if (or (not (numberp arg)) (< arg 1)) 1 arg))
	ov ovs)
    (if (catch 'exit
	  (while (and (setq pos (previous-overlay-change pos))
		      (not (= pos pos1)))
	    (setq pos1 pos)
	    (if (> pos (point-min))
		(progn
		  (setq ovs (overlays-at (1- pos)))
		  (while (consp ovs)
		    (setq ov (car ovs))
		    (setq ovs (cdr ovs))
		    (if (and (flyspell-overlay-p ov)
			     (= 0 (setq arg (1- arg))))
			(throw 'exit t)))))))
	(save-excursion
	  (goto-char pos)
	  ;; (ispell-word)
      (flyspell-correct-word-before-point)
	  (setq flyspell-word-cache-word nil) ;; Force flyspell-word re-check
	  (flyspell-word))
      (error "No word to correct before point"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Narrow or widen intelligenetly.
;;  Taken from : https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows
intelligently.  Intelligently means: region, org-src-block,
org-subtree, or defun, whichever applies first.  Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((and (boundp 'org-src-mode) org-src-mode (not p))
         (org-edit-src-exit))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'prog-mode) (narrow-to-defun))
        (t (error "Please select a region to narrow to"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This is another one of those little functions that is handy in certain
;;  situations but I'm not sure if I will remember that it is available in the
;;  future.  Time will tell.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xah/copy-file-path (&optional φdir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
If `universal-argument' is called, copy only the dir path.
Version 2015-01-14
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'"
  (interactive "P")
  (let ((fPath
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name))))
    (kill-new
     (if (equal φdir-path-only-p nil)
         fPath
       (file-name-directory fPath)))
    (message "File path copied: 「%s」" fPath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  A couple helper functions for launching applications when on Windows.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ry/launch-windows-explorer ()
  "Open Windows explorer."
  (interactive)
  (if (eq system-type 'windows-nt)
      (shell-command "explorer.exe")
    (error "This command can only be used on Windows.")))

(defun ry/launch-internet-explorer ()
  "Open Internet Explorer."
  (interactive)
  (if (eq system-type 'windows-nt)      
      (shell-command "C:/Progra~1/Intern~1/iexplore.exe https://www.bing.com")
    (error "This command can only be used on Windows.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  The function below is a modified version of a function found at:
;;  http://www.emacswiki.org/emacs/NxmlMode#toc11.  In additional to displaying
;;  the current XPATH in the echo area it will be copied to the clipboard.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ry/xml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (kill-new (format "/%s" (mapconcat 'identity path "/")))
        (message "XPath copied: 「%s」" (mapconcat 'identity path "/"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Get things ready for async package installation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package async
  :ensure t)

(use-package paradox
  :ensure t
  :config
  (progn
    (setq paradox-execute-asynchronously t)
    (setq paradox-github-token t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Remove unnecessary modes from the modeline.  Probably not needed as
;;  use-package should handle this.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diminish
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Load the sollarized dark theme.  The package is from MELPA.  I liike to have
;;  the mode-line in different colors to make the active/inactive window more
;;  easily found.  The light gray is pleasing.  Also add the different faces for
;;  org blocks so that they stand out more.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package solarized-theme
  :ensure t
  :config
  (progn
    (load-theme 'solarized-dark t)
    (set-face-attribute 'mode-line nil
                        :inverse-video t
                        :weight 'bold
                        :overline nil
                        :underline nil
                        :box nil
                        :foreground "#93a1a1"
                        :background "#073642")
    (set-face-attribute 'mode-line-inactive nil
                        :inverse-video t
                        :weight 'bold
                        :overline nil
                        :underline nil
                        :box nil
                        :foreground "#657b83"
                        :background "#073642")
    (with-eval-after-load 'org
      (set-face-attribute 'org-block-begin-line nil
                          :underline t
                          :background "#073642")
      (set-face-attribute 'org-block-end-line nil
                          :overline t
                          :background "#073642")
      (set-face-attribute 'org-block-background nil
                          :background "#073642"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Rainbox delimiter mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t 
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Enable smartscan mode.  M-n and M-p to move to next/previous 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartscan
  :ensure t
  :commands (smartscan-symbol-go-forward smartscan-symbol-go-backward highlight-symbol-first)
  :config
  (progn
    (global-smartscan-mode)
    (defun highlight-symbol-first ()
      "Jump to the first location of symbol at point."
      (interactive)
      (push-mark)
      (eval
       `(progn
          (goto-char (point-min))
          (search-forward-regexp
           (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
           nil t)
          (beginning-of-thing 'symbol))))
    (bind-keys ("M-P" . highlight-symbol-first))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  NXML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package nxml-mode
  :commands nxml-mode
  :config
  (progn
    (setq nxml-child-indent 2)
    (setq nxml-attribute-indent 4)
    (setq nxml-auto-insert-xml-declaration-flag nil)
    (setq nxml-slash-auto-complete-flag t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web-mode initialization stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :ensure t
  :mode 
  (("\\.phtml\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.jsp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode))
  :init
  (setq web-mode-enable-auto-pairing nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("C-M-s" . helm-occur)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x r l" . helm-bookmarks)
         ("C-h f" . helm-apropos)
         ("C-h r" . helm-info-emacs)
         ("C-h C-l" . helm-locate-library)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-b" . helm-mini)
         ("<f7>" . helm-bookmarks)
         ("<f8>" . bookmark-set))
  :config
  (progn
    (require 'helm-config)

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (setq helm-quick-update                     t
          helm-split-window-in-side-p           t
          helm-buffers-fuzzy-matching           t
          helm-ff-search-library-in-sexp        t
          helm-scroll-amount                    8
          helm-ff-file-name-history-use-recentf t)

    (setq helm-mini-default-sources '(helm-source-buffers-list
                                      helm-source-recentf
                                      helm-source-bookmarks
                                      helm-source-buffer-not-found))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;  Make helm always create a new window and always split the current window
    ;;  vertically.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq helm-display-function
          (lambda (buf)
            (split-window-vertically)
            (other-window 1)
            (switch-to-buffer buf)))

    (helm-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable multiple cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-s C-S-s" . mc/edit-lines)
         ("C->" . mc/mark-next-symbol-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c *" . mc/mark-all-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GURU mode 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package guru-mode
  :ensure t
  :init
  (guru-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Expand region by symantic units.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Eldoc mode for lisp coding.
;;  http://pages.sachachua.com/.emacs.d/Sacha.html#unnumbered-47
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package "eldoc"  
  :diminish eldoc-mode
  :init
  (progn
    (setq eldoc-idle-delay 0.2)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add org mode stuff to the load path.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure t 
  :config
  (load "~/.emacs.d/custom-org-mode.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  undo-tree config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :commands undo-tree-visualize
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  GOD mode config with some keymaps to make it easier to use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package god-mode
  :ensure t
  ;;:diminish god-mode                   
  :bind (("<escape>" . god-local-mode)
         ("C-x C-1" . delete-other-windows)
         ("C-x C-2" . sacha/vsplit-last-buffer)
         ("C-x C-3" . sacha/hsplit-last-buffer)
         ("C-x C-0" . delete-window)
         ("C-x C-k" . kill-this-buffer)
         ("C-x C-S-k" . kill-other-buffers)
         ("C-x C-o" . other-window)
         ("C-c C-r" . org-capture)
         ("C-C C-a" . org-agenda)
         ("C-x C-h" . mark-whole-buffer)
         ("C-x C-d" . dired)
         ("C-x C-g" . magit-status)
         ("C-c C-g" . magit-status)
         ("C-c C-t" . hydra-toggle/body)
         ("C-c C-l" . hydra-launch/body))
  :init
  (progn
    (god-mode-all)
    
    (define-key god-local-mode-map (kbd "z") 'repeat)
    (define-key god-local-mode-map (kbd "i") 'god-local-mode)
    (define-key god-local-mode-map (kbd "v") 'scroll-up-command)
    (define-key god-local-mode-map (kbd "r") 'ry/replace-character-at-point)
    
    (add-to-list 'god-exempt-major-modes 'dired-mode)
    (add-to-list 'god-exempt-major-modes 'org-agenda-mode)
    (add-to-list 'god-exempt-major-modes 'org-capture-mode)

    ;;  Change the color of the cursor to RED if god-mode is enabled.
    (add-hook 'post-command-hook
              (lambda ()
                (if (or god-local-mode buffer-read-only)
                    (progn                   
                      (set-cursor-color "#b22222")
                      (setq cursor-type 'box))
                  (progn
                    (set-cursor-color "#839496")
                    (setq cursor-type 'bar)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Init golden ratio mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
  (progn 
    (golden-ratio-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (progn
    (use-package company-quickhelp
      :ensure t
      :config
      (progn
        (company-quickhelp-mode 1)
        (setq company-quickhelp-delay 0.1)))
    (global-company-mode)
    (setq company-idle-delay 0.5)
    (setq company-show-numbers t)
    (setq company-selection-wrap-around t)
    (setq company-tooltip-limit 10)
    (setq company-tooltip-flip-when-above t)
    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-ignore-invisible t)
    (setq company-dabbrev-ignore-buffers "\\`[ ]'")
    (setq company-dabbrev-code-ignore-case t)
    (setq company-dabbrev-code-other-buffers 'all)
    (setq company-dabbrev-other-buffers 'all)
    
    (with-eval-after-load 'company
      (define-key company-active-map (kbd "M-n") nil)
      (define-key company-active-map (kbd "M-p") nil)
      (define-key company-active-map (kbd "C-n") #'company-select-next)
      (define-key company-active-map (kbd "C-p") #'company-select-previous))

    (require 'color)
    (let ((bg (face-attribute 'default :background)))
      (set-face-attribute 'company-tooltip nil
                          :inherit 'default
                          :background (color-lighten-name bg 2))
      (set-face-attribute 'company-scrollbar-bg nil
                          :background (color-lighten-name bg 10))
      (set-face-attribute 'company-scrollbar-fg nil
                          :background (color-lighten-name bg 5))
      (set-face-attribute 'company-tooltip-selection nil
                          :inherit font-lock-function-name-face)
      (set-face-attribute 'company-tooltip-common nil
                          :inherit font-lock-constant-face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Smartparens init
;;  Some of the configs taken from Sacha's config the rest is taken from
;;  mwfogleman.
;;  http://pages.sachachua.com/.emacs.d/Sacha.html
;;  https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :bind
  (("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-a" . sp-backward-down-sexp)
   ("C-S-a" . sp-beginning-of-sexp)
   ("C-S-d" . sp-end-of-sexp)
   ("C-M-e" . sp-up-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-t" . sp-transpose-sexp)
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-<delete>" . sp-unwrap-sexp)
   ("M-S-<backspace>" . sp-backward-unwrap-sexp)
   ("C-<right>" . sp-forward-slurp-sexp)
   ("C-<left>" . sp-forward-barf-sexp)
   ("C-M-<left>" . sp-backward-slurp-sexp)
   ("C-M-<right>" . sp-backward-barf-sexp)
   ("M-D" . sp-splice-sexp)
   ("C-M-<delete>" . sp-splice-sexp-killing-forward)
   ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
   ("C-M-S-<backspace>" . sp-splice-sexp-killing-around)
   ("C-]" . sp-select-next-thing-exchange)
   ("C-<left_bracket>" . sp-select-previous-thing)
   ("C-M-]" . sp-select-next-thing)
   ("M-F" . sp-forward-symbol)
   ("M-B" . sp-backward-symbol)
   ("H-t" . sp-prefix-tag-object)
   ("H-p" . sp-prefix-pair-object)
   ("H-s c" . sp-convolute-sexp)
   ("H-s a" . sp-absorb-sexp)
   ("H-s e" . sp-emit-sexp)
   ("H-s p" . sp-add-to-previous-sexp)
   ("H-s n" . sp-add-to-next-sexp)
   ("H-s j" . sp-join-sexp)
   ("H-s s" . sp-split-sexp)
   ("M-9" . sp-backward-sexp)
   ("M-0" . sp-forward-sexp))
  :init
  (progn
    (smartparens-global-mode t)
    (smartparens-strict-mode t)
    (show-smartparens-global-mode t)
    (use-package smartparens-config)

    (sp-local-pair 'web-mode "<" nil :when '(sacha/sp-web-mode-is-code-context))
  
    (sp-with-modes '(html-mode sgml-mode web-mode)
                   (sp-local-pair "<" ">"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Dired+ to enhance dired and commander.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dired+
  :ensure t
  :commands dired
  :config
  (progn
    (toggle-diredp-find-file-reuse-dir 1)
    (add-to-list 'load-path "~/.emacs.d/extra")
    (require 'dired-sort-menu)
    (setq dired-hide-details-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Setup Magit
;;  Taken from https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org
;;  but originally came from Magnars.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-status))
  :config
  (use-package git-timemachine
    :ensure t
    :bind (("C-x v t" . git-timemachine)))
  (use-package git-link
    :ensure t
    :bind (("C-x v L" . git-link))
    :init
    (setq git-link-open-in-browser t))
  (setq magit-use-overlays nil)
  ;; (diminish 'magit-auto-revert-mode)
  ;; (diminish 'magit-backup-mode)
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (bind-keys :map magit-status-mode-map
             ("TAB" . magit-section-toggle)
             ("<C-tab>" . magit-section-cycle)
             ("q" . magit-quit-session)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Realtive line numbers for easier jumping within a file and running macros.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package linum-relative
  :ensure t
  :commands linum-mode
  :config
  (progn
    (setq linum-format 'linum-relative)
    (setq linum-relative-current-symbol "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Nice commenting/uncommenting.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  For editing the MediaWiki at work.  An old version of Mediawiki is being
;;  used which is incompatible so this is only being used for the little code
;;  coloring that it provides.  As such the keybinding for C-x C-s that is
;;  provided by the mode is overwritten by the default action.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mediawiki
  :ensure t
  :commands mediawiki-mode
  :config
  (eval-after-load 'mediawiki
    (define-key mediawiki-mode-map (kbd "C-x C-s") 'save-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Stripe dired buffers and tables in ORG mode for easier reading.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package stripe-buffer
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'dired-mode-hook 'stripe-listify-buffer)
    (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
    (setq stripe-hl-line "#073642")
    (set-face-attribute stripe-highlight-face nil :background "#073642")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Ace jump and key chord look helpful/interesting.  The config for key chord
;;  was taken from Sacha Chua.
;;  http://pages.sachachua.com/.emacs.d/Sacha.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  JSON formatting.  Not used so much but I know it will be helpful.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package json-reformat
  :ensure t
  :commands (json-pretty-print json-pretty-print-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Add Git diff information to the gutter to easily see what has changed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package git-gutter+
  :ensure t
  :commands git-gutter+-mode
  :diminish git-gutter+-mode
  :config
  (progn
    (setq git-gutter+-modified-sign "  ") ;; two space
    (setq git-gutter+-added-sign "++")    ;; multiple character is OK
    (setq git-gutter+-deleted-sign "--")
    (set-face-background 'git-gutter+-modified "#073642")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Use hydra to define a toggle map and an application launch map.
;;  This is similar to the version done on the page below, which seems like a
;;  very good idea.
;;  https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package hydra
  :ensure t
  :config
  (progn
    ;;  Change the blue face color as it is hard to see in Solarized dark.
    (set-face-attribute 'hydra-face-blue nil
                        :foreground "#4169e1")
    
    (defhydra hydra-toggle (:color pink)
      "
_a_ abbrev-mode:         %`abbrev-mode
_b_ menu-bar-mode:       %`menu-bar-mode
_d_ debug-on-error:      %`debug-on-error
_f_ flyspell-mode:       %`flyspell-mode
_h_ global-hl-line-mode: %`global-hl-line-mode
_s_ smartparens-mode:    %`smartparens-mode

"
      ("a" abbrev-mode nil)
      ("b" menu-bar-mode)
      ("d" toggle-debug-on-error nil)
      ("f" flyspell-mode nil)
      ("h" global-hl-line-mode nil)
      ("s" smartparens-mode nil)
      ;; ("w" whitespace-mode nil)
      ("q" nil "cancel"))

    (defhydra hydra-launch (:color blue)
      "launch"
      ("i" ry/launch-internet-explorer "iexplore" :color blue)
      ("w" ry/launch-windows-explorer "wexplore" :color blue)
      ("q" nil "cancel" :color red))

    (defhydra hydra-xml (:color blue)
      "
_f_ Format
_l_ Linearlize
_w_ Where
_x_ Xquery buffer
_X_ Xquery region

"
      ("f" ry/xml-format nil)
      ("l" ry/xml-linearlize nil)
      ("w" ry/xml-where nil)
      ("x" ry/xquery nil)
      ("X" ry/xquery-with-region nil)
      ("q" nil "cancel" :color red))

    (global-set-key (kbd "C-c t") 'hydra-toggle/body)
    (global-set-key (kbd "C-c l") 'hydra-launch/body)
    (global-set-key (kbd "C-c x") 'hydra-xml/body)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Save recent file history.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package recentf
  :ensure t
  :init
  (progn
    (recentf-mode)
    (setq recentf-max-saved-items 25)
    (setq recentf-auto-cleanup 'never)
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Add numbers to EWW.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eww-lnum
  :ensure t
  :commands eww
  :init
  (eval-after-load "eww"
    '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
            (define-key eww-mode-map "F" 'eww-lnum-universal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Stack exchange package.  Nice to be able to read SX in emacs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sx
  :ensure t
  :commands (sx-tab-feature sx-tab-frontpage sx-tab-hot sx-tab-newest sx-tab))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Package for making commands work differently based on if a region is
;;  selected or not.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package whole-line-or-region
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Another folding package based on the active region.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fold-this
  :ensure t
  :bind (("C-c C-f" . fold-this-all)
         ("C-c C-F" . fold-this)
         ("C-c M-f" . fold-this-unfold-all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Nice visual regex replace package.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (progn
    (global-anzu-mode) 
    (global-set-key (kbd "M-%") 'anzu-query-replace) 
    (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Visual feedback while creating regular expressions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package re-builder
  :ensure t
  :config
  (setq reb-re-syntax 'string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Call the change--inner and then the starting character to modify the inside
;;  portion of the group.  Not sure if this will get use.  One of those things
;;  that could save a lot of time but there is a need to change a deep habit.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package change-inner
  :ensure t
  :config
  (progn
    (global-set-key (kbd "M-i") 'change-inner)
    (global-set-key (kbd "M-o") 'change-outer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Programmers like to know where the 80th column is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fill-column-indicator
  :ensure t
  :config
  (progn    
    (defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))
    
    (defvar sanityinc/fci-mode-suppressed nil)
    (make-variable-buffer-local 'sanityinc/fci-mode-suppressed)
    
    (defadvice popup-create (before suppress-fci-mode activate)
      "Suspend fci-mode while popups are visible"
      (let ((fci-enabled (sanityinc/fci-enabled-p)))
        (when fci-enabled
          (setq sanityinc/fci-mode-suppressed fci-enabled)
          (turn-off-fci-mode))))
    
    (defadvice popup-delete (after restore-fci-mode activate)
      "Restore fci-mode when all popups have closed"
      (when (and sanityinc/fci-mode-suppressed
                 (null popup-instances))
        (setq sanityinc/fci-mode-suppressed nil)
        (turn-on-fci-mode)))

    (setq fci-rule-column 80)
    (fci-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Searching package similar to helm swoop.  I liked helm swoop but it was slow
;;  for log files.  We will see if this is any faster.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package swiper
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Speedbar in the current frame.  Might be useful to list the current methods
;;  in a file when working on Java.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sr-speedbar
  :ensure t
  :config
  (progn
    (setq sr-speedbar-skip-other-window-p nil)
    (setq speedbar-use-images nil)
    (setq sr-speedbar-right-side t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fix annoying buffer popups.  
(use-package popwin
  :ensure t
  :config
  (popwin-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  On-the-fly syntax checking.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package lisp-mode
  :mode ("Cask\\'" . emacs-lisp-mode)
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Highlight the symbol at point.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package highlight-symbol
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'highlight-symbol-mode)
    (setf highlight-symbol-idle-delay 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Help to discover keybindings for the current mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package discover-my-major
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Text manipulation helpers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package move-text
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Perform setup for eclim.  The setup for Windows is problematic so there are
;;  a few documented steps below.
;;  It would be nice in the future to have this be more generic, but I'm simply
;;  happy that it is working.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs-eclim
  :ensure t
  :config
  (progn
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;  Use the company backend that comes with eclim instead.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (require 'company-emacs-eclim)
    (company-emacs-eclim-setup)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;  Windows setup so that eclim knows where the bat file is.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (custom-set-variables
     '(eclim-eclipse-dirs '("C:/IBM/SDP"))
     '(eclim-executable "C:/IBM/SDP/p2/cic.p2.cache.location/plugins/org.eclim_1.7.14/bin/eclim.bat")
     '(company-eclim-executable "C:/IBM/SDP/p2/cic.p2.cache.location/plugins/org.eclim_1.7.14/bin/eclim.bat"))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;  Toggle debugging.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (eclim-toggle-print-debug-messages)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;  Again, a windows modification so that the eclim bat can be found.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun eclim-executable-find ()
      (let (file)
        (dolist (eclipse-root eclim-eclipse-dirs)
          (and (file-exists-p
                (setq file (expand-file-name "plugins" eclipse-root)))
               (setq file (car (last (directory-files file t "^org.eclim_"))))
               (file-exists-p (setq file (expand-file-name "bin/eclim" file)))
               (return (expand-file-name "eclim.bat" eclipse-root))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;  Same as above.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun company-eclim-executable-find ()
      (let (file)
        (cl-dolist (eclipse-root '("c:/IBM/SDP"))
          (and (file-exists-p (setq file (expand-file-name "plugins" eclipse-root)))
               (setq file (car (last (directory-files file t "^org.eclim_"))))
               (file-exists-p (setq file (expand-file-name "bin/eclim" file)))
               (cl-return (expand-file-name "eclim.bat" eclipse-root))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;  This is not a great thing to do but was the only way that I could find
    ;;  to get the shell commands for eclim to work.  For some reason running
    ;;  the commands from a different drive didn't work eventhough the full
    ;;  path to the eclim.bat was specified.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun shell-command-to-string (command)
      "Execute shell command COMMAND and return its output as a string."
      (setq default-directory "C:/")
      (with-output-to-string
        (with-current-buffer
          standard-output
          (process-file shell-file-name nil t nil shell-command-switch command))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;  Again, I don't like to do this but there is an errant backslash (\) in
    ;;  the file path.  This will simply do a regex replace on the backslash.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun eclim--call-process (&rest args)
      "Calls eclim with the supplied arguments. Consider using
`eclim/execute-command' instead, as it has argument expansion,
error checking, and some other niceties.."
      (let ((cmd (eclim--make-command args)))
        (setq cmd2 (replace-regexp-in-string "\\\\" "" (format "%s" cmd)))
        (when eclim-print-debug-messages (message "Executing: %s" cmd2))
        (eclim--parse-result (shell-command-to-string cmd))))

    ;; Clobber this keybinding for eclim use.
    (define-key eclim-mode-map (kbd "M-/") 'company-emacs-eclim)
    (define-key eclim-mode-map (kbd "M-.") 'eclim-java-find-declaration)))

(use-package smartscan
  :ensure t)

(provide 'emacs-config)
;;; emacs-config.el ends here
