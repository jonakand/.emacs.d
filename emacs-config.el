;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Set the inactive modeline so it will match the flat look and size of the 
;;  modeline settings in the god mode config.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'mode-line-inactive nil
		    :foreground "grey80" :background "grey30"
		    :inverse-video nil
		    :box '(:line-width 1 :color "grey30" :style nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Enable some functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show the function the cursor is currently in in the status line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-function-mode 1)

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
;;  Turn off line wrapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-default 'truncate-lines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Increase the threshold size to 100MB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-default 'large-file-warning-threshold 1000000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save temp files in c:/temp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (eq system-type 'windows-nt)
    (progn
      (setq backup-directory-alist
	    `((".*" . "~/.emacs.d/BACKUP")))
      (setq auto-save-file-name-transforms
	    `((".*" , "~/.emacs.d/BACKUP" t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Auto revert the buffer if the underlying file has changed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-auto-revert-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ibuffer key.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Bindings for changing window sized.
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
(global-set-key (kbd "C-M-B") 'ry/xml-format)
(global-set-key (kbd "C-M-L") 'ry/xml-linearlize)
(global-set-key (kbd "<C-S-return>") 'xquery-with-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SQL related bindings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f5>") 'ry/sql-send-paragraph)
(global-set-key (kbd "<S-f5>") 'ry/sql-open-database)
(global-set-key (kbd "<C-f5>") 'ry/sql-send-export-paragraph)
(global-set-key (kbd "<M-f5>") 'ry/sql-connect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Spell checking keys.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f1] 'flyspell-check-previous-highlighted-word)
(global-set-key [f2] 'ispell-word)
(global-set-key [f3] 'flyspell-check-next-highlighted-word)
(global-set-key [f4] 'ispell-buffer)

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
;;  Define a map to use for toggling modes.  This seemed like a very nice idea.
;;  See if I can remember to use it.
;;  Taken from : https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-prefix-command 'toggle-map)
(bind-key "C-x t" 'toggle-map)
(bind-keys :map toggle-map
           ("l" . linum-mode)
           ("o" . org-mode)
           ("s" . smartparens-mode)
           ("t" . text-mode)
           ("w" . whitespace-mode)
	   ("n" . menu-bar-mode)
	   ("h" . global-hl-line-mode))

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
;;  Functions to open TEMP buffer and temp file on the desktop along with their
;;  global key bindings.
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
(defun ry/sql-open-database ()
  "Open a SQLI process and name the SQL statement window with the name provided."
  (interactive)
  (setq sql-set-product "db2")
  (sql-db2)
  (other-window 1)
  (switch-to-buffer "*DATABASE*")
  (sql-mode)
  (sql-set-product "db2")
  (setq sql-buffer "*SQL*")
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
;; Customized functions                
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
;; Functions for formatting XML documents.
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
;;  Perform an XQUERY on the selected region.
;;  This function was taken from http://donnieknows.com/blog/hacking-xquery-emacs-berkeley-db-xml
;;  and modified to work with Saxon instead of Berkley DB.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xquery-with-region (beg end)
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
         (insert (perform-xquery xquery t)))
       (nxml-mode)
       (goto-char (point-min)))
     (switch-to-buffer-other-window xquery-result)
     (other-window -1)))

(defun perform-xquery (xquery &optional timed)
  "Perform the selected Xquery using Saxon."
  (setq file (make-temp-file "elisp-dbxml-"))
  (write-region xquery nil file)
  (setq result (shell-command-to-string
		(concat "java -cp H:/emacs/Java/saxon9he.jar net.sf.saxon.Query -q:\"" file "\" !indent=yes\n")))
  (delete-file file)
  (concat "" result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
(defun ry/switch-buffer ()
  "Function to switch the current buffer to God-mode
 prior to switching buffers."
  (interactive)
  (god-local-mode 1)
  (helm-mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Simple function to byte compile configs.
;;  Taken from http://pages.sachachua.com/.emacs.d/Sacha.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sacha/byte-recompile ()
  "Byte recompile EL files in the specified directories."
  (interactive)
  (byte-recompile-directory "~/emacs/Config" 0))

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
;;  Decode a HEX string/region to ASCII characters.
;;  http://stackoverflow.com/questions/12003231/how-do-i-convert-a-string-of-hex-into-ascii-using-elisp
;;
;;  TODO : This function is questionable.  Need to double check.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun decode-hex-string (hex-string)
  (apply #'concat 
         (loop for i from 0 to (- (/ (length hex-string) 2) 1) 
               for hex-byte = (substring hex-string (* 2 i) (* 2 (+ i 1)))
               collect (format "%c" (string-to-number hex-byte 16)))))

(defun hex-decode-region (start end) 
  "Decode a hex string in the selected region."
  (interactive "r")
  (save-excursion
    (let* ((decoded-text 
            (decode-hex-string 
             (buffer-substring start end))))
      (delete-region start end)
      (insert decoded-text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Function for checking the next problem word.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Hide/Show modeline.  This looked interesting but I'm not sure if it will get
;;  used.
;;  Taken from : https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-local hidden-mode-line-mode nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(bind-key "m" 'hidden-mode-line-mode toggle-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme the window early so that things don't flash from light to dark when
;; the theme is loaded.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Smart mode line config.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smart-mode-line
  :ensure t 
  :config
  (progn
    (sml/setup)
    (sml/apply-theme 'dark)))

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
  :config
  (add-hook 'prog-mode-hook 'global-smartscan-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config hideshowvis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package hideshowvis
  :ensure t
  :config
  (progn
    (add-to-list 'hs-special-modes-alist
		 '(nxml-mode
		   "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
		   ""
		   "<!--" ;; won't work on its own; uses syntax table
		   (lambda (arg) (my-nxml-forward-element))
		   nil))

    (defun my-nxml-forward-element ()
      (let ((nxml-sexp-element-flag))
	(setq nxml-sexp-element-flag (not (looking-at "<!--")))
	(unless (looking-at outline-regexp)
	  (condition-case nil
	      (nxml-forward-balanced-item 1)
	    (error nil)))))
    
    (add-hook 'hs-nxml-hook
	      (lambda ()
		(save-excursion
		  (when (search-forward-regexp "^<\\?xml" 6 0)
		    (nxml-mode)))))

    (autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")

    (autoload 'hideshowvis-minor-mode
      "hideshowvis"
      "Will indicate regions foldable with hideshow in the fringe."
      'interactive)
    (dolist (hook (list 'emacs-lisp-mode-hook
			'java-mode-hook
			'nxml-mode-hook))
      (add-hook hook 'hideshowvis-enable))
    
    (global-set-key (kbd "M--") 'hs-hide-block)
    (global-set-key (kbd "M-=") 'hs-show-block)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Add hook to NXML to load custom functions when NXML loads.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package nxml-mode
  :commands 
  :init
  (progn 
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
   ("\\.djhtml\\'" . web-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm
  :ensure t
  :bind (("C-M-s" . helm-occur)
	 ("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)
	 ("C-x r l" . helm-bookmarks)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x C-b" . helm-mini)
	 ("<f7>" . helm-bookmarks)
	 ("<f8>" . bookmark-set))
  :init
  (progn
    (require 'helm-config)
    (helm-mode 1)
    (setq htlm-ff-auto-update-initial-value nil)

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
	  helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
	  helm-quick-update t
	  helm-M-x-requires-pattern nil
	  helm-ff-skip-boring-files t)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;  Make helm always create a new window and always split the current window
    ;;  vertially.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq helm-display-function
	  (lambda (buf)
	    (split-window-vertically)
	    (other-window 1)
	    (switch-to-buffer buf)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Hunspell
;; rw-hunspell is no longer used as Emacs 24.4 made Hunspell integration very
;; easy.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ispell-personal-dictionary "~/emacs/Config/en_US_personal")
(setq ispell-silently-savep t)
(setq ispell-quietly t)

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
  (progn
    (guru-mode)))

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
  :commands turn-on-eldoc-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Load sunrise commander and set F11 to open it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sunrise-commander
  :ensure t 
  :commands sunrise
  :bind ("<f11>" . sunrise)
  :config
  (progn
    (sr-set-windows-default-ratio 'sr-windows-default-ratio 80)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add org mode stuff to the load path.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure t 
  :commands org-mode
  :idle (load "~/.emacs.d/custom-org-mode.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Yasnippet setup and prompt to use popup menu mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :ensure t 
  :commands yas-global-mode
  :idle (yas-global-mode)
  :config
  (progn
    (use-package popup))

    (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
    (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
    (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
    (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
   
    (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
      (when (featurep 'popup)
	(popup-menu*
	 (mapcar
	  (lambda (choice)
	    (popup-make-item
	     (or (and display-fn (funcall display-fn choice))
		 choice)
	     :value choice))
	  choices)
	 :prompt prompt
	 ;; start isearch mode immediately
	 :isearch t)))

    (setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  undo-tree config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :ensure t 
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  GOD mode config with some keymaps to make it easier to use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package god-mode
  :ensure t 
  :bind (("<escape>" . god-local-mode)
	 ("C-x C-1" . delete-other-windows)
	 ("C-x C-2" . sacha/vsplit-last-buffer) ;; split-window-below)
	 ("C-x C-3" . sacha/hsplit-last-buffer) ;;'split-window-right)
	 ("C-x C-0" . delete-window)
	 ("C-x C-k" . kill-this-buffer)
	 ("C-x C-S-k" . kill-other-buffers)
	 ("C-x C-o" . other-window)
	 ("C-c C-r" . org-capture)
	 ("C-x C-h" . mark-whole-buffer)
	 ("C-x C-d" . dired))
  :init
  (progn
    (god-mode-all)
    
    (define-key god-local-mode-map (kbd "z") 'repeat)
    (define-key god-local-mode-map (kbd "i") 'god-local-mode)
    (define-key god-local-mode-map (kbd "v") 'scroll-up-command)
    
    ;;  Change the color of the modeline based on god-local-mode being enabled 
    ;;  or not.
    (add-hook 'post-command-hook
	      (lambda ()
		(if god-local-mode
		    (progn
		      (set-face-attribute 'mode-line nil
					  :foreground "#ffffff" :background "#b22222" ;;"#ff1493"
					  :inverse-video nil
					  :box '(:line-width 1 :color "#b22222" :style nil))
		      (setq cursor-type 'box))
		  (progn
		    (set-face-attribute 'mode-line nil
					:foreground "#ffffff" :background "#006400"
					:inverse-video nil
					:box '(:line-width 1 :color "#006400" :style nil))
		    (setq cursor-type 'bar)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Init golden ratio mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package golden-ratio
  :ensure t 
  :config
  (progn 
    (golden-ratio-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t 
  :config
  (progn
    (global-company-mode)
    (setq company-idle-delay 0)
    (setq company-show-numbers t)
        
    (with-eval-after-load 'company
      (define-key company-active-map (kbd "M-n") nil)
      (define-key company-active-map (kbd "M-p") nil)
      (define-key company-active-map (kbd "C-n") #'company-select-next)
      (define-key company-active-map (kbd "C-p") #'company-select-previous))

    (require 'color)
    (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Smartparens init.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartparens
  :ensure t 
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Dired+ to enhance dired and commander.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dired+
  :ensure t 
  :config
  (progn
    (require 'dired-sort-menu)
    (setq dired-hide-details-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Clojure development.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cider
  :ensure t 
  :config
  (setq cider-lein-command "~/emacs/Leiningen/lein.bat"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Setup Magit
;;  Taken from https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org
;;  but originally came from Magnars.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-status))
  :init
  (use-package git-timemachine
    :ensure t
    :bind (("C-x v t" . git-timemachine)))
  (use-package git-link
    :ensure t
    :bind (("C-x v L" . git-link))
    :init
    (setq git-link-open-in-browser t))
  :config
  (setq magit-use-overlays nil)
  (diminish 'magit-auto-revert-mode)
  (diminish 'magit-backup-mode)
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

(use-package linum-relative
  :ensure t
  :init
  (setq linum-format 'linum-relative)
  :config
  (setq linum-relative-current-symbol ""))

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))	
