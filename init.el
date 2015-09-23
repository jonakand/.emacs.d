;(let ((file-name-handler-alist nil))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Bootstrap use-package.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (require 'package)
  (setq package-enable-at-startup nil)

  (setq package-archives '(("org" . "http://orgmode.org/elpa/") 
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")))

  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile
    (require 'use-package))

  (require 'diminish)
  (require 'bind-key)

  (setq use-package-verbose t)            

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Tangle and byte compile the source ORG document if it is newer than the
  ;;  previously tangled and compiled file.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let ((src "~/.emacs.d/emacs-config.org")
        (dst "~/.emacs.d/emacs-config.elc"))
    (if (file-newer-than-file-p src dst)
        (progn
          (require 'ob-tangle)
          (add-hook 'after-init-hook (lambda () (org-babel-load-file "~/.emacs.d/emacs-config.org" t))))
      (add-hook 'after-init-hook (lambda () (load-file "~/.emacs.d/emacs-config.elc")))))

  ;; (add-hook 'after-init-hook (lambda () (org-babel-tangle-file "~/.emacs.d/emacs-config.org")))
;)
