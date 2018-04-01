(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))


(setq package-enable-at-startup nil)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-gcal ein ## auctex elpy jedi magit dracula-theme evil-mode markdown-mode use-package helm evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'evil)
(evil-mode t)

(use-package org :ensure t)

(use-package helm
  :ensure t)

(use-package markdown-mode
  :ensure t)

(load-theme 'dracula t) 

(tool-bar-mode 0)
(menu-bar-mode 0)

;;org mode shits

;;(setq org-default-notes-file "~/notes/notes.org")

(setq org-agenda-files '("~/gtd/gtd.org"
			 "~/gtd/inbox.org"
			 "~/gtd/tickler.org"))

(setq org-capture-templates '(("t" "todo [inbox]" entry
			       (file+headline "~/gtd/inbox.org" "Tasks")
			       "* TODO %i%?")
			      ("T" "Tickler" entry
			       (file+headline "~/gtd/tickler.org" "Tickler")
			       "* %i%? \n %U")))

(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
			   ("~/gtd/someday.org" :level . 1)
			   ("~/gtd/tickler.org" :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))			  

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;;

(setq jedi:complete-on-dot t)
(setq jedi:server-args
      '("--virtual-env" "~/datascience3"))
(add-hook 'python-mode-hook 'jedi:setup)


(use-package elpy
  :ensure t)
(elpy-enable)
(elpy-use-ipython)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
