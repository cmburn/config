;; My personal Emacs config!

;; Package management junk

;; Melpa is an extended package archive for Emacs, allowing use of some extra
;; packages, add it here
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; If Emacs doesn't see any package archives, pull them in
(when (not package-archive-contents)
    (package-refresh-contents))

;; use-package is a super useful tool for managing packages (though not actually
;; a package manager), so if Emacs can't find it, fix that
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;; Fancy highlight stuff
(use-package anzu)
(global-set-key (kbd "M-%") 'anzu-query-replace-regexp)

;; Auctex is a LaTex editing suite for Emacs, *extremely* useful
(use-package tex-mode
  :ensure auctex)
;; Allows you to view your latex report side by side without having to open up
;; evince or something
(use-package latex-preview-pane)


;; Nicer tabs
(use-package centaur-tabs
  :demand
  :config
  :bind
  (("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)))
(centaur-tabs-mode t)
(centaur-tabs-headline-match)
(setq centaur-tabs-style "chamfer")
(setq centaur-tabs-set-bar 'over)
(setq centaur-tabs-set-modified-marker t)

;; Automagically makes things work for CMake projects, which my C/C++ projects
;; usually are
(use-package cmake-ide)

;; Company is a big completion headwork for emacs, next few use-package calls
;; tell emacs to set it up for us
(use-package company
  :ensure t
  :config
  (progn
    (use-package company-c-headers :ensure t)
    (use-package company-ctags :ensure t)
    (use-package company-ycmd :ensure t)))
(company-ycmd-setup)
(add-hook 'after-init-hook 'global-company-mode)

;; Don't open up annoying scratch buffer by default
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Easy and visually appealing modeline from DOOM Emacs
(use-package doom-modeline)
(use-package all-the-icons)
(doom-modeline-mode)

;; Flycheck automatically checks as you code and shows you errors
(use-package flycheck
  :ensure t
  :config
  (progn
    (use-package flycheck-ycmd :ensure t
      :init (lambda()
	      flycheck-ycmd-setup
	      (add-hook 'ycmd-file-parse-result-hook
			'flycheck-ycmd--cache-parse-results)
	      ))
    (use-package flycheck-clang-analyzer :ensure t)
    (use-package flycheck-projectile :ensure t)))
(add-to-list 'flycheck-checkers 'ycmd 'clang-analyzer)


;; Helm helps to perform autocomplete within Emacs itself, i.e. calling M-x
;; functions. It makes life easier if you don't have absolutely everything
;; committed to muscle memory (like me!)
(use-package helm)
(use-package helm-cscope)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h a") 'helm-apropos)
(helm-mode 1)


;; Fun with MATLAB
(use-package matlab
  :ensure matlab-mode
  :config
  (add-to-list
    'auto-mode-alist
    '("\\.m\\'" . matlab-mode)))


;; Treemacs allows for a sidebar to easier navigate your project- dired is great
;; and all, but sometimes it's easier to see how your directories are layed out
;; visually
(use-package treemacs)
(use-package treemacs-projectile)

;; Package for folding for readability
(use-package vimish-fold)

;; Doom-themes are one of the few with support for centaur-tabs
(use-package doom-themes)
(load-theme 'doom-old-hope t)

;; Cscope is probably the most useful single piece of software to navigate your
;; code, I've heard a lot of recommendations for other similar programs but none
;; ever quite measured up in my eyes.
(use-package xcscope)

;; YCMD is the completion framework I prefer- make sure you have ycmd installed!
(use-package ycmd)
(setq ycmd-server-command '("python3" "/usr/local/bin/ycmd"))
(setq ycmd-startup-timeout 600)
(add-hook 'after-init-hook #'global-ycmd-mode)


;; Set C code to automatically be formatted to openbsd style(9).
;; I keep the necessary file in my extras.tgz, just tar xzf in your .emacs.d
(add-to-list 'load-path "~/.emacs.d/extern/openbsd-knf-style")
(require 'openbsd-knf-style)
(c-add-style "OpenBSD" openbsd-knf-style)
(setq c-default-style '((c-mode . "OpenBSD")))

;; General Emacs goodies :)

;; Disable the big buttons on top to free up some space
(tool-bar-mode -1)
;; Show line numbers in the status bar
(line-number-mode 1)
;; as well as the column numbers
(column-number-mode 1)
;; If you throw a compressed file (like a .tgz or .zip file) at Emacs, show the
;; contents in human-readable format
(auto-compression-mode 1)

;; Highlight searching, replacing, and parenthesis junk so I know what I'm doing!
;; Well, sorta at least...
(setq search-highlight 1)
(setq query-replace-highlight 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(setq blink-matching-paren 1)
(transient-mark-mode 1)
(show-paren-mode 1)
;; Adds a matching parenthesis so stuff doesn't blow up in my face, as it tends
;; to do
(electric-pair-mode 1)

;; General coding style guides (show max recommended column, extra whitespace,
;; etc)
(setq-default show-trailing-whitespace 1)
(setq whitespace-style '(trailing lines space-before-tab)
      whitespace-line-column 80)
;; Highlight trailing wh
(global-whitespace-mode 1)
(global-font-lock-mode 1)
(global-display-line-numbers-mode 1)
(global-display-fill-column-indicator-mode 1)
(setq require-final-newline 1)
(setq-default fill-column 80)

;; I can't stand it when Emacs wraps lines
(setq-default truncate-lines 1)

;; Anything here has the fill column marker disabled
(add-hook 'after-change-major-mode-hook
	  (lambda()
	    (when (derived-mode-p 'dashboard-mode 'matlab-shell-mode)
	    (display-fill-column-indicator-mode 0))))

;; Nice, readable font
(set-face-attribute 'default nil
		    :family "Iosevka Fixed Extended"
		    :weight 'normal
		    :height 110
		    :width 'normal)

;; Make sure I don't leave extra space on the end of a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; C stuff
(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Emacsese (Emacsish?) for "If you're editing C or C++"
	    (when (derived-mode-p 'c-mode 'c++-mode)
	      (which-func-mode 1)
	      (flycheck-mode 1)
	      ;; Cscope is super useful, albeit a hard to use sometimes.
	      ;; Helm makes that a little easier :)
	      (helm-cscope-mode 1)
	      (cscope-minor-mode 1)
	      (company-ycmd 1)
	      (openbsd-set-knf-style 1)
	      (local-set-key (kbd "M-.") 'helm-cscope-find-global-definition)
              (local-set-key (kbd "M-@") 'helm-cscope-find-calling-this-function)
              (local-set-key (kbd "M-s") 'helm-cscope-find-this-symbol)
              (local-set-key (kbd "M-,") 'helm-cscope-pop-mark)
	      ))
	  )
