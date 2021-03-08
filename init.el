;; Copyright (c) 2019 Charlie Burnett <burne251@umn.edu>

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;; My personal Emacs config!
;; ------------------------------------------------------------------------------
;; Package management
;; ------------------------------------------------------------------------------
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
(setq use-package-verbose t)

;; ------------------------------------------------------------------------------
;; Code completion
;; ------------------------------------------------------------------------------
;; Company is a big completion framework for emacs
;; A few use-package calls to tell Emacs to set it up for us
(use-package company
  :ensure t
  :config
  (progn
    (use-package company-c-headers :ensure t)
    (use-package company-ctags :ensure t)
    (use-package company-ycmd :ensure t)))
(company-ycmd-setup)
(add-hook 'after-init-hook 'global-company-mode)

;; YCMD is the completion framework I prefer- make sure you have ycmd installed!
(use-package ycmd)
(setq ycmd-server-command '("python3" "/usr/local/ycmd/ycmd/__main__.py"))
(setq ycmd-startup-timeout 600)
(add-hook 'after-init-hook #'global-ycmd-mode)

;; Automagically makes things work for CMake projects, which my C/C++ projects
;; usually are
(use-package cmake-ide)

;; Emacs specific completion

;; Helm helps to perform autocomplete within Emacs itself, i.e. calling M-x
;; functions. It makes life easier if you don't have absolutely everything
;; committed to muscle memory (like me!)
(use-package helm)
(use-package helm-cscope)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h a") 'helm-apropos)
(helm-mode 1)

;; ------------------------------------------------------------------------------
;; Error checking
;; ------------------------------------------------------------------------------
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

;; ------------------------------------------------------------------------------
;; UI
;; ------------------------------------------------------------------------------
;; Easy and visually appealing modeline from DOOM Emacs
(use-package doom-modeline)
(use-package all-the-icons
  :config
  (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts t)))
(doom-modeline-mode)

;; Navigation tabs
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

;; Doom-themes are one of the few with support for centaur-tabs
(use-package doom-themes)
(load-theme 'doom-dark+ t)

;; Nice, readable font
(set-face-attribute 'default nil
		    :family "Inconsolata"
		    :weight 'normal
		    :height 120
		    :width 'normal)

;; Use easy to use dashboard by default
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Search highlighting
(use-package anzu)
(global-set-key (kbd "M-%") 'anzu-query-replace-regexp)


;; ------------------------------------------------------------------------------
;; Code Navigation
;; ------------------------------------------------------------------------------
;; CScope is probably the most useful single piece of software to navigate your
;; code, I've heard a lot of recommendations for other similar programs but none
;; ever quite measured up in my eyes.
(use-package xcscope)

;; Treemacs allows for a sidebar to easier navigate your project- dired is great
;; and all, but sometimes it's easier to see how your directories are layed out
;; visually
(use-package treemacs)
(use-package treemacs-projectile)

;; Folding for readability
(use-package vimish-fold)


;; ------------------------------------------------------------------------------
;; Language-Specific Bindings
;; ------------------------------------------------------------------------------
;; MATLAB
(use-package matlab
  :ensure matlab-mode
  :config
  (add-to-list
    'auto-mode-alist
    '("\\.m\\'" . matlab-mode)))

;; LaTeX
(use-package tex-mode
  :ensure auctex)
;; Allows you to view your latex report side by side without having to open up
;; evince or something
(use-package latex-preview-pane)


;; C
;; Set C code to automatically be formatted to openbsd style(9).
;; Fetch the file if Emacs can't find it
(require 'url)
;; Check if the directory we want is at least there, otherwise make one
(when (not (file-directory-p "~/.emacs.d/elisp"))
  (make-directory "~/.emacs.d/elisp"))
;; And just fetch the file!
(when (not (file-exists-p "~/.emacs.d/elisp/openbsd-knf-style.el"))
  (url-copy-file (concat "https://raw.githubusercontent.com/"
			 "hogand/openbsd-knf-emacs/master/"
			 "openbsd-knf-style.el")
		 "~/.emacs.d/elisp/openbsd-knf-style.el"))
;; Ensure our directory is loaded by emacs when it's looking for stuff
(add-to-list 'load-path "~/.emacs.d/elisp/")
;; Lastly, enable the package
(require 'openbsd-knf-style)
(c-add-style "OpenBSD" openbsd-knf-style)
(setq c-default-style '((c-mode . "OpenBSD")))
(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Emacsese (Emacsish?) for "If you're editing C or C++"
	    (when (derived-mode-p 'c-mode 'c++-mode)
	      (which-func-mode 1)
	      (flycheck-mode 1)
	      ;; Cscope is super useful, albeit hard to use sometimes.
	      ;; Helm makes that a little easier :)
	      (helm-cscope-mode 1)
	      (cscope-minor-mode 1)
	      (company-ycmd 1)
	      (vimish-fold-mode 1)
	      (openbsd-set-knf-style)
	      (local-set-key (kbd "M-.") 'helm-cscope-find-global-definition)
              (local-set-key (kbd "M-@") 'helm-cscope-find-calling-this-function)
              (local-set-key (kbd "M-s") 'helm-cscope-find-this-symbol)
              (local-set-key (kbd "M-,") 'helm-cscope-pop-mark)
	      ))
	  )


;; ------------------------------------------------------------------------------
;; General Emacs Config
;; ------------------------------------------------------------------------------

;; Littering is bad, make sure Emacs knows better
(setq backup-directory-alist '(("." . "~/.emacs-backups.d/")))
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

;; ------------------------------------------------------------------------------
;; General styling aids
;; ------------------------------------------------------------------------------

;; General coding style guides (show max recommended column, extra whitespace,
;; etc)
(setq-default show-trailing-whitespace 1)
(setq whitespace-style '(trailing lines space-before-tab)
      whitespace-line-column 80)
;; Highlight trailing whitespace
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


;; Make sure I don't leave extra space on the end of a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)
