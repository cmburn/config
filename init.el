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
;; -----------------------------------------------------------------------------
;; Package management
;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
;; Code completion
;; -----------------------------------------------------------------------------
;; YCMD is the completion framework I prefer- make sure you have ycmd installed!
(use-package ycmd
  :config
  (progn
    (set-variable 'ycmd-server-command
		  `("python" "-u" ,(file-truename "~/.emacs.d/ycmd/ycmd/")))
    (setq ycmd-startup-timeout 15000)
    (add-hook 'after-init-hook #'global-ycmd-mode)))

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


;; Emacs specific completion

;; Helm helps to perform autocomplete within Emacs itself, i.e. calling M-x
;; functions. It makes life easier if you don't have absolutely everything
;; committed to muscle memory (like me!)
(use-package helm
  :config
  (progn
    (use-package helm-cscope)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-h a") 'helm-apropos)
    (helm-mode t)))

;; -----------------------------------------------------------------------------
;; Error checking
;; -----------------------------------------------------------------------------
;; Flycheck automatically checks as you code and shows you errors
(use-package flycheck
  :ensure t
  :config
  (use-package flycheck-ycmd :ensure t)
  (use-package flycheck-clang-analyzer :ensure t)
  (use-package flycheck-projectile :ensure t))


;; -----------------------------------------------------------------------------
;; UI
;; -----------------------------------------------------------------------------
;; Navigation tabs
(use-package doom-modeline
  :config
  (progn
    (setq doom-modeline-icon t)
    (use-package font-utils)
    (use-package all-the-icons)
    (doom-modeline-mode)))

(use-package centaur-tabs
  :demand
  :config
  :bind
  (("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward)))
(centaur-tabs-mode t)
(centaur-tabs-headline-match)

;; Leuven is nice and readable.
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-leuven-theme/lisp/")
;; (use-package leuven-theme)
(load-theme 'leuven t)
(set-face-background 'mode-line-inactive "#5D6B99")
(set-face-background 'mode-line "#5D6B99")

;; Nice, readable font
;; Sadly I use windows from time to time, and the fonts are different there so
;; let's work around that here.
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil
			:family "Consolas"
			:weight 'normal
			:height 110
			:width 'normal)
  (set-face-attribute 'default nil
		      :family "Inconsolata"
		      :weight 'normal
		      :height 120
		      :width 'normal))

;; Use easy to use dashboard by default
(use-package dashboard
  :ensure t
  :config
  (progn
    (dashboard-setup-startup-hook)
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))))


;; Search highlighting
(use-package anzu
  :config
  (global-set-key (kbd "M-%") 'anzu-query-replace-regexp))


;; -----------------------------------------------------------------------------
;; Code Navigation
;; -----------------------------------------------------------------------------
;; CScope is probably the most useful single piece of software to navigate your
;; code, I've heard a lot of recommendations for other similar programs but none
;; ever quite measured up in my eyes.
(use-package xcscope
  :config
  ;; I didn't want to add the whole of mingw to my windows' system PATH, so just
  ;; copy cscope.exe to your ~/.emacs.d/ directory.
  (if (eq system-type 'windows-nt)
      (setq cscope-program "~/.emacs.d/cscope.exe")))

;; Treemacs allows for a sidebar to easier navigate your project- dired is great
;; and all, but sometimes it's easier to see how your directories are layed out
;; visually
(use-package treemacs)
(use-package treemacs-projectile)

;; Folding for readability
(use-package vimish-fold)
(use-package yafolding)


;; -----------------------------------------------------------------------------
;; Language-Specific Bindings
;; -----------------------------------------------------------------------------
;; MATLAB
(use-package matlab
  :ensure matlab-mode
  :config
  (add-to-list
   'auto-mode-alist
   '("\\.m\\'" . matlab-mode)))

;; LaTeX
(use-package tex-mode
  :ensure auctex
  :config
  ;; Allows you to view your latex report side by side without having to open up
  ;; evince or something
  (use-package latex-preview-pane))

;; Bison
(use-package bison-mode)


;; CMake
(use-package cmake-mode
  :ensure t
  :config
  (progn
    (use-package cmake-ide
      :config
      (cmake-ide-setup))
    (use-package cmake-project)))

;; Check if we ought to treat this as a cmake project or not, to be called in
;; our C/C++ hook
(defun check-cmake-project ()
  (if (or (file-exists-p "CMakeLists.txt")
          (file-exists-p
	   (expand-file-name "CMakeLists.txt"
			     (car (project-roots (project-current))))))
      (cmake-project-mode)))

;; C/C++
;; Set C code to automatically be formatted to openbsd style(9).
;; Fetch the file if Emacs can't find it

(defun fetch-openbsd-style ()
  "Grab the openbsd style elisp file"
  (progn
    (require 'url)
    ;; Check if the directory we want is at least there, otherwise make one
    (when (not (file-directory-p "~/.emacs.d/elisp"))
      (make-directory "~/.emacs.d/elisp"))
    ;; And just fetch the file!
    (when (not (file-exists-p "~/.emacs.d/elisp/openbsd-knf-style.el"))
      (url-copy-file (concat "https://raw.githubusercontent.com/"
			     "hogand/openbsd-knf-emacs/master/"
			     "openbsd-knf-style.el")
		     "~/.emacs.d/elisp/openbsd-knf-style.el"))))

;; Ensure our directory is loaded by emacs when it's looking for stuff
(add-to-list 'load-path "~/.emacs.d/elisp/")
(unless (file-exists-p "~/.emacs.d/elisp/openbsd-knf-style.el")
  (fetch-openbsd-style))

;; Lastly, enable the package
(require 'openbsd-knf-style)
(c-add-style "OpenBSD" openbsd-knf-style)
(setq c-default-style '((c-mode . "OpenBSD")))

;; Finally, set everything we want to run when we open a C or C++ file
(add-hook 'c-mode-common-hook
	  (lambda()
	    ;; Emacsese (Emacsish?) for "If you're editing C or C++"
	    (when (derived-mode-p 'c-mode 'c++-mode)
	      (company-ycmd t)
	      (cscope-minor-mode t)
	      (flycheck-mode t)
	      (helm-cscope-mode t)
	      (local-set-key (kbd "M-.") 'helm-cscope-find-global-definition)
	      (openbsd-set-knf-style)
	      (check-cmake-project)
	      (vimish-fold-mode t)
	      (which-func-mode t)
	      ;; Cscope is super useful, albeit hard to use sometimes.
	      ;; Helm makes that a little easier :)
              (local-set-key (kbd "M-,") 'helm-cscope-pop-mark)
              (local-set-key (kbd "M-@")
			     'helm-cscope-find-calling-this-function)
              (local-set-key (kbd "M-s") 'helm-cscope-find-this-symbol))))

(add-hook 'c++-mode-hook 'c-mode-common-hook)

;; -----------------------------------------------------------------------------
;; General Emacs Config
;; -----------------------------------------------------------------------------

;; Less copy and pasting
(use-package yasnippet)
(use-package helm-c-yasnippet)

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

;; Security stuff
(setq tls-checktrust t)
(setq gnutls-verify-error t)
(let ((trustfile "/etc/ssl/cert.pem"))
  (setq tls-program
        `(,(format  "gnutls-cli --x509cafile %s -p %%p %%h" trustfile)
          ,(format
	    "openssl s_client -connect %%h:%%p -CAfile %s -no_ssl2 -ign_eof"
	    trustfile)))
  (setq gnutls-trustfiles (list trustfile)))

;; Windows likes to be a special child and use different file endings
(set-buffer-file-coding-system 'unix)

;; Highlight searching, replacing, and parenthesis junk so I know what I'm doing
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

;; -----------------------------------------------------------------------------
;; General styling aids
;; -----------------------------------------------------------------------------

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
