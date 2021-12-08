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
;; LSP is a standard protocol for code completion, and is used by many, if not
;; most, modern IDEs.

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp
  :custom
  (lsp-clangd-binary-path (shell-command-to-string "which clangd"))
  :config
  (progn
    (use-package lsp-ui :commands lsp-ui-mode)
    (use-package helm-lsp :commands helm-lsp-workspace-symbol)
    (use-package lsp-treemacs
      :commands lsp-treemacs-errors-list)))

(defun lsp-treemacs-error-list-toggle ()
  "Toggles an open window of LSP errors in lsp-treemacs"
  (interactive)
  (let ((errwin (get-buffer-window "*LSP Error List*" 'visible)))
    (if errwin
	(delete-window errwin)
      (lsp-treemacs-errors-list))))

(global-set-key (kbd "M-e") 'lsp-treemacs-error-list-toggle)

;; optionally if you want to use debugger

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-enabled-clients 'clangd)
  (add-to-list 'lsp-enabled-clients 'perl-language-server)
  (require 'dap-cpptools))


;; Company is a big completion framework for emacs
;; A few use-package calls to tell Emacs to set it up for us
(use-package company
  :ensure t
  :config
  (progn
    (use-package company-c-headers :ensure t)
    (use-package company-ctags :ensure t)))
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
;; Error checking/Debugging
;; -----------------------------------------------------------------------------
;; Flycheck automatically checks as you code and shows you errors
(use-package flycheck
  :ensure t
  :config
  (use-package flycheck-clang-analyzer :ensure t)
  (use-package flycheck-projectile :ensure t))

;; Debugger integration
(use-package dap-mode)

;; -----------------------------------------------------------------------------
;; UI
;; -----------------------------------------------------------------------------


(use-package doom-themes)
(load-theme 'doom-dark+ t)


;; Easier navigation than the default
(use-package doom-modeline
  :config
  (progn
    (setq doom-modeline-icon t)
    (use-package font-utils)
    (use-package all-the-icons)
    (doom-modeline-mode)))

;; Navigation tabs
(use-package centaur-tabs
  :demand
  :config
  :bind
  (("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward)))
(centaur-tabs-mode t)

(set-face-attribute 'default nil
		    :family "Fira Code"
		    :foundry "CTDB"
		    :height 113)

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
(use-package treemacs
  :config
  (progn
    (setq treemacs-width 28)
    (global-set-key (kbd "M-a") 'treemacs))

  )
(use-package treemacs-projectile)



;; Folding for readability
(use-package vimish-fold)
(use-package yafolding)


;; -----------------------------------------------------------------------------
;; Language-Specific Bindings
;; -----------------------------------------------------------------------------
;; *Bison*
(use-package bison-mode)

;; *C/C++*
;; Set C code to automatically be formatted to openbsd style(9).
;; Fetch the file if Emacs can't find it

;; CMake (needs to be loaded before our c hook is defined
(use-package cmake-mode
  :ensure t
  :config
  (progn
    (use-package cmake-ide
      :config
      (cmake-ide-setup))
    (use-package cmake-project)))
;; Requires cmake-language-server from pip
(add-hook 'cmake-mode-hook
	  (lambda ()))

;; Check if we ought to treat this as a cmake project or not, to be called in
;; our C/C++ hook
(defun check-cmake-project ()
  (if (or (file-exists-p "CMakeLists.txt")
          (file-exists-p
	   (expand-file-name "CMakeLists.txt"
			     (car (project-root (project-current))))))
      (cmake-project-mode)))


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
(setq c-default-style '((c-mode . "OpenBSD")
			(c++-mode . "OpenBSD")
			(java-mode . "OpenBSD")))


;; (require 'ope-mode)

;; Finally, set everything we want to run when we open a C-like file
(add-hook 'c-mode-common-hook
	  (lambda()
	    ;; Emacsese (Emacsish?) for "If you're editing C, C++, or Java"
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	      (cscope-minor-mode t)
	      (flycheck-mode t)
	      (lsp)
	      (helm-cscope-mode t)
	      (lsp-mode t)
	      (lsp-ui-mode t)
	      (openbsd-set-knf-style)
	      (unless (derived-mode-p 'java-mode)
		(check-cmake-project))
	      (vimish-fold-mode t)
	      (which-func-mode t)
	      ;; Cscope is super useful, albeit hard to use sometimes.
	      ;; Helm makes that a little easier :)
              (local-set-key (kbd "M-@")
			     'helm-cscope-find-calling-this-function)
	      (local-set-key (kbd "M-.") 'helm-cscope-find-global-definition)
              (local-set-key (kbd "M-s") 'helm-cscope-find-this-symbol)
              (local-set-key (kbd "M-,") 'helm-cscope-pop-mark))))



(add-hook 'c-mode-hook #'c-mode-common-hook)

;; *Java*

;; OpenBSD doesn't always set JAVA_HOME
(when (eq system-type 'berkeley-unix)
  (unless (eq (getenv "JAVA_HOME") nil)
    setenv "JAVA_HOME" "/usr/local/jdk-11"))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-java :ensure nil)

(add-hook 'java-mode-hook
	  (lambda()
	    (require 'dap-java)
	    (require 'lsp-java)
	    (lsp)
	    (dap-mode t)
	    (lsp-mode t)
	    (flycheck-mode t)))


;; *LaTeX*

;; Most people seem to prefer texlab, but Rust's package management system
;; *really* doesn't like it when rustup isn't supported on your system, and
;; fights you every step of the way seemingly, and digestif only needs lua and
;; git to work, both of which I generally have installed anyway
(defun fetch-digestif ()
  "Grab/install digestif"
  (progn
    (require 'url)
    (unless (file-exists-p "~/.emacs.d/bin/digestif")
      (unless (file-directory-p "~/.emacs.d/bin/")
	(mkdir "~/.emacs.d/bin/"))
      (url-copy-file (concat "https://github.com/astoff/digestif/"
			     "raw/master/scripts/digestif")
		     "~/.emacs.d/bin/digestif")
      (shell-command "sh ~/.emacs.d/bin/digestif"))))


(use-package tex-mode
  :ensure auctex
  :config
  ;; Allows you to view your latex report side by side without having to open up
  ;; evince or something
  (use-package latex-preview-pane))

;; Autocomplete for latex

(add-hook 'tex-mode-hook
	  (lambda ()
	    (lsp-mode t)
	    (fetch-digestif)
	    (flycheck-mode t)))

;; *MATLAB*
(use-package matlab
  :ensure matlab-mode
  :config
  (add-to-list
   'auto-mode-alist
   '("\\.m\\'" . matlab-mode)))


;; *Perl*


(defalias 'perl-mode 'cperl-mode)
(use-package helm-perldoc)
(add-hook 'cperl-mode-hook
	  (lambda()
	    (lsp)
	    (setq indent-tabs-mode nil)
	    (setq cperl-indent-level 4)
	    (flycheck-mode t)))

(setq safe-local-variable-values '((eval cperl-set-style "BSD")))
(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))

;; *Shell*
(use-package flycheck-checkbashisms)
(add-hook 'shell-mode-hook
	  (lambda ()
	    (flycheck-mode t)
	    (sh-posix-bash)))

;; *TCL*

(add-hook 'tcl-mode-hook (flycheck-mode t))

;; -----------------------------------------------------------------------------
;; General Emacs Config
;; -----------------------------------------------------------------------------

;; Less copy and pasting
(use-package yasnippet :config
  (progn
    (yas-global-mode)
    (use-package yasnippet-snippets)
    (use-package helm-c-yasnippet)))


(defun insert-isc-license () (interactive) (insert (isc-license)))



;; Absolutely ugly hack but it works
(defun isc-license ()
  "Returns a commented version of the ISC license"
  (let ((local-mode major-mode)
	(year (nth 5 (decode-time))))
      (with-temp-buffer
	(delay-mode-hooks (funcall local-mode))
	(beginning-of-line)
	(open-line 1)
	(insert-before-markers
	 (concat "%@%Copyright (c) " (number-to-string year)
		 " Charlie Burnett <burne251@umn.edu>
%@%
%@%Permission to use, copy, modify, and distribute this software for any
%@%purpose with or without fee is hereby granted, provided that the above
%@%copyright notice and this permission notice appear in all copies.
%@%
%@%THE SOFTWARE IS PROVIDED \"AS IS\" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%@%WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%@%MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%@%ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%@%WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%@%ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%@%OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE."))
	(if (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	    (progn
	      (goto-char (point-min))
	      (insert-before-markers "/*\n")
	      (while (search-forward "%@%" nil t)
		(replace-match " * "))
	      (goto-char (point-max))
	      (insert " */"))
	  (progn
	    (comment-region (point-min) (point-max))
	    (goto-char (point-min))
	    (while (search-forward "%@%" nil t)
	      (replace-match ""))))
	(delete-trailing-whitespace)
	(buffer-substring-no-properties (point-min) (point-max)))))

;; Copyright (c) 2021 Charlie Burnett <burne251@umn.edu>
;;
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


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

;; I usually run in full screen mode for Emacs, so this helps a lot
(display-battery-mode)
(display-time-mode)

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

(use-package magit)

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

(setq comp-async-report-warnings-errors nil)
(setq warning-suppress-log-types '((comp)))
(setq warning-suppress-types '((comp)))


;; I can't stand it when Emacs wraps lines
(setq-default truncate-lines 1)

;; Anything here has the fill column marker disabled
(add-hook 'after-change-major-mode-hook
	  (lambda()
	    (when (derived-mode-p 'dashboard-mode 'matlab-shell-mode)
	      (display-fill-column-indicator-mode 0))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Don't know why they disable this...
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; -----------------------------------------------------------------------------
;; End of file
;; -----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(spotify yasnippet-snippets yafolding vimish-fold use-package treemacs-projectile matlab-mode magithub lsp-ui lsp-java latex-preview-pane helm-perldoc helm-lsp helm-cscope helm-c-yasnippet font-utils flycheck-projectile flycheck-clang-analyzer flycheck-checkbashisms doom-themes doom-modeline dockerfile-mode dashboard company-ctags company-c-headers cmake-project cmake-mode cmake-ide centaur-tabs bison-mode auctex anzu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
