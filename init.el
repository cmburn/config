;; Copyright (c) 2019-2023 Charlie Burnett <cmburnett17@protonmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(when (version< emacs-version "29.0")
  (error "This config requires at least Emacs 29"))

;; Builtin dependencies
(require 'display-line-numbers)
(require 'package)
(require 'treesit)
(require 'url)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-archive-priorities '(("melpa"  . 100)
                                   ("gnu"    .  50)
                                   ("nongnu" .  25)))

;; Pull in use-package if we haven't run before
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :init
  (setq use-package-always-ensure t)
  (use-package use-package-ensure-system-package
    :ensure t))

;; Bootstrap straight.el, the Emacs package manager used here.
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com/"
		 "radian-software/straight.el/develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Doesn't work < v27.0
(unless (version< emacs-version "27.0")
  (setq package-enable-at-startup nil))


;; Security tweaks
(setq tls-checktrust t)
(setq gnutls-verify-error t)
(let ((trustfile "/etc/ssl/cert.pem"))
  (setq tls-program
        `(,(format  "gnutls-cli --x509cafile %s -p %%p %%h" trustfile)
          ,(format
	    "openssl s_client -connect %%h:%%p -CAfile %s -no_ssl2 -ign_eof"
	    trustfile)))
  (setq gnutls-trustfiles (list trustfile)))


(defconst package-list
  (list
   ;; Stuff to improve the Emacs UI
   'all-the-icons 'centaur-tabs 'doom-modeline 'doom-themes 'font-utils
   ;; Highlights what we're doing during search/replace operations
   'anzu
   ;; Autocomplete framework
   'company 'company-c-headers 'company-ctags
   ;; Welcome page for emacs with our last-edited files
   'dashboard
   ;; IDE Language Server Protocol
   'lsp-mode 'dap-mode 'helm-lsp 'lsp-treemacs
   ;; Build helper for Emacs
   'flymake
   ;; Emacs command autocomplete framework
   'helm 'helm-cscope 'helm-perldoc
   ;; Incremental file parser
   'tree-sitter
   ;; Directory display tool
   'treemacs
   ;; File/code templating
   'yasnippet
   ;; C symbol database
   'xcscope
   ;; Miscellaneous Language modes
   'bazel
   'bison-mode
   'dockerfile-mode
   'elixir-ts-mode
   'go-mode
   'matlab-mode
   'tex-mode)
  "Packages to be installed")



;; Pull everything in
(mapc 'straight-use-package package-list)

(add-hook 'elixir-mode 'lsp)

(setq treesit-language-source-alist
 '((heex "https://github.com/phoenixframework/tree-sitter-heex")
   (elixir "https://github.com/elixir-lang/tree-sitter-elixir")))

(setq major-mode-remap-alist
 '((elixir-mode . elixir-ts-mode)))

(mapc #'treesit-install-language-grammar
      (mapcar #'car treesit-language-source-alist))

(defun fetch-unless-exists (url directory &optional name)
  "Grabs the file if it doesn't exist"
  (when (not (file-directory-p directory))
    (make-directory directory))
  (let* ((file (if name name (file-name-nondirectory url)))
	 (output (concat directory "/" file)))
    (unless (file-exists-p file) (url-copy-file url file))))


(fetch-unless-exists (concat "https://raw.githubusercontent.com/"
			     "hogand/openbsd-knf-emacs/master/"
			     "openbsd-knf-style.el")
		     "~/.emacs.d/elisp")
(fetch-unless-exists (concat "https://github.com/astoff/digestif/"
			     "raw/master/scripts/digestif")
		     "~/.emacs.d/bin")


;; Custom functions/variables
(defcustom display-line-numbers-exempt-modes
  '(eshell-mode
    shell-mode
    matlab-shell-mode
    dashboard-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers depending on the mode"
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(defun fetch-digestif ()
  "Grab digestif for LaTeX"
  (progn
    (unless (file-exists-p "~/.emacs.d/bin/digestif")
      (unless (file-directory-p "~/.emacs.d/bin/")
	(mkdir "~/.emacs.d/bin/"))
      (url-copy-file (concat "https://github.com/astoff/digestif/"
			     "raw/master/scripts/digestif")
		     "~/.emacs.d/bin/digestif")
      (shell-command "sh ~/.emacs.d/bin/digestif"))))

;; Modify our hooks

(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (when (derived-mode-p 'dashboard-mode 'matlab-shell-mode)
	      (display-fill-column-indicator-mode 0))))
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'c-mode-common-hook
	  (lambda()
	    (when (derived-mode-p 'c-mode 'c++-mode)
	      (cscope-minor-mode t)
	      (helm-cscope-mode t)
	      (lsp)
	      (vimish-fold-mode t)
	      (which-func-mode t)
	      (local-set-key (kbd "M-.") 'helm-cscope-find-global-definition)
	      (local-set-key (kbd "M-s") 'helm-cscope-find-this-symbol)
	      (local-set-key (kbd "M-,") 'helm-cscope-pop-mark)
	      (local-set-key (kbd "M-@")
			     'helm-cscope-find-calling-this-function))))



(add-hook 'cperl-mode-hook
	  (lambda()
	    (setq indent-tabs-mode nil)
	    (setq cperl-indent-level 4)
	    (cperl-set-style 'PBP)
	    (lsp)))



(add-hook 'go-mode-hook 'lsp)

(add-hook 'prog-mode-hook
	  (lambda ()
	    (display-fill-column-indicator-mode)))

(add-hook 'tex-mode-hook
	  (lambda ()
	    (fetch-digestif)
	    (flycheck-mode t)))

(add-hook 'text-mode-hook (lambda () (display-fill-column-indicator-mode)))

;; General settings stuff

(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))
(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))
(auto-compression-mode 1)
(centaur-tabs-mode)
(column-number-mode 1)
(dashboard-setup-startup-hook)
(defalias 'perl-mode 'cperl-mode)
(display-battery-mode)
(display-time-mode)
(doom-modeline-mode)
(electric-pair-mode 1)
(global-display-line-numbers-mode)
(global-font-lock-mode 1)
(global-set-key (kbd "C-h a") 'helm-apropos)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode t)
(line-number-mode 1)
(load-theme 'doom-material-dark t)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq backup-directory-alist '(("." . "~/.emacs-backups.d/")))
(setq blink-matching-paren 1)
(setq case-replace nil)
(setq centaur-tabs-height 28)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-style "bar")
(setq comp-async-report-warnings-errors nil)
(setq dashboard-center-content t)
(setq doom-modeline-height 32)
(setq doom-modeline-icon t)
(setq query-replace-highlight 1)
(setq require-final-newline 1)
(setq safe-local-variable-values '((eval cperl-set-style "PBP")))
(setq search-highlight 1)
(setq show-paren-delay 0 show-paren-style 'parenthesis)
(setq warning-suppress-log-types '((comp)))
(setq warning-suppress-types '((comp)))
(setq whitespace-style
      '(trailing lines space-before-tab)
      whitespace-line-column 80)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default fill-column 80)
(setq-default show-trailing-whitespace 1)
(setq-default truncate-lines 1)
(defconst screen-ratio
  (/ (float (x-display-pixel-width)) (x-display-pixel-height)))
(setq treemacs-width (if (> screen-ratio (/ (float 16) 9)) 48 32))
(setq load-path (cons "~/.emacs.d/elisp/" load-path))
(show-paren-mode 1)
(tool-bar-mode -1)
(transient-mark-mode 1)
