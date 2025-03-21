;; Copyright (c) 2019-2022 Charlie Burnett <cmburnett17@protonmail.com>

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

;; Pull in straight.el, our package manager of choice


(require 'use-package-ensure)

(defvar bootstrap-version)


; Custom functions/variables
(defcustom display-line-numbers-exempt-modes
  '(eshell-mode
    shell-mode
    matlab-shell-mode
    treemacs-mode
    help-mode
    dashboard-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defcustom display-fill-column-indicator-exempt-modes
  '(dashboard-mode
    matlab-shell-mode)
  "Major modes on which to disable the display fill column indicator"
  :group 'display-fill-column-indicator
  :type 'list)

(defcustom tabless-modes
  '("*LSP"
    "*straight"
    "*Messages"
    "*lsp"
    "*digestif"
    "*helm"
    "*Flycheck"
    "*company"
    "*epc"
    "*helm"
    "*Helm"
    "*Compile-Log*"
    "*lsp"
    "*company"
    "*Flycheck"
    "*tramp"
    " *Mini"
    "*help"
    "*straight"
    " *temp"
    "*Help"
    "*mybuf")
  ""
  :group 'tab
  :type 'list)

(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     (some (lambda (str)
	     (string-prefix-p str)) tabless-modes)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name))))))

(defcustom ui-size
  120
  ""
  :group 'font
  :type 'integer)

(defcustom ui-font
  "Cantarell"
  ""
  :group 'font
  :type 'string)

(defun mark-ui-font (font &optional size-in)
  (let ((fonts (if (listp font) font '(font))))
    (mapcar
     (lambda (f)
       (let ((size (if size-in size-in ui-size)))
       (set-face-attribute
	f nil
	:family ui-font
	:height size
	:spacing (* size 1.1)))))))

(defun disable-exempt-modes ()
  "Disable exempt modes after a mode change"
  (display-line-numbers-mode
   (if (derived-mode-p display-line-numbers-exempt-modes)
       0
     1))
  (display-fill-column-indicator-mode
   (if (derived-mode-p display-fill-column-indicator-exempt-modes)
       0
     1)))

(defun mark-ui-font (face-arg)
  (let ((faces (if (listp face-arg)
		   face-arg
		 '(face-arg))))
    (mapcar
     (lambda (arg)
       (set-face-font arg ui-font))
	    face-arg)))

(use-package emacs
  :bind
  (("C-s" . isearch-forward-regexp)
   ("C-r" . i-search-backward-regexp)
   ("C-M-s" . isearch-forward)
   ("C-M-s" . isearch-backward))
  :hook ((text-mode . display-fill-column-indicator-mode)
	 (text-mode . display-line-numbers-mode)
	 (after-change-major-mode . disable-exempt-modes)
	 (tex-mode . flycheck-mode))
  :init
  (setq blink-matching-paren 1)
  (setq case-replace nil)
  (setq comp-async-report-warnings-errors nil)
  (setq query-replace-highlight 1)
  (setq require-final-newline 1)
  (setq show-paren-delay 0 show-paren-style 'parenthesis)
  (setq warning-suppress-log-types '((comp)))
  (setq warning-suppress-types '((comp)))
  (setq backup-directory-alist '(("." . "~/.emacs-backups.d/")))
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq-default fill-column 80)
  (setq-default show-trailing-whitespace 1)
  (setq-default truncate-lines 1)
  (setq dashboard-center-content t)
  (setq doom-modeline-height 32)
  (setq doom-modeline-icon t)
  (setq safe-local-variable-values '((eval cperl-set-style "PBP")))
  (setq search-highlight 1)
  (setq treemacs-width 24)
  (setq load-path (cons "~/.emacs.d/elisp/" load-path))
  (setq whitespace-style
       '(trailing lines space-before-tab)
       whitespace-line-column 80)
  (add-to-list 'default-frame-alist
	       '(font . "Menlo-12"))
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
  (setq straight-use-package-by-default 't)
  (setq use-package-always-ensure t)
  :config
  (scroll-bar-mode -1)
  (electric-pair-mode 1)
  (put 'downcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (global-display-line-numbers-mode)
  (line-number-mode 1)
  (show-paren-mode 1)
  (tool-bar-mode -1)
  (auto-compression-mode 1)
  (transient-mark-mode 1))

(use-package doom-modeline
  :init
  (setq doom-modeline-height 32)
  (setq doom-modeline-icon t)
  :config
  (doom-modeline-mode)
  (set-face-attribute 'doom-modeline nil :family ui-font :height ui-size))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package anzu
  :bind ("M-%" . anzu-query-replace-regexp))

(use-package helm
  :config (helm-mode t)
  :bind
  (("C-h a" . helm-apropos)
   ("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x)))

(use-package company
  :hook ((after-init . global-company-mode)))

(use-package treemacs
  :bind ("M-1" . treemacs)
  :config
  (mark-ui-font
   '(treemacs-directory-face
     treemacs-directory-collapsed-face
     treemacs-file-face
     treemacs-root-face
     treemacs-root-unreadable-face
     treemacs-root-remote-face
     treemacs-root-remote-unreadable-face
     treemacs-root-remote-disconnected-face
     treemacs-git-added-face
     treemacs-git-commit-diff-face
     treemacs-git-conflict-face
     treemacs-git-ignored-face
     treemacs-git-modified-face
     treemacs-git-renamed-face
     treemacs-git-unmodified-face
     treemacs-git-untracked-face
     treemacs-tags-face
     treemacs-help-title-face
     treemacs-help-column-face)))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))


(use-package lsp-mode
  :init
  (use-package lsp-treemacs
    :bind ("M-6" . lsp-treemacs-errors-list))
  (use-package dap-mode)
  :hook ((c-mode-common . lsp)
	 (go-mode . lsp)))

(use-package font-utils)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package projectile
  :init
  (use-package helm-projectile)
  (use-package treemacs-projectile))

(use-package editorconfig)
(use-package tree-sitter)

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package centaur-tabs
  :config
  (centaur-tabs-change-fonts ui-font ui-size)
  (centaur-tabs-mode t)
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-icon-type 'all-the-icons)
  (setq centaur-tabs-style "bar"))

(use-package flycheck)
(use-package vterm)

(use-package auctex
  :hook ((LaTeX-mode . lsp)
	 (LaTeX-mode . flycheck-mode)))
(use-package matlab-mode)
(use-package bison-mode)

