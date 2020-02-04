;;; init.el --- simple Emacs configuration
;; -*- lexical-binding: t; coding:utf-8; fill-column: 80 -*-

;;; Commentary:
;;

;;; Code:

;;; Bootstrap
;; Speed up startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;; Initialize package.el
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap `use-package'
(setq-default use-package-always-ensure t
              use-package-always-defer t
              use-package-verbose nil
              use-package-expand-minimally t
              use-package-enable-imenu-support t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;; Generic
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(normal-erase-is-backspace-mode 0)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq column-number-mode t
      line-number-mode t)
(setq save-abbrevs nil)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Coding system
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;; Quiet startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; Macros
(load "defuns-config.el")

;; Key-bindings
(use-package bind-key)
(fset 'align-equals "\C-[xalign-regex\C-m=\C-m")
(bind-key "M-=" 'align-equals)
(bind-key "C-x C-m" 'execute-extended-command)
(bind-key "C-c ;" 'comment-or-uncomment-region)
(bind-key "M-n" 'next5)
(bind-key "M-p" 'prev5)
(bind-key "M-o" 'other-window)
(bind-key "M-i" 'back-window)
(bind-key "C-z" 'zap-to-char)
(bind-key "C-h" 'backward-delete-char)
(bind-key "M-d" 'delete-word)
(bind-key "M-h" 'backward-delete-word)
(bind-key "M-u" 'zap-to-char)

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;; IME
(use-package mozc
  :if (and (eq system-type 'gnu/linux) window-system)
  :commands (mozc-mode mozc-handle-event)
  :custom (default-input-method "japanese-mozc")
  :bind (("M-l" . mozc-start)
         :map mozc-mode-map
         ("," . (lambda () (interactive) (mozc-insert-str "、")))
         ("." . (lambda () (interactive) (mozc-insert-str "。")))
         ("?" . (lambda () (interactive) (mozc-insert-str "？")))
         ("!" . (lambda () (interactive) (mozc-insert-str "！")))
         ("C-m" . (lambda () (interactive) (mozc-insert-str "")))
         ("<enter>" . (lambda () (interactive) (mozc-insert-str "")))
         ("C-h" . delete-backward-char))
  :preface
  (defun mozc-start()
    (interactive)
    (message "Mozc start")
    (mozc-mode 1))
  (defun mozc-end()
    (interactive)
    (mozc-handle-event 'enter)
    (message "Mozc end")
    (mozc-mode -1))
  (defun mozc-insert-str (str)
    (mozc-handle-event 'enter)
    (insert str))
  :config
  (mozc-mode 1)
  ;; Show candidates style
  (use-package mozc-popup
    :custom (mozc-candidates-style "popup")))

;; whitespace
(use-package whitespace
  :commands whitespace-mode
  :bind
  ("C-c W" . whitespace-cleanup)
  :init
  (setq whitespace-style '(face empty spaces space-mark tabs tab-mark
                                trailing))
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (setq whitespace-space-regexp "\\(\x3000+\\)")
  (global-whitespace-mode t))

;; posframe
(use-package posframe
  :if window-system)

;; scrolling
(use-package smooth-scrolling
  :custom
  (scroll-step 1)
  (scroll-conservatively 10000)
  (auto-window-vscroll nil))

;;; UI
(use-package solarized-theme
  :init (load-theme 'solarized-dark t))

(use-package fill-column-indicator
  :init
  (setq fci-rule-column 80)
  (setq fci-rule-color (face-attribute 'highlight :background))
  :hook
  ((markdown-mode
    git-commit-mode) . fci-mode))

(use-package persp-mode
  :disabled
  :diminish
  :commands (get-current-persp persp-contain-buffer-p)
  :hook (after-init . persp-mode))

;;; High-light
(global-hl-line-mode t)
(show-paren-mode t)
(defvar show-paren-style 'mixed)
(transient-mark-mode t)

(use-package beacon
  :custom
  (beacon-color "#f1fa8c")
  :hook (after-init . beacon-mode))

(use-package highlight-indent-guides
  :diminish
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode))

(use-package rainbow-mode
  :diminish
  :hook (emacs-lisp-mode . rainbow-mode))

(use-package volatile-highlights
  :commands (vhl/disable-advice-if-defined vhl/install-extension)
  :config
  (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  )

;;; Undo/Redo
(use-package undo-tree
  :bind
  ("M-/" . undo-tree-redo)
  :init
  (global-undo-tree-mode))

;;; Search
(use-package ag
  :custom
  (ag-highligh-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  :config
  (use-package wgrep-ag))

(use-package anzu
  :diminish
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (after-init . global-anzu-mode))

(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind
  ("C-s"     . swiper)
  ("M-x"     . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f"  . counsel-describe-function)
  ("<f1> v"  . counsel-describe-variable)
  ("<f1> l"  . counsel-find-library)
  ("<f2> i"  . counsel-info-lookup-symbol)
  ("<f2> u"  . counsel-unicode-char)
  ("C-c C-r" . ivy-resume)
  ("C-c k"   . counsel-ag)
  :hook
  (after-init . ivy-mode)
  (ivy-mode . counsel-mode)
  :config
  ;; Enhance fuzzy matching
  (use-package flx)
  ;; Enhance M-x
  (use-package amx)
  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :config (counsel-projectile-mode 1))
  ;; Show ivy frame using posframe
  (use-package ivy-posframe
    :if window-system
    :init
    (setq ivy-posframe-display-functions-alist
          '((t . ivy-posframe-display-at-frame-center)))
    (ivy-posframe-mode 1)
    :config
    (setq ivy-posframe-parameters
          '((left-fringe . 5)
            (right-fringe . 5)))))

(use-package projectile
  :diminish
  :after ivy
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;;; Completion
(use-package company
  :diminish company-mode
  :bind (:map company-active-map
         ("C-n"   . company-select-next)
         ("C-p"   . company-select-previous)
         ("<tab>" . company-complete-common-or-cycle)
         ("C-f"   . company-complete-selection)
         :map company-search-map
         ("C-p"   . company-select-previous)
         ("C-n"   . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (completion-ignore-case t)
  (company-dabbrev-downcase nil)
  :hook
  (after-init . global-company-mode)
  :config
  ;; using child frame
  (use-package company-posframe
    :if window-system
    :init
    (company-posframe-mode 1))
  ;; Show quick tooltip
  (use-package company-quickhelp
    :hook (global-company-mode . company-quickhelp-mode)))

(use-package yasnippet
  :diminish yas-minor-mode
  :custom (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :hook (after-init . yas-global-mode))

;;; Cursor
(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->"         . mc/mark-next-like-this)
  ("C-<"         . mc/mark-previous-like-this)
  ("C-c C-<"     . mc/mark-all-like-this))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;;; Check
(use-package flycheck
  :hook
  (after-init . global-flycheck-mode))

(use-package flyspell
  :diminish
  :if (executable-find "aspell")
  :hook
  ((org-mode yaml-mode markdown-mode git-commit-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  (before-save-hook . flyspell-buffer)
  (flyspell-mode . (lambda ()
                     (dolist (key '("C-;" "C-," "C-."))
                       (unbind-key key flyspell-mode-map))))
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :custom-face
  (flyspell-incorrect ((t (:underline (:color "#f1fa8c" :style wave)))))
  (flyspell-duplicate ((t (:underline (:color "#50fa7b" :style wave)))))
  :config
  (setq use-dialog-box nil)
  (use-package flyspell-correct-ivy
    :bind ("C-M-;" . flyspell-correct-at-point)
    :init
    (setq flyspell-correct-interface #'flyspell-correct-ivy)))

;;; Git
(use-package diffview
  :bind ("M-g v" . diffview-current))

(use-package gitattributes-mode)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package git-gutter
  :init
  (global-git-gutter-mode))

(use-package git-timemachine
  :bind ("M-g t" . git-timemachine-toggle))

;;; Tool
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;; Language
;; LSP
(use-package lsp-mode
  :commands lsp
  :custom
  ;; Debug
  (lsp-log-io t)
  (lsp-trace t)
  (lsp-print-performance t)
  ;; General
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental) ; none, full, incremental, or nil
  (lsp-prefer-flymake nil) ; t(flymake), nil(lsp-ui), or :none
  (lsp-response-timeout 10)
  (lsp-auto-configure t)
  :hook
  ((c-mode c++-mode python-mode rust-mode) . lsp)
  :config
  ;; LSP UI tools
  (use-package lsp-ui :commands lsp-ui-mode)
  ;; Lsp completion
  (use-package company-lsp :commands company-lsp))

;; C/C++
(use-package cc-mode
  :bind (:map c-mode-base-map
         ("C-c c" . compile))
  :hook (c-mode-common . (lambda ()
                            (c-set-style "k&r")
                            (setq c-basic-offset 4)
                            (setq tab-width 4))))

;; Dockerfile
(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

;; JavaScript
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))

;; Log
(use-package logview)

;; Markdown
(use-package markdown-mode
  :custom
  (markdown-hide-markup nil)
  (markdown-bold-underscore t)
  (markdown-italic-underscore t)
  (markdown-header-scaling t)
  (markdown-indent-function t)
  (markdown-enable-math t)
  (markdown-hide-urls nil)
  :mode "\\.md\\'")

(use-package markdown-toc)

;; Protobuf
(use-package protobuf-mode
  :mode "\\.proto$")

;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; R
(use-package ess
  :disabled
  :custom (ess-ask-for-ess-directory nil)
  :config
  (require 'ess-site))

;; Rust
(use-package rust-mode
  :custom
  (rust-format-on-save t)
  :mode "\\.rs\\'")

;; Yaml
(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

;;; init.el ends here
