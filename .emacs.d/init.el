;; -*- mode: emacs-lisp -*-
;; Simple .emacs configuration

;; ------------------------
;; -- Package Management --
;; ------------------------
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(defun install-packages (packages)
  (let ((refreshed nil))
    (dolist (pack packages)
      (unless (package-installed-p pack)
        (unless refreshed
          (package-refresh-contents)
          (setq refreshed t))
        (package-install pack)))))

(install-packages '(anzu
                    auto-complete
                    bind-key
                    column-marker
                    dash
                    dockerfile-mode
                    drag-stuff
                    epl
                    exec-path-from-shell
                    expand-region
                    f
                    flycheck
                    fuzzy
                    git-commit
                    git-gutter-fringe
                    gitconfig-mode
                    gitignore-mode
                    google-c-style
                    haskell-mode
                    helm
                    htmlize
                    idle-highlight-mode
                    ido
                    ido-ubiquitous
                    jade-mode
                    js2-mode
                    let-alist
                    magit
                    markdown-mode
                    multiple-cursors
                    nyan-mode
                    pkg-info
                    popup
                    popwin
                    powerline
                    prodigy
                    projectile
                    rainbow-delimiters
                    rust-mode
                    s
                    smartparens
                    smex
                    smooth-scrolling
                    solarized-theme
                    sws-mode
                    undo-tree
                    use-package
                    volatile-highlights
                    web-mode
                    yaml-mode
                    yascroll
                    yasnippet))

;; ---------------------
;; -- Global Settings --
;; ---------------------
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'ansi-color)
(require 'compile)
(require 'ffap)
(require 'dired-x)
(require 'linum)
(require 'recentf)
(require 'smooth-scrolling)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(menu-bar-mode -1)
(normal-erase-is-backspace-mode 0)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq column-number-mode t)
(setq inhibit-startup-message t)
(setq save-abbrevs nil)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)

;; anzu mode
(require 'anzu)
(global-anzu-mode t)

;; auto-completion
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu 0.2)
(setq ac-delay 0.1)
(setq ac-use-fuzzy t)
(setq ac-use-menu-map t)

;; highlight
(global-hl-line-mode t)
(show-paren-mode t)
(setq show-paren-style 'mixed)
(transient-mark-mode t)
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; ido mode
(require 'ido)
(ido-mode t)
(ido-ubiquitous-mode t)
(setq ido-enable-flex-matching t)

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; uniquify file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; whitespace
(require 'whitespace)
(setq whitespace-style '(face
                         trailing
                         tabs
                         spaces
                         empty
                         space-mark
                         tab-mark
                         ))
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
        ))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(global-whitespace-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-marker-1 ((t (:background "red"))) t)
 '(fundamental-mode-default ((t (:inherit default))) t))

;;------------
;;-- Themes --
;;------------
(custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))
(load-theme 'solarized-dark t)

;; ---------------------
;; -- Syntax checking --
;; ---------------------
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ------------
;; -- Macros --
;; ------------
(load "defuns-config.el")
(fset 'align-equals "\C-[xalign-regex\C-m=\C-m")
(global-set-key "\M-=" 'align-equals)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c;" 'comment-or-uncomment-region)
(global-set-key "\M-n" 'next5)
(global-set-key "\M-p" 'prev5)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-i" 'back-window)
(global-set-key "\C-z" 'zap-to-char)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\M-d" 'delete-word)
(global-set-key "\M-h" 'backward-delete-word)
(global-set-key "\M-u" 'zap-to-char)

;; ---------------------------------
;; -- Markdown Mode configuration --
;; ---------------------------------
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; ---------------------------
;; -- JS Mode configuration --
;; ---------------------------
(load "js-config.el")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; ---------------------------
;; -- C/C++ Mode configuration --
;; ---------------------------
(require 'google-c-style)
(defun my-c-c++-mode-init ()
  (setq c-basic-offset 4)
  )
(add-hook 'c-mode-common-hook 'my-c-c++-mode-init)
(add-hook 'c++-mode-common-hook 'my-c-c++-mode-init)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c++-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c++-mode-common-hook 'google-make-newline-indent)

;; ---------------------------------
;; -- R Mode configuration --
;; ---------------------------------
(autoload 'R-mode "ess-site"
  "Emacs Speaks Statistics mode" t)
(add-to-list 'auto-mode-alist '("\\.[rR]$" . R-mode))

;; ---------------------------------
;; -- Haskell Mode configuration --
;; ---------------------------------
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;
