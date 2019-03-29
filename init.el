;;; init --- My Emacs Config
;;; -*- mode: lisp -*-
;;; Commentary:
;;; Code:

;; Customize BS

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Load my-packages for require
(add-to-list 'load-path "~/.emacs.d/my-packages")
(require 'my-packages)
(require 'c-style)

;; Increase GC Threshold - Speeds up startup
(setq-default gc-cons-threshold 10000000)

;;; Theme and Looks
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/tomorrow-theme")
(add-to-list 'load-path "~/.emacs.d/themes/tomorrow-theme")
(load-theme 'tomorrow-night t)

;; Font
(set-face-attribute 'default nil :font "Iosevka Type Light-9")

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))

;; Disable background color so transparency is nicer
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;;; Basic Stuff
;; Make yes or no prompts be y or n prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable menu-bars, scroll-bars, and other nonsense
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Show what column I'm in
(column-number-mode t)

;; Useful Mouse
(xterm-mouse-mode t)

;; Show Matching parens
(setq show-paren-delay 0) ; Quickly
(show-paren-mode t)

(setq-default inhibit-startup-message    t                 ; I hate the startup message
	      make-backup-files          nil               ; I hate these more
	      vc-follow-symlinks         t                 ; Why would you not do this?
	      frame-title-format         "%@%b%* - emacs"  ; Useful window title
	      gdb-many-windows           t                 ; GDB Mode is Awesome
	      diff-switches              "-u"
	      scroll-step                10
	      cua-mode                   nil               ; C-c,v,x is for the un-enlightened
	      cua-auto-tabify-rectangles nil)

;; Auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;;; Org-Mode Stuff
;; Syntax Highlight Source blocks
(setq org-src-fontify-natively t)

;; http://orgmode.org/worg/org-contrib/org-collector.html
(require 'org-collector)

;; Adds links to man pages through C-c C-l
(require 'org-man)

;; http://orgmode.org/worg/org-contrib/org-wikinodes.html
(require 'org-wikinodes)

;;; Plugins
;; Aggressive Indent Mode
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;; Auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Clean AIndent Mode
(set 'clean-aindent-is-simple-indent t)
;; (add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Delight
(delight '((abbrev-mode)
	   (evil-commentary-mode "" evil-commentary)
	   (aggressive-indent-mode "" aggressive-indent)
	   (color-identifiers-mode "" rainbow-identifiers)
	   (company-mode "" company)
	   (flycheck-mode "" flycheck)
	   (helm-mode "")
	   (indent-guide-mode " »|" indent-guide)
	   (projectile-mode "" projectile)
	   (projectile-rails-mode " RoR" projectile-rails)
	   (global-whitespace-mode "" whitespace)
	   (subword-mode " -,_" subword)
	   (undo-tree-mode "" undo-tree)
	   (vi-tilde-fringe-mode "" vi-tilde-fringe)
	   (ws-butler-mode "" ws-butler)
	   ))

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(ruby-reek))
;; flycheck-rubocoprc (concat (file-name-as-directory (ignore-errors projectile-project-root)) ".rubocop.yml")

;; Function Args
(fa-config-default)
(set-default 'semantic-case-fold t)

;; Helm
(helm-mode 1)
(setq helm-autoresize-mode t)
(setq helm-buffer-max-length 40)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Indent-guide
(indent-guide-global-mode)

;; (Relative) Line Numbers
(require 'relative-linum)
(global-linum-mode t)

;; Org-Bullets
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Projectile
(projectile-mode +1)

;; Rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Rainbow-identifiers
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

;; Semantic
(semantic-mode t)
(global-semanticdb-minor-mode t)
(global-semantic-idle-scheduler-mode t)
(global-semantic-idle-summary-mode t)
(global-semantic-stickyfunc-mode t)
(semantic-add-system-include "/usr/include")

;; Smart-mode-line
(sml/setup)
(sml/apply-theme 'dark)

;; VI Fringe Tilde
(global-vi-tilde-fringe-mode t)

;; Whitespace
(add-hook 'after-init-hook 'global-whitespace-mode)
(setq whitespace-style (list 'face 'trailing))

;; WS Butler
(add-hook 'prog-mode-hook 'ws-butler-mode)

;;; C/C++
(defun my:c/c++-hook ()
  (setq my-c-include-paths (split-string
			    "/usr/local/include
                             /usr/include"))
  (setq c-default-style "bsd-knf"
	c-basic-offset 4
	whitespace-style (list 'face 'trailing)
	flycheck-gcc-include-path my-c-include-paths
	flycheck-gcc-openmp t
	flycheck-disabled-checkers '(c/c++-clang) ;; Clang is stupid
	)
  (which-function-mode t)

  ;; Company additions that are c/c++ mode specific
  (add-to-list 'company-backends 'company-c-headers)
  (define-key c-mode-map [(tab)] 'company-complete)
  (define-key c++-mode-map [(tab)] 'company-complete)
  )
(add-hook 'c-mode-hook 'my:c/c++-hook)

;;; Lisps
;; ELisp
(defun my-elisp-hook()
  (turn-on-eldoc-mode)
  )
(add-hook 'emacs-lisp-mode-hook 'my-elisp-hook)

(setq inferior-lisp-program "sbcl")

;; Sly
(setq sly-complete-symbol-function 'sly-flex-completions)

;;; Ruby
(defun my:ruby-hook ()
  (inf-ruby-minor-mode +1)
  (projectile-rails-mode)
  (subword-mode +1))
(add-hook 'ruby-mode-hook 'my:ruby-hook)

;;; EVIL Stuff
(setq evil-want-integration t) ;; Needed for evil-collection
(setq evil-want-keybinding nil) ;; Needed for evil-collection
(evil-mode 1)

;; Evil-Collection
(evil-collection-init)

;; Evil-Commentary
(evil-commentary-mode)

;; Evil-Matchit
(global-evil-matchit-mode 1)

;; Evil-Exchange
(setq evil-exchange-key (kbd "gx"))
(evil-exchange-install)

;; Evil-Surround
(global-evil-surround-mode 1)

;; Evil-Leader
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  ;; Emacs Shortcuts
  "m"  'execute-extended-command
  ";"  'eval-expression
  "hv" 'describe-variable
  "hk" 'describe-key

  ;; Highlight-symbol
  "hs" 'auto-highlight-symbol-mode

  ;; Multiple-Cursors
  "cn" 'mc/mark-next-like-this
  "cp" 'mc/mark-previous-like-this
  "ca" 'mc/mark-all-like-this

  ;; Buffer/File operations
  "fs" 'whitespace-cleanup
  "t" projectile-command-map

  ;; Evil-Numbers maps
  "na" 'evil-numbers/inc-at-pt
  "nx" 'evil-numbers/dec-at-pt

  ;; Rainbow-Mode
  "rr" 'rainbow-mode
  "rs" 'fancy-narrow-to-region
  "re" 'fancy-widen

  ;; Splits
  "sv" 'evil-window-vsplit
  "sh" 'evil-window-split
  "sc" 'evil-quit
  "so" 'delete-other-windows

  ;; Window Controls
  "ww" 'evil-window-next
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right

  ;; Expand-Region
  "v" 'er/expand-region
  )

;; EVIL Bindings
;; Swap v and C-v, block-visual is much more useful
(define-key evil-normal-state-map (kbd "v") 'evil-visual-block)
(define-key evil-normal-state-map (kbd "C-v") 'evil-visual-char)
;; Set j/k to do gj/gk
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "g j") 'evil-next-line)
(define-key evil-normal-state-map (kbd "g k") 'evil-previous-line)
;; Function args
(define-key evil-insert-state-map (kbd "C-h") 'moo-complete)

;;; init.el ends here
