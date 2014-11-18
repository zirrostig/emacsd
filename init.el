;;; -*- mode: lisp -*-

;;; External Files
;; Customize BS
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Load my-packages for require
(add-to-list 'load-path "~/.emacs.d/my-packages")
(require 'my-packages)

;;; Turn off a bunch of stuff
;; Turn menu/scroll/tool bars off
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Disable Welcome Frame
(setq inhibit-startup-message t)

;; Turn of backup files
(setq make-backup-files nil)

;;; Looks
;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/tomorrow-theme")
(add-to-list 'load-path "~/.emacs.d/themes/tomorrow-theme")(menu-bar-mode -1)
(load-theme 'tomorrow-night t)

;; Some Transparency (if compositing)
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))

;; Disable background set (transparency in terminal)
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

;; Set Font
(set-face-attribute 'default nil :font "Inconsolatazi4-12")

;;; Display more information
;; Turn on column number
(column-number-mode t)

;; Show matching paren
(show-paren-mode t)

;; Highlight current line
(add-hook 'after-init-hook 'global-hl-line-mode)

;; Frame Title
;; CHANGEME: Someday
(setq frame-title-format (concat "%b - emacs@" (system-name)))

;; Whitespace
(add-hook 'after-init-hook 'global-whitespace-mode)
(setq whitespace-style (list 'face 'trailing 'tabs))

;; (Relative) Line Numbers
(global-linum-mode t)
(require 'relative-linum)

;;; Other things
;; Unified Diffs
(setq diff-switches "-u")

;; Scrolling Steps
(setq scroll-step 10)

;; Awesomeness for gdb-mode
(setq gdb-many-windows t)

;; No Tab Characters, spaces only
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

; Xterm-Mouse (Mostly for scrolling)
(xterm-mouse-mode t)

;;; Language Specific
;; C
(setq-default c-basic-offset 4
              c-default-style "k&r")

;; Perl
(defvaralias 'cperl-indent-level 'tab-width)

;;; Plugins
;; Auto-complete
(defun my:ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))

(add-hook 'c++-mode-hook 'my:ac-c-headers-init)
(add-hook 'c-mode-hook 'my:ac-c-headers-init)

;; Color-identifiers
(setq color-identifiers:num-colors 50)
(add-hook 'prog-mode-hook 'global-color-identifiers-mode)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Elisp-Slime-Nav
(defun my-lisp-hook()
  (elisp-slime-nav-mode)
  (turn-on-eldoc-mode)
  )
(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Helm
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)
(helm-mode 1)
(defun helm-my-buffers ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
  (helm-other-buffer '(helm-c-source-buffers-list
                       helm-c-source-elscreen
                       helm-c-source-projectile-files-list
                       helm-c-source-ctags
                       helm-c-source-recentf
                       helm-c-source-locate)
                     "*helm-my-buffers*")))

;; Indent-guide
(indent-guide-global-mode)

;; Markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Nyan Mode
(nyan-mode 1)
(nyan-start-animation)

;; Rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Smart-mode-line
(sml/setup)
(sml/apply-theme 'dark)

;; Undo-tree
(global-undo-tree-mode)

;;; EVIL Stuff
(evil-mode 1)

;; Evil-Matchit
(global-evil-matchit-mode 1)

;; Evil-Exchange
(setq evil-exchange-key (kbd "gx"))
(evil-exchange-install)

;; Evil-Surround
(global-evil-surround-mode 1)

;; Evil-Tabs
;(global-evil-tabs-mode t)

;; Evil-Leader
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  ; Emacs Shortcuts
  "m" 'execute-extended-command
  "hf" 'describe-function
  "hv" 'describe-variable
  "hk" 'describe-key

  ; Window Controls
  "ww" 'evil-window-next
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right

  ; Splits
  "sv" 'evil-window-vsplit
  "sh" 'evil-window-split
  "sc" 'evil-quit

  ; Tabs
  "tn" 'elscreen-create
  "tc" 'elscreen-kill

  ; Helm
  "to" 'helm-mini

  ; Frames
  "fn" 'make-frame-command
  "fo" 'other-frame

  ; Evil-Numbers maps
  "na" 'evil-numbers/inc-at-pt
  "nx" 'evil-numbers/dec-at-pt

  ; Highlight-symbol
  "hs" 'auto-highlight-symbol-mode

  ; Rainbow-Mode
  "rr" 'rainbow-mode

  ; Undo-Tree
  "u" 'undo-tree-visualize

  ; Multiple-Cursors
  "cn" 'mc/mark-next-like-this
  "cp" 'mc/mark-previous-like-this
  "ca" 'mc/mark-all-like-this

  ; Expand-Region
  "v" 'er/expand-region
  )

;; EVIL Bindings
; Swap v and C-v, block-visual is much more useful
(define-key evil-normal-state-map (kbd "v") 'evil-visual-block)
(define-key evil-normal-state-map (kbd "C-v") 'evil-visual-char)
; Set j/k to do gj/gk
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "g j") 'evil-next-line)
(define-key evil-normal-state-map (kbd "g k") 'evil-previous-line)
