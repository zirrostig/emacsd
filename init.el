;;; init --- My Emacs Config
;;; -*- mode: lisp -*-
;;; Commentary:
;;; Code:

;; Customize BS
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Load my-packages for require
(add-to-list 'load-path "~/.emacs.d/my-packages")
(require 'my-packages)
(require 'c-style)

;;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/tomorrow-theme")
(add-to-list 'load-path "~/.emacs.d/themes/tomorrow-theme")
(load-theme 'tomorrow-night t)
;; Font
(set-face-attribute 'default nil :font "Inconsolatazi4-12")
;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))
;; Disable background color so transparency is nicer
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)
;; Highlight current line
(add-hook 'after-init-hook 'global-hl-line-mode)

;;; Change starting modes
(menu-bar-mode -1)t
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(show-paren-mode t)
(xterm-mouse-mode t)

(setq-default inhibit-startup-message    t
	      make-backup-files          nil
	      vc-follow-symlinks         t
	      frame-title-format         "%@%b%* - emacs"
	      gdb-many-windows           t
	      diff-switches              "-u"

	      scroll-step                10
	      cua-mode                   nil
	      cua-auto-tabify-rectangles nil)

;; Whitespace
(add-hook 'after-init-hook 'global-whitespace-mode)
(setq whitespace-style (list 'face 'trailing))

;; (Relative) Line Numbers
(require 'relative-linum)
(global-linum-mode t)

;;; Plugins
;; Aggressive Indent Mode
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;; Auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;;(setq-default TeX-master nil)

;; Clean AIndent Mode
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Color-identifiers
(setq color-identifiers:num-colors 50)
(add-hook 'prog-mode-hook 'global-color-identifiers-mode)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Diminished
(eval-after-load "filladapt" '(diminish 'filladapt-mode))

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Function Args
(fa-config-default)
(set-default 'semantic-case-fold t)

;; Indent-guide
(indent-guide-global-mode)

;; Golden-ratio
(golden-ratio-mode 1)

(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

;; Helm
(setq-default helm-move-to-line-cycle-in-source t
	      helm-autoresize-max-height 50
	      helm-autoresize-min-height 20
	      helm-buffers-fuzzy-matching t
	      helm-recentf-fuzzy-match t
	      )
(helm-autoresize-mode t)
(global-set-key (kbd "C-x C-f") 'helm-find-files) ; Replace default file-finder
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(helm-mode 1)

;; Nyan Mode
(nyan-mode 1)
(nyan-start-animation)

;; Org-Bullets
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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

;; WS Butler
(add-hook 'c-mode-common-hook 'ws-butler-mode)


;;; C/C++
(defun my:c/c++-hook ()
  (setq my-c-include-paths (split-string
                            "/usr/include/ImageMagick-6
                           /usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.2/include
                           /usr/local/include
                           /usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.2/include-fixed
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
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.9.2/") ; Will have to change when g++ updates
  (define-key c-mode-map [(tab)] 'company-complete)
  (define-key c++-mode-map [(tab)] 'company-complete)
  )
(add-hook 'c-mode-hook 'my:c/c++-hook)

;;; Lisps
(defun my-lisp-hook()
  (turn-on-eldoc-mode)
  )
(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)

;;; EVIL Stuff
(evil-mode 1)

;; Evil-Nerd-Commenter
(setq evilnc-hotkey-comment-operator "gc")

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

  ;; Search
  "hf" 'helm-apropos
  "hg" 'helm-google-suggest
  "hl" 'helm-locate
  "hs" 'helm-surfraw ; For everything else

  ;; Highlight-symbol
  "hs" 'auto-highlight-symbol-mode

  ;; Make
  "b" 'helm-make

  ;; Multiple-Cursors
  "cn" 'mc/mark-next-like-this
  "cp" 'mc/mark-previous-like-this
  "ca" 'mc/mark-all-like-this

  ;; Frames
  "fn" 'make-frame-command
  "fo" 'other-frame

  ;; Buffer/File operations
  "fs" 'whitespace-cleanup

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

  ;; Helm-mini
  "to" 'helm-mini

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
; Swap v and C-v, block-visual is much more useful
(define-key evil-normal-state-map (kbd "v") 'evil-visual-block)
(define-key evil-normal-state-map (kbd "C-v") 'evil-visual-char)
;; Set j/k to do gj/gk
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "g j") 'evil-next-line)
(define-key evil-normal-state-map (kbd "g k") 'evil-previous-line)
;; Make arrow keys useful
(define-key evil-normal-state-map (kbd "<up>") 'previous-buffer)
(define-key evil-normal-state-map (kbd "<down>") 'next-buffer)
;; Function args
(define-key evil-insert-state-map (kbd "C-h") 'moo-complete)

;;; init.el ends here
