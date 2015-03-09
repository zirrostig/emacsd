;;; my-packges --- Makes sure all my emacs packages are installed
;;; -*- mode: lisp -*-

;;; Commentary:
;; Taken from
;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-packages.el

;;; Code:
(eval-when-compile (require 'cl))
(require 'package)

;; Package Setup
(setq package-archives
             '(("melpa" . "http://melpa.milkbox.net/packages/")
               ("org" . "http://orgmode.org/elpa/")
               ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defvar my-packages
  '(aggressive-indent       ; Crazy alignment indent maintainer
    auctex                  ; Best LaTeX
    auto-highlight-symbol   ; Highlights what matches word under point
    c-eldoc                 ; Edit C with the comfort emacs gives you with lisp
    clean-aindent-mode      ; Mostly for the auto delete of WS due to auto-indent
    company                 ; Auto-complete
    company-c-headers       ; More Auto-complete
    delight                 ; Change major/minor mode display
    doc-view                ; pdfs in emacs makes LaTeX easier
    esup                    ; Emacs Start-Up Profiler
    evil                    ; VIM
    evil-anzu               ; match counter
    evil-commentary         ; TPope's commentary ported to evil
    evil-exchange           ; Swap motions
    evil-jumper             ; Jump list
    evil-leader             ; Leader Commands
    evil-matchit            ; Better % matches
    evil-numbers            ; VIM's C-a, C-x commands
    evil-org                ; Org-Mode with VIM
    evil-paredit            ; Paredit fixer for evil
    evil-surround           ; Change surrounding things like '([{<
    evil-visualstar         ; Gives a better #* command
    expand-region           ; Keep pressing the button to select more
    fancy-narrow            ; Restrict your working domain
    flycheck                ; On the fly syntax checker
    function-args           ; Documentation on the arguments when you need it
    golden-ratio            ; Resize splits to \phi
    helm                    ; Fuzzy everything
    helm-make               ; Choose what to build with helm
    highlight-symbol        ; Show matches for word under point
    indent-guide            ; Show indentation level
    markdown-mode           ; Better editor for Markdown
    multiple-cursors        ; Sublime?
    nyan-mode               ; Rainbow based file position
    org-bullets             ; utf-8 bullets in org-mode
    org-plus-contrib        ; lots of extra org-mode stuff
    popup                   ; Bubble info
    popwin                  ; No more BS buffers
    rainbow-delimiters      ; Colorize braces based on pairs
    rainbow-identifiers     ; Colorize variables based on hash
    slime                   ; LISP
    smart-mode-line         ; Clean up mode line so I don't have to
    smart-tabs-mode         ; \t's to indent, ' ' to align
    undo-tree               ; visualize and keep undo
    vi-tilde-fringe         ; Tilde's are back
    visual-regexp           ; makes :s better
    visual-regexp-steroids  ; makes :s even better
    ws-butler               ; Cleans trailing WS only on lines modified
    xclip)                  ; integrate with Xclip
  "All of my favorite plugins.")

(defun my-packages-installed-p ()
  "Return nil if there are packages that are not installed."
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun my-packages-install-packages ()
  "Install missing packages."
  (unless (my-packages-installed-p)
    ;; Referesh package lists
    (package-refresh-contents)
    ;; Install missing
    (dolist (p my-packages)
      (when (not (package-installed-p p))
        (package-install p)))))

(my-packages-install-packages)

(provide 'my-packages)
;;; my-packages.el ends here
