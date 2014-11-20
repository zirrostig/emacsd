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
  '(auctex
    auto-complete
    auto-complete-clang-async
    auto-complete-c-headers
    auto-highlight-symbol
    color-identifiers-mode
    company
    company-c-headers
    company-ghc
    elisp-slime-nav
    evil
    evil-exchange
    evil-leader
    evil-matchit
    evil-numbers
    evil-org
    evil-surround
    evil-tabs
    evil-visualstar
    expand-region
    flycheck
    haskell-mode
    helm
    highlight-symbol
    indent-guide
    latex-preview-pane
    markdown-mode
    multiple-cursors
    nyan-mode
    rainbow-delimiters
    slime
    smart-mode-line
    undo-tree
    visual-regexp
    visual-regexp-steroids
    xclip)
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
