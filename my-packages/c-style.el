;;; c-style --- A Reasonable C/C++ style that doesn't make me want scream
;;; -*- mode: lisp -*-

;;; Commentary:
;; Style taken from Erik Änggård emacs config:
;; https://github.com/andoma/dotfiles/blob/master/emacs.d/c-style.el

;;; Code:
(defconst bsd-knf-style
  '((c-auto-newline . nil)
    ;; (c-tab-always-indent . nil)
    (c-recognize-knr-p . t)
    (c-basic-offset . 8)
    (c-comment-only-line-offset . 0)
    (c-cleanup-list . (brace-else-brace
		       empty-defun-braces
		       defun-close-semi
		       list-close-comma
		       scope-operator))
    (c-hanging-braces-alist . ((defun-open . (before after))
			       (defun-close . (before))
			       (class-open . (after))
			       (class-close . nil)
			       (inline-open . nil)
			       (inline-close . nil)
			       (block-open . (after))
			       (block-close . (before))
			       (substatement-open . nil)
			       (statement-case-open . nil)
			       (brace-list-open . nil)
			       (brace-list-close . nil)
			       (brace-list-intro . nil)
			       (brace-list-entry . nil)
			       ))
    (c-offsets-alist . ((knr-argdecl-intro . +)
			(arglist-cont-nonempty . 4)
			(knr-argdecl . 0)
			(block-open . -)
			(label . -)
			(statement-cont . 4)
			)))
  "BSD KNF Style")
(c-add-style "bsd-knf" bsd-knf-style nil)

(provide 'c-style)
;;; c-style.el ends here
