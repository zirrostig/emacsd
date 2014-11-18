;;; relative-linum.el --- Relative Line Numbers *** how I like it ***

;; After seeing linum-relative.el and some others, I realized that none of them
;; provided linum's like I wanted, so I took inspiration and code from
;; linum-rela.el and made my own

;;; Commentary:

;;; Code:
(require 'linum)

(defface relative-linum-face
  `((t :foreground ,(face-attribute 'font-lock-comment-face :foreground)
       :background "unspecified-bg"
       :weight normal
       :slant normal
       ))
  "Face for displaying line numbers"
  :group 'relative-linum)

(defface relative-linum-current-face
  `((t :inherit relative-linum-face
       :foreground "goldenrod"
       :weight bold
       ))
  "Face for displaying the current line number"
  :group 'relative-linum)

(defvar linum-last 0 "Last line number.")

(defadvice linum-update (before relative-linum-update activate)
  "Get the last position of linum."
  (setq linum-last (line-number-at-pos)))

(defun relative-linum (line-number)
  "Does the actual relative position calculations using LINE-NUMBER, and set the line display appropiately."
  (let* ((diff (abs (- line-number linum-last)))
         (current-p (= diff 0))
         (current-symbol (if (not current-p)
                             (if (= (mod diff 5) 0)
                              (number-to-string diff)
                              "")
                           "->"))
         (face (if current-p 'relative-linum-current-face 'relative-linum-face)))
    (propertize (format "%3d %2s " line-number current-symbol) 'face face)))

(defun relative-linum-toggle ()
  "Toggle relative-linum and regular."
  (interactive)
  (if (eq linum-format 'dynamic)
      (setq linum-format 'relative-linum)
    (setq linum-format 'dynamic)))
(setq linum-format 'relative-linum)

(provide 'relative-linum)
;;; relative-linum.el ends here
