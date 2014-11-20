;;; relative-linum.el --- Relative Line Numbers *** how I like it ***

;; After seeing linum-relative.el and some others, I realized that none of them
;; provided linum's like I wanted, so I took inspiration and code from
;; linum-relative.el and made my own

;;; Commentary:

;; relative-linum displays alongside absolute line numbers, the relative
;; distance from the current line. Also fading the absolute line numbers
;; away as they get further from the current line.

;;; To Use:

;; Add to your load-path then in your init.el
;;
;;     (require 'relative-linum)

;; To toggle
;;
;;      M-x relative-linum-toggle

;;; Configuration:
;; Defaults are shown in example

;; Minimum luminance for the absolute line number fading
;;
;;     (setq linum-min-luminance 0.25)

;; Luminance step per line (bigger == faster)
;;
;;     (setq linum-luminance-step 0.015)

;; Interval to display relative line numbers
;;
;;     (setq linum-relative-display-step 5)

;;; Code:
(require 'linum)
(require 'color)

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
(defvar linum-min-luminance 0.25 "Minimum luminance for faded numbers.")
(defvar linum-luminance-step 0.015 "Luminance decrements by this amount per line.")
(defvar linum-relative-display-step 5 "Distance between relative numbers.")

(defadvice linum-update (before relative-linum-update activate)
  "Get the last position of linum."
  (setq linum-last (line-number-at-pos)))

(defun relative-linum-color (distance)
  "Calculate the color for the linum face based on its DISTANCE from the current line."
  (let* ((delta (* distance linum-luminance-step))
         (start-color (face-attribute 'relative-linum-face :foreground))
         (rgb (color-name-to-rgb start-color))
         (hsl (apply 'color-rgb-to-hsl rgb))
         (luminance (max linum-min-luminance (- (nth 2 hsl) delta))))
    (progn
      (setcar (cddr hsl) luminance)
      (apply 'color-rgb-to-hex (apply 'color-hsl-to-rgb hsl)))))

(defun relative-linum (line-number)
  "Does the actual relative position calculations using LINE-NUMBER, and set the line display appropiately."
  (let* ((diff (abs (- line-number linum-last)))
         (current-p (= diff 0))
         (current-symbol (if (not current-p)
                             (if (= (mod diff linum-relative-display-step) 0)
                              (number-to-string diff)
                              "")
                           "->"))
         (abs-color (if current-p
                        (face-attribute 'relative-linum-current-face :foreground)
                      (relative-linum-color diff)))
         (face (if current-p 'relative-linum-current-face 'relative-linum-face))
         (abs-str (propertize (format " %3d" line-number)
                              'face `(:foreground ,abs-color :background "unspecified-bg")))
         (rel-str (propertize (format " %2s" current-symbol) 'face face)))
    (concat abs-str rel-str)))
    ;(propertize (format " %3d %2s " line-number current-symbol) 'face face)))

(defun relative-linum-toggle ()
  "Toggle relative-linum and regular."
  (interactive)
  (if (eq linum-format 'dynamic)
      (setq linum-format 'relative-linum)
    (setq linum-format 'dynamic)))
(setq linum-format 'relative-linum)

(provide 'relative-linum)
;;; relative-linum.el ends here
