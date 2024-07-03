;;; technicolor.el --- Summary: programmatic color palette access -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Aatmun Baxi
;;
;; Author: Aatmun Baxi
;; Maintainer: Aatmun Baxi
;; Created: July 02, 2024
;; Modified: July 02, 2024
;; Version: 0.0.1
;; Keywords:  convenience
;; Homepage: https://github.com/aatmunbaxi/technicolor
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Many custom Emacs themes provide their own color palettes as a
;; variable or collection of variables. Unfortunately for users who enjoy using
;; theme-specific colors in various parts of Emacs, there is no
;; universal way to access these colors programmatically. Some theme
;; collections define their own functions to access colors via elisp,
;; but these solutions are not homogeneous across theme collections.
;;
;; technicolor attempts to partially solve this problem by allowing
;; the user to define a "universal" palette which contains colors
;; that all the themes they would like to use in elisp implement.
;; Then the user may use technicolor to access the colors in the universal
;; palette, which would then access the correct theme-dependent color
;; that the user specifies.
;;
;; In addition, it provides an abstraction above the `color' library
;; for basic color manipulation, allowing for more ergonomic retrieval
;; and usage of theme-specific colors.

;;; Code:

;;; Requirements:

(require 'cl-lib)
(require 'color)

;;; Vars
(defgroup technicolor nil
  "Almost univeral color palette access."
  :group 'technicolor)

(defcustom technicolor-themes nil
  "List of themes, their accessors, and palette mappings.

Each entry in this list is of the form (regex accessor mappings)
where:

1. regex: Regex matching a theme or class of themes
2. accessor: Unqouted symbol of function that will access colors.
An accessor should take in the symbol
representing a color and return the string representation of that
color or the symbol `unspecified' (or some fallback color) if the
color symbol is not found in the theme palette.
3. mappings: An alist of colors that need be mapped from
`technicolor-colors' to the names of the colors you want in the
theme.

If all your themes implement a certain color, then you
can omit them from this list for each entry.

For example in a theme collection `foo', with accessor `foo-themes-get-color',
 if you want the color `red' in `technicolor-colors' to map to
`foo-theme-bright-red', the entry for `foo' themes might be

`'(\"^foo-.*\" foo-themes-get-color ((red . foo-theme-bright-red)))'"
  :type '(list (list symbol))
  :group 'technicolor)

;;;###autoload
(defvar technicolor-doom-themes-data
  '("^doom-.*" doom-color ((foreground . fg)
                           (background . bg)))
  "Default configuration for `doom-themes'.")

;;;###autoload
(defvar technicolor-modus-themes-data
  '("^modus-.*" modus-themes-get-color-value ((foreground . fg-main)
                                              (background . bg-main)))
  "Default configuration for `modus-themes'.")

;;;###autoload
(defvar technicolor-ef-themes-data
  '("^ef-.*" ef-themes-get-color-value ((foreground . fg-main)
                                        (background . bg-main)))
  "Default configuration for `ef-themes'.")

;;;###autoload
(defvar technicolor-standard-themes-data
  '("^standard-.*" standard-themes-get-color-value ((foreground . fg-main)
                                                    (background . bg-main)))
  "Default configuration for `standard-themes'.")

;;;###autoload
(defvar technicolor-catppuccin-themes-data
  '("^catppuccin" technicolor--get-catppuccin-color ((foreground . text)
                                                     (background . base)))
  "Default configuration for `catppuccin-themes'.
Please note that these themes use some colorful names for all the other colors,
so heavy customization might be needed.")

(defcustom technicolor-colors nil
  "List of colors in universal palette that can be accessed.

These should be symbols representing colors that can all
be sensibly accessed in the themes matched in `technicolor-themes'."
  :type '(list symbol)
  :group 'technicolor)



;;; Property functions

(defun technicolor--valid-color-p (color)
  "Determine if COLOR is a valid element of a palette."
  (let ((col (technicolor-get-color color)))
    (not (or (null col) (equal col 'unspecified)))))

(defun technicolor--invalid-color-p (color)
  "Determine if COLOR is a valid element of a palette."
  (not (technicolor--valid-color-p color)))



;;; Private Functions

(defun technicolor--get-catppuccin-color (color)
  "Get catppuccin COLOR."
  (if (boundp 'catppuccin-flavor)
      (let ((ctp-theme-colors (intern
                               (concat "catppuccin-" (symbol-name catppuccin-flavor) "-colors"))))
        (alist-get color (eval ctp-theme-colors)))
    'unspecified))


(defun technicolor--get-theme-data (theme)
  "Get theme data for theme name THEME.

Does so by matches the regexes in the cars of elements in
`technicolor-themes'."
  (let ((theme-name (symbol-name theme))
        (data nil))
    (pcase-dolist  (`(,theme-rx ,theme-color-fun ,theme-mapping) technicolor-themes)
      (when (string-match theme-rx theme-name)
        (setq data `(,theme-rx ,theme-color-fun ,theme-mapping))))
    data))



(defun technicolor--color-to-hex (col)
  "Convert a color COL to 12 bit hexadecimal.

COL can be a hexadecimal string of arbitrary bit depth or list of r g b
color values."
  (cond ((and col (listp col))
         (pcase col
           (`(,r ,g ,b) (color-rgb-to-hex r g b 2))))
        ((string-prefix-p "#" col)
         (technicolor--color-to-hex (color-name-to-rgb col)))))


;;; Macros

(defmacro technicolor--with-technicolor-color (color form)
  "Execute FORM when COLOR is a valid (i.e. not `unspecified') color.
Return `unspecified' otherwise."
  (declare (indent 1))
  `(if (technicolor--valid-color-p ,color)
       ,form
     'unspecified))

(defmacro technicolor--with-technicolor-colors (colors form)
  "Execute FORM if all values of COLORS are valid colors.
Return `unspecified' otherwise."
  (declare (indent 1))
  `(if (cl-notany #'technicolor--invalid-color-p ,colors)
       ,form
     'unspecified))


;;; Public functions

;;;###autoload
(defun technicolor-get-color (color)
  "Get COLOR from current theme as specified by `technicolor-themes'.

  COLOR should appear in `technicolor-colors' or be universally
available in all themes known to technicolor. Return `unspecified'
if theme or color is not recognized. This is for safety in case
this function is used in a face definition, where it is safe to
use `unspecified'."
  (cond ((and (not (null color)) (symbolp color))
         (pcase-let* ((`(_  ,accessor ,color-mapping) (technicolor--get-theme-data (car custom-enabled-themes)))
                      (theme-color (if (assoc color color-mapping)
                                       (alist-get color color-mapping)
                                     color)))
           (if-let ((col (when accessor
                           (funcall accessor theme-color))))
               col
             'unspecified)))
        ((string-prefix-p "#" color) color)))



;;;###autoload
(defun technicolor-darken (color alpha)
  "Darken COLOR by ALPHA percent.

COLOR can be a symbol in `technicolor-colors', a hexadecimal string, or list
of either of the above."
  (cond ((listp color)
         (mapcar (lambda (col) (technicolor-darken col alpha)) color))
        ((and color (symbolp color))
         (technicolor--with-technicolor-color color
           (technicolor--color-to-hex
            (color-darken-name
             (technicolor-get-color color) alpha))))
        ((string-prefix-p "#" color) (technicolor--color-to-hex (color-darken-name color alpha)))))


;;;###autoload
(defun technicolor-lighten (color alpha)
  "Lighten COLOR by ALPHA percent.

COLOR can be a symbol in `technicolor-colors', a hexadecimal string, or list
of either of the above."
  (technicolor-darken color (- alpha)))

;;;###autoload
(defun technicolor-complement (color)
  "Return hexadecimal complement of COLOR."
  (technicolor--with-technicolor-color color
    (technicolor--color-to-hex (color-complement (technicolor-get-color color)))))

;;;###autoload
(defun technicolor-gradient (start stop step-nums)
  "Generate gradient from START color to STOP color.

Return list of colors in gradient of length STEP-NUMS."
  (technicolor--with-technicolor-colors  `(,start ,stop)
    (let ((-start (technicolor-get-color start))
          (-stop (technicolor-get-color stop)))
      (mapcar  #'technicolor--color-to-hex
               (color-gradient (color-name-to-rgb -start)
                               (color-name-to-rgb -stop) step-nums)))))

;;;###autoload
(defun technicolor-saturate (color alpha)
  "Saturate COLOR by ALPHA percent."
  (cond ((listp color)
         (mapcar (lambda (col) (technicolor-saturate col alpha)) color))
        ((and color (symbolp color))
         (technicolor--with-technicolor-color color
           (technicolor--color-to-hex
            (color-saturate-name
             (technicolor-get-color color) alpha))))
        ((string-prefix-p "#" color) (technicolor--color-to-hex (color-darken-name color alpha)))))


;;;###autoload
(defun technicolor-desaturate (color alpha)
  "Desturate COLOR by ALPHA percent."
  (cond ((listp color)
         (mapcar (lambda (col) (technicolor-saturate col alpha)) color))
        ((and color (symbolp color))
         (technicolor--with-technicolor-color color
           (technicolor--color-to-hex
            (color-desaturate-name
             (technicolor-get-color color) alpha))))
        ((string-prefix-p "#" color) (technicolor--color-to-hex (color-darken-name color alpha)))))


;; TODO requiring `cl-lib' for just one function is overkill
;; either lean into it or rework the function
;; stolen from `doom-themes'
;;;###autoload
(defun technicolor-blend (color1 color2 alpha)
  "Blend two colors COLOR1 and COLOR2 by percentage ALPHA."
  (technicolor--with-technicolor-colors `(,color1 ,color2)
    (cond ((and color1 color2 (symbolp color1) (symbolp color2))
           (technicolor-blend (technicolor-get-color  color1) (technicolor-get-color color2) alpha))
          ((or (listp color1) (listp color2))
           (cl-loop for x in color1
                    when (if (listp color2) (pop color2) color2)
                    collect (technicolor-blend x it alpha)))

          ((and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
           (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
                  (cl-loop for it    in (color-name-to-rgb color1)
                           for other in (color-name-to-rgb color2)
                           collect (+ (* (/ alpha 100.0) it) (* other (- 1  (/ alpha 100.0))))))))))



(provide 'technicolor)
;;; technicolor.el ends here
