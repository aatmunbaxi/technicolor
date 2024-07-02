;;; technicolor.el (almost) universal programmatic color paletta access -*- lexical-binding: t; -*-
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
;; Many custom emacs themes provide their own color palettes as a
;; variable or collection of variables. Unfortunately for users who enjoy using
;; theme-specific colors in various parts of emacs, there is no
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
;;; Code:
(defgroup technicolor nil
  "Almost univeral color palette access."
  :group 'technicolor)

;; TODO make this one big variable like in fontaine?
(defcustom technicolor-themes nil
  "List of technicolor themes, their accessors, and palette mappings.

Each entry in this list should contain the following:

1. Regex matching a theme or class of themes
2. Unqouted symbol of function that will access colors
3. An alist of colors that need be mapped from `technicolor-colors'
to the names of the colors you want those to correspond to.

For example in a theme `foo', with accessor `foo-get-color', if
you want the color `red' in `technicolor-colors' to map to
`foo-bright-red', the entry for `foo' might be

`'(\"^foo-.*\" foo-get-color ((red . foo-bright-red)))'"
  :type '(list (list symbol))
  :group 'technicolor)

;;;###autoload
(defvar technicolor-doom-themes-data
  '("^doom-.*" doom-color ((foreground . fg)
                           (background . bg)))
  "Default configuration for `doom-themes'.")

;;;###autoload
(defvar technicolor-modus-themes-data
  '("^modus-.*" modus-themes-get-color-value '((foreground . fg-main)
                                               (background . bg-main)))
  "Default configuration for `modus-themes'.")

;;;###autoload
(defvar technicolor-ef-themes-data
  '("^ef-.*" ef-themes-get-color-value '((foreground . fg-main)
                                         (background . bg-main)))
  "Default configuration for `ef-themes'.")

;;;###autoload
(defvar technicolor-standard-themes-data
  '("^ef-.*" standard-themes-get-color-value '((foreground . fg-main)
                                               (background . bg-main)))
  "Default configuration for `standard-themes'.")

;;;###autoload
(defvar technicolor-catppuccin-themes-data
  '("^catppuccin*" technicolor--get-catppuccin-color '((foreground . text)
                                                       (background . base)))
  "Default configuration for `catppuccin-themes'.
Please note that these themes use some colorful names for all the other colors,
so heavy customization might be needed.")

(defcustom technicolor-colors nil
  "List of colors in universal palette that can be sensibly accessed
in all themes matched in `technicolor-themes-alist'."
  :type '(list symbol)
  :group 'technicolor)

(defun technicolor--get-catppuccin-color (color)
  (if (featurep 'catppuccin-theme)
      (let ((ctp-theme-colors (intern
                               (concat "catppuccin-" (symbol-name catppuccin-flavor) "-colors"))))
        (alist-get color (eval ctp-theme-colors))))
  (user-error (format "catppuccin-theme not installed")))

(defun technicolor--get-theme-data (theme)
  (let ((theme-name (symbol-name theme))
        (data nil))
    (pcase-dolist  (`(,theme-rx ,theme-color-fun ,theme-mapping) technicolor-themes)
      (when (string-match theme-rx theme-name)
        (setq data `(,theme-rx ,theme-color-fun ,theme-mapping))))
    (if data
        data (user-error "%s has no associated data in `technicolor-themes'" theme))))

;;;###autoload
(defun technicolor-get-color (color)
  "Get COLOR from current theme as specified by `technicolor-themes'.

  COLOR should appear in `technicolor-colors' or be universally
available in all themes known to technicolor."
  (pcase-let* ((`(_  ,accessor ,color-mapping) (technicolor--get-theme-data (car custom-enabled-themes)))
               (theme-color (if (alist-get color color-mapping)
                                (alist-get color color-mapping)
                              color)))
    (when (and (memq color technicolor-colors) accessor)
      (funcall accessor theme-color))))


(provide 'technicolor)
;;; technicolor.el ends here
