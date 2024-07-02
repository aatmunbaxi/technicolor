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
;; Package-Requires: ((emacs "24.3"))
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

(defcustom technicolor-themes-alist nil
  "alist of technicolor themes and their accessors.

The CAR of each element should be a regexp that will match
the name of a theme or group of themes, whose palette accessor is the CDR
of the element."
  :type '(alist :key-type regexp :value-type function)
  :group 'technicolor)

(defcustom technicolor-colors nil
  "List of colors in universal palette that can be sensibly accessed
in all themes matched in `technicolor-themes-alist'"
  :type '(list symbol)
  :group 'technicolor)

(defun technicolor--get-theme-accessor (theme)
  (let ((theme-name (symbol-name theme))
        (get-color-function nil))
    (pcase-dolist (`(,re . ,fun) technicolor-themes-alist)
      (when (string-match re theme-name)
        (message theme-name)
        (setq get-color-function fun)))
    get-color-function))




(defun technicolor-get-color (color &rest arguments)
  "Get COLOR from current theme.

COLOR should appear in `technicolor-colors'. When THEME
is `nil', get COLOR from currently enabled theme."
  (if (memq color technicolor-colors)
      (funcall (technicolor--get-theme-accessor (car custom-enabled-themes)) color arguments)
    (user-error  "Color %s not in `technicolor-colors'" color)))



(provide 'technicolor)
;;; technicolor.el ends here
