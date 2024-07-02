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
;;
;;; Code:


(provide 'technicolor)
;;; technicolor.el ends here
