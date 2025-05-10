;;; purescript-vars.el --- Variable definitions for PureScript Mode -*- lexical-binding: t -*-

;; Author: 1997-1998 Graeme E Moss <gem@cs.york.ac.uk>
;;         1997-1998 Tommy Thorn <thorn@irisa.fr>
;;         2003      Dave Love <fx@gnu.org>
;;         2025      Konstantin Kharlamov <Hi-Angel@yandex.ru>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defvar-local purescript-literate nil
  "If not nil, the current buffer contains a literate PureScript script.
Possible values are: `bird' and `tex', for Bird-style and LaTeX-style
literate scripts respectively.  Set by `purescript-mode' and
`literate-purescript-mode'.  For an ambiguous literate buffer -- i.e. does
not contain either \"\\begin{code}\" or \"\\end{code}\" on a line on
its own, nor does it contain \">\" at the start of a line -- the value
of `purescript-literate-default' is used.")
(put 'purescript-literate 'safe-local-variable 'symbolp)

(provide 'purescript-vars)
