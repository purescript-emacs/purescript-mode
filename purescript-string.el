;;; purescript-string.el --- string manipulation utilties -*- lexical-binding: t -*-
;;;###autoload
(defun purescript-is-prefix-of (x y)
  "Is x string a prefix of y string?"
  (string= x (substring y 0 (min (length y) (length x)))))

(defun purescript-string ())

(provide 'purescript-string)
