;;; purescript-font-lock-tests.el --- Unit tests for purescript font-lock -*- lexical-binding: t -*-

;; Copyright (c) 2025 Konstantin Kharlamov. All rights reserved.

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

;;; Code:

(require 'ert)
(require 'purescript-mode)

(defun purescript-test-ranges (text ranges-list)
  (with-temp-buffer
    (insert text)
    (purescript-mode)
    (font-lock-ensure)
    (let ((ret
           (catch 'fail
             (dolist (range ranges-list)
               (let ((begin (nth 0 range))
                     (end (nth 1 range))
                     (face-expected (nth 2 range)))
                 (dolist (pos (number-sequence begin end))
                   (let ((face-found (get-char-property pos 'face)))
                     (when (not (eq face-found face-expected))
                       (throw 'fail `(,begin ,end ,face-expected ,face-found ,pos)))))))
             nil)))
      (when ret
        (message "Range [%d:%d] has face %s (expected %s) at %d"
                 (nth 0 ret) (nth 1 ret) (nth 3 ret) (nth 2 ret) (nth 4 ret))
        (should-not ret)))))

(ert-deftest imports ()
  (purescript-test-ranges
   "import Data.Array (many)
import Data.Array as Array
import Data.Either (Either(..))
" '((1 6 font-lock-keyword-face)
    (8 17 font-lock-type-face)
    (26 31 font-lock-keyword-face)
    (33 42 font-lock-type-face)
    (44 45 font-lock-keyword-face)
    (47 51 font-lock-type-face)
    (53 58 font-lock-keyword-face)
    (60 70 font-lock-type-face)
    (73 78 font-lock-type-face)
    (80 81 font-lock-variable-name-face))))

(ert-deftest string ()
  (purescript-test-ranges
   "foo = \"hello\""
   '((1 3 font-lock-function-name-face)
     (5 5 font-lock-variable-name-face)
     (7 13 font-lock-string-face))))

(ert-deftest multiline-string ()
  (purescript-test-ranges
   "foo = \"\"\"
hello
\"\"\"
"
   '((1 3 font-lock-function-name-face)
     (5 5 font-lock-variable-name-face)
     (7 19 font-lock-string-face))))

(ert-deftest multiline-string-with-hash ()
  :expected-result :failed
  (purescript-test-ranges
   "foo = \"\"\"
# a string with hashtag
  # another # one
-- not a comment --
-- | not a comment
{- not a comment -}
\"\"\"
"
   '((1 3 font-lock-function-name-face)
     (5 5 font-lock-variable-name-face)
     (7 114 font-lock-string-face))))

(ert-deftest multiline-string-with-embedded-strings ()
  :expected-result :failed
  (purescript-test-ranges
   "foo = \"\"\"
this = \"still a string\"
\"\"\"
"
   '((1 3 font-lock-function-name-face)
     (5 5 font-lock-variable-name-face)
     (7 37 font-lock-string-face))))
