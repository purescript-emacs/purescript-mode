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

(ert-deftest docs-bar-comment-different-spacings ()
  (purescript-test-ranges
   "--|  Docs comment 0 space
-- | Docs comment 1 space
--    | Docs comment many spaces
"
   '((1 85 font-lock-doc-face))))

(ert-deftest docs-bar-comment-continuation ()
  "Acc. to
https://github.com/purescript/documentation/blob/master/language/Syntax.md
PureScript explicitly doesn't support Haskell-style docs continuation
where vertical bar is omitted"
  (purescript-test-ranges
   "-- | Docs start
-- continue
"
   '((1 16 font-lock-doc-face)
     (17 19 font-lock-comment-delimiter-face)
     (20 28 font-lock-comment-face))))

(ert-deftest docs-cap-comment-different-spacings ()
  (purescript-test-ranges
   "-- ^ Docs comment space
--    ^ Docs comment many spaces
"
   '((1 57 font-lock-doc-face))))

;; For some unknown reason this fails on older Emacses
(when (>= emacs-major-version 28)
  (ert-deftest multiline-comment ()
    (purescript-test-ranges
   "{-
multiline comment
-- | not a doc
--| not a doc
still comment
-}
noncomment
{--}
noncomment
"
     '((1 64 font-lock-comment-face)
       (65 66 font-lock-comment-delimiter-face)
       (67 78 nil)
       (79 80 font-lock-comment-face)
       (81 82 font-lock-comment-delimiter-face)
       (83 93 nil)))))

(ert-deftest multiline-comment-w-delimiter-inside ()
  :expected-result :failed
  (purescript-test-ranges
   "{- {-{- -} noncomment"
   '((1 6 font-lock-comment-face)
     (7 10 font-lock-comment-delimiter-face)
     (11 21 nil))))

(ert-deftest type-with-typenames-and--> ()
  (purescript-test-ranges
   "type Component props = Effect (props -> JSX)"
   '((1 4 font-lock-keyword-face)
     (5 5 nil)
     (6 14 font-lock-type-face)
     (15 21 nil)
     (22 22 font-lock-variable-name-face)
     (23 23 nil)
     (24 29 font-lock-type-face)
     (30 37 nil)
     (38 39 font-lock-variable-name-face)
     (40 40 nil)
     (41 43 font-lock-type-face)
     (44 45 nil))))

(ert-deftest module-in-different-locations ()
  (purescript-test-ranges
   "module React.Basic.Hooks ( Component, module React.Basic
                         , module Data.Tuple.Nested ) where
"
   '((1 6 font-lock-keyword-face)
     (7 7 nil)
     (8 24 font-lock-type-face)
     (25 27 nil)
     (28 36 font-lock-type-face)
     (37 38 nil)
     (39 44 font-lock-keyword-face)
     (45 45 nil)
     (46 56 font-lock-type-face)
     (57 84 nil)
     (85 90 font-lock-keyword-face)
     (91 91 nil)
     (92 108 font-lock-type-face)
     (109 111 nil)
     (112 116 font-lock-keyword-face)
     (117 117 nil))))

(ert-deftest func-decl-w-do-and-qualified-do ()
  (purescript-test-ranges
   "mkMyComponent :: Component {}
mkMyComponent = do
  modalComp :: (NodeRef -> JSX) <- mkModal
  component \"mkMyComponent\" \\_ -> React.do
    dialogRef :: NodeRef <- newNodeRef
  pure $ R.label_ []
"
   '((1 13 font-lock-function-name-face)
     (14 14 nil)
     (15 16 font-lock-variable-name-face)
     (17 17 nil)
     (18 26 font-lock-type-face)
     (27 30 nil)
     (31 43 font-lock-function-name-face)
     (44 44 nil)
     (45 45 font-lock-variable-name-face)
     (46 46 nil)
     (47 48 font-lock-keyword-face)
     (49 61 nil)
     (62 63 font-lock-variable-name-face)
     (64 65 nil)
     (66 72 font-lock-type-face)
     (73 73 nil)
     (74 75 font-lock-variable-name-face)
     (76 76 nil)
     (77 79 font-lock-type-face)
     (80 81 nil)
     (82 83 font-lock-variable-name-face)
     (84 104 nil)
     (105 119 font-lock-string-face)
     (120 120 nil)
     (121 121 font-lock-variable-name-face)
     (122 122 font-lock-keyword-face)
     (123 123 nil)
     (124 125 font-lock-variable-name-face)
     (126 126 nil)
     (127 131 font-lock-type-face)
     (132 132 font-lock-variable-name-face)
     (133 134 font-lock-keyword-face)
     (135 149 nil)
     (150 151 font-lock-variable-name-face)
     (152 152 nil)
     (153 159 font-lock-type-face)
     (160 160 nil)
     (161 162 font-lock-variable-name-face)
     (163 181 nil)
     (182 182 font-lock-variable-name-face)
     (183 183 nil)
     (184 184 font-lock-type-face)
     (185 185 font-lock-variable-name-face)
     (186 192 nil)
     (193 194 font-lock-type-face)
     (195 195 nil))))
