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
    (80 81 font-lock-builtin-face))))

(ert-deftest string ()
  (purescript-test-ranges
   "foo = \"hello\""
   '((1 3 font-lock-function-name-face)
     (5 5 font-lock-builtin-face)
     (7 13 font-lock-string-face))))

(ert-deftest multiline-string ()
  (purescript-test-ranges
   "foo = \"\"\"
hello
\"\"\"
"
   '((1 3 font-lock-function-name-face)
     (5 5 font-lock-builtin-face)
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
     (5 5 font-lock-builtin-face)
     (7 114 font-lock-string-face))))

(ert-deftest multiline-string-with-embedded-strings ()
  :expected-result :failed
  (purescript-test-ranges
   "foo = \"\"\"
this = \"still a string\"
\"\"\"
"
   '((1 3 font-lock-function-name-face)
     (5 5 font-lock-builtin-face)
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
     (22 22 font-lock-builtin-face)
     (23 23 nil)
     (24 29 font-lock-type-face)
     (30 37 nil)
     (38 39 font-lock-builtin-face)
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
     (15 16 font-lock-builtin-face)
     (17 17 nil)
     (18 26 font-lock-type-face)
     (27 30 nil)
     (31 43 font-lock-function-name-face)
     (44 44 nil)
     (45 45 font-lock-builtin-face)
     (46 46 nil)
     (47 48 font-lock-keyword-face)
     (49 61 nil)
     (62 63 font-lock-builtin-face)
     (64 65 nil)
     (66 72 font-lock-type-face)
     (73 73 nil)
     (74 75 font-lock-builtin-face)
     (76 76 nil)
     (77 79 font-lock-type-face)
     (80 81 nil)
     (82 83 font-lock-builtin-face)
     (84 104 nil)
     (105 119 font-lock-string-face)
     (120 120 nil)
     (121 121 font-lock-builtin-face)
     (122 122 font-lock-keyword-face)
     (123 123 nil)
     (124 125 font-lock-builtin-face)
     (126 126 nil)
     (127 131 font-lock-type-face)
     (132 132 font-lock-builtin-face)
     (133 134 font-lock-keyword-face)
     (135 149 nil)
     (150 151 font-lock-builtin-face)
     (152 152 nil)
     (153 159 font-lock-type-face)
     (160 160 nil)
     (161 162 font-lock-builtin-face)
     (163 181 nil)
     (182 182 font-lock-builtin-face)
     (183 183 nil)
     (184 184 font-lock-type-face)
     (185 185 font-lock-builtin-face)
     (186 192 nil)
     (193 194 font-lock-type-face)
     (195 195 nil))))

(ert-deftest instance-miscellaneous ()
  "A diverse code snippet using `instance' (from Data.List module)"
  (purescript-test-ranges
   "instance extendNonEmptyList :: Extend NonEmptyList where
  extend f w@(NonEmptyList (_ :| as)) =
    NonEmptyList (f w :| (foldr go { val: Nil, acc: Nil } as).val)
    where
    go a { val, acc } = { val: f (NonEmptyList (a :| acc)) : val, acc: a : acc }
instance semigroupNonEmptyList :: Semigroup (NonEmptyList a) where
  append (NonEmptyList (a :| as)) as' =
    NonEmptyList (a :| as <> toList as')
derive newtype instance foldableNonEmptyList :: Foldable NonEmptyList
"
   '((1 8 font-lock-keyword-face) (9 28 nil)
     (29 30 font-lock-builtin-face) (31 31 nil)
     (32 37 font-lock-type-face) (38 38 nil)
     (39 50 font-lock-type-face) (51 51 nil)
     (52 56 font-lock-keyword-face) (57 69 nil)
     (70 70 font-lock-builtin-face) (71 71 nil)
     (72 83 font-lock-type-face) (84 85 nil)
     (86 86 font-lock-keyword-face) (87 87 nil)
     (88 89 font-lock-builtin-face) (90 95 nil)
     (96 96 font-lock-builtin-face) (97 101 nil)
     (102 113 font-lock-type-face) (114 119 nil)
     (120 121 font-lock-builtin-face) (122 137 nil)
     (138 138 font-lock-builtin-face) (139 139 nil)
     (140 142 font-lock-type-face) (143 147 nil)
     (148 148 font-lock-builtin-face) (149 149 nil)
     (150 152 font-lock-type-face) (153 158 nil)
     (159 159 font-lock-builtin-face) (160 168 nil)
     (169 173 font-lock-keyword-face) (174 196 nil)
     (197 197 font-lock-builtin-face) (198 203 nil)
     (204 204 font-lock-builtin-face) (205 208 nil)
     (209 220 font-lock-type-face) (221 224 nil)
     (225 226 font-lock-builtin-face) (227 233 nil)
     (234 234 font-lock-builtin-face) (235 243 nil)
     (244 244 font-lock-builtin-face) (245 247 nil)
     (248 248 font-lock-builtin-face) (249 255 nil)
     (256 263 font-lock-keyword-face) (264 286 nil)
     (287 288 font-lock-builtin-face) (289 289 nil)
     (290 298 font-lock-type-face) (299 300 nil)
     (301 312 font-lock-type-face) (313 316 nil)
     (317 321 font-lock-keyword-face) (322 332 nil)
     (333 344 font-lock-type-face) (345 348 nil)
     (349 350 font-lock-builtin-face) (351 360 nil)
     (361 361 font-lock-builtin-face) (362 366 nil)
     (367 378 font-lock-type-face) (379 382 nil)
     (383 384 font-lock-builtin-face) (385 388 nil)
     (389 390 font-lock-builtin-face) (391 403 nil)
     (404 409 font-lock-keyword-face) (410 448 nil)
     (449 450 font-lock-builtin-face) (451 451 nil)
     (452 459 font-lock-type-face) (460 460 nil)
     (461 472 font-lock-type-face) (473 473 nil))))

(ert-deftest foreign-imports ()
  (purescript-test-ranges
   "foreign import func2 :: Effect Int
foreign import func3
  :: Effect Int
foreign import
  func4 :: Effect Int
  foreign import func5 -- invalid indentation, but allowed in other context
invalid_dont_highlight foreign import func6
foreign importinvalid
"
   '((1 7 font-lock-keyword-face)
     (8 8 nil)
     (9 14 font-lock-keyword-face)
     (15 21 nil)
     (22 23 font-lock-builtin-face)
     (24 24 nil)
     (25 30 font-lock-type-face)
     (31 31 nil)
     (32 34 font-lock-type-face)
     (35 35 nil)
     (36 42 font-lock-keyword-face)
     (43 43 nil)
     (44 49 font-lock-keyword-face)
     (50 58 nil)
     (59 60 font-lock-builtin-face)
     (61 61 nil)
     (62 67 font-lock-type-face)
     (68 68 nil)
     (69 71 font-lock-type-face)
     (72 72 nil)
     (73 79 font-lock-keyword-face)
     (80 80 nil)
     (81 86 font-lock-keyword-face)
     (87 95 nil)
     (96 97 font-lock-builtin-face)
     (98 98 nil)
     (99 104 font-lock-type-face)
     (105 105 nil)
     (106 108 font-lock-type-face)
     (109 111 nil)
     (112 118 font-lock-keyword-face)
     (119 119 nil)
     (120 125 font-lock-keyword-face)
     (126 132 nil)
     (133 135 font-lock-comment-delimiter-face)
     (136 185 font-lock-comment-face)
     (186 207 font-lock-function-name-face)
     (208 229 nil)
     (230 236 font-lock-function-name-face)
     (237 251 nil))))

(ert-deftest operator-faces-simple ()
  "Tests operator faces unrelated to backticks"
  (purescript-test-ranges
   "-- ..
import Data.Maybe (Maybe(..))

-- -> and ::
id :: a -> a
id = \\x -> x

-- <-
main = do
  n <- pure 42
  pure n

-- => and =
showVal :: Show a => a -> String
showVal = show

-- @
firstElem j@(Just _) = j

-- guard |
abs' x | x < 0 = -x
       | otherwise = x

-- ~ and unicode ∷
toUnfoldable ∷ forall f. Unfoldable f => List ~> f
toUnfoldable = undefined

-- : as an operator
arr :: Array Int
arr = 1 : [2,3]
"
   '((1 3 font-lock-comment-delimiter-face)
     (4 6 font-lock-comment-face)
     (7 12 font-lock-keyword-face)
     (13 13 nil)
     (14 23 font-lock-type-face)
     (24 25 nil)
     (26 30 font-lock-type-face)
     (31 31 nil)
     (32 33 font-lock-builtin-face)
     (34 37 nil)
     (38 40 font-lock-comment-delimiter-face)
     (41 50 font-lock-comment-face)
     (51 52 font-lock-function-name-face)
     (53 53 nil)
     (54 55 font-lock-builtin-face)
     (56 58 nil)
     (59 60 font-lock-builtin-face)
     (61 63 nil)
     (64 65 font-lock-function-name-face)
     (66 66 nil)
     (67 67 font-lock-builtin-face)
     (68 68 nil)
     (69 69 font-lock-builtin-face)
     (70 71 nil)
     (72 73 font-lock-builtin-face)
     (74 77 nil)
     (78 80 font-lock-comment-delimiter-face)
     (81 83 font-lock-comment-face)
     (84 87 font-lock-function-name-face)
     (88 88 nil)
     (89 89 font-lock-builtin-face)
     (90 90 nil)
     (91 92 font-lock-keyword-face)
     (93 97 nil)
     (98 99 font-lock-builtin-face)
     (100 118 nil)
     (119 121 font-lock-comment-delimiter-face)
     (122 130 font-lock-comment-face)
     (131 137 font-lock-function-name-face)
     (138 138 nil)
     (139 140 font-lock-builtin-face)
     (141 141 nil)
     (142 145 font-lock-type-face)
     (146 148 nil)
     (149 150 font-lock-builtin-face)
     (151 153 nil)
     (154 155 font-lock-builtin-face)
     (156 156 nil)
     (157 162 font-lock-type-face)
     (163 163 nil)
     (164 170 font-lock-function-name-face)
     (171 171 nil)
     (172 172 font-lock-builtin-face)
     (173 179 nil)
     (180 182 font-lock-comment-delimiter-face)
     (183 184 font-lock-comment-face)
     (185 193 font-lock-function-name-face)
     (194 195 nil)
     (196 196 font-lock-builtin-face)
     (197 197 nil)
     (198 201 font-lock-type-face)
     (202 202 nil)
     (203 203 font-lock-keyword-face)
     (204 205 nil)
     (206 206 font-lock-builtin-face)
     (207 210 nil)
     (211 213 font-lock-comment-delimiter-face)
     (214 221 font-lock-comment-face)
     (222 225 font-lock-function-name-face)
     (226 228 nil)
     (229 229 font-lock-builtin-face)
     (230 232 nil)
     (233 233 font-lock-builtin-face)
     (234 236 nil)
     (237 237 font-lock-builtin-face)
     (238 238 nil)
     (239 239 font-lock-builtin-face)
     (240 248 nil)
     (249 249 font-lock-builtin-face)
     (250 260 nil)
     (261 261 font-lock-builtin-face)
     (262 265 nil)
     (266 268 font-lock-comment-delimiter-face)
     (269 284 font-lock-comment-face)
     (285 296 font-lock-function-name-face)
     (297 297 nil)
     (298 298 font-lock-builtin-face)
     (299 307 nil)
     (308 308 font-lock-builtin-face)
     (309 309 nil)
     (310 319 font-lock-type-face)
     (320 322 nil)
     (323 324 font-lock-builtin-face)
     (325 325 nil)
     (326 329 font-lock-type-face)
     (330 330 nil)
     (331 332 font-lock-builtin-face)
     (333 335 nil)
     (336 347 font-lock-function-name-face)
     (348 348 nil)
     (349 349 font-lock-builtin-face)
     (350 361 nil)
     (362 364 font-lock-comment-delimiter-face)
     (365 381 font-lock-comment-face)
     (382 384 font-lock-function-name-face)
     (385 385 nil)
     (386 387 font-lock-builtin-face)
     (388 388 nil)
     (389 393 font-lock-type-face)
     (394 394 nil)
     (395 397 font-lock-type-face)
     (398 398 nil)
     (399 401 font-lock-function-name-face)
     (402 402 nil)
     (403 403 font-lock-builtin-face)
     (404 406 nil)
     (407 407 font-lock-builtin-face)
     (408 414 nil))))

(ert-deftest tuple-and-cap-highlighted ()
  (purescript-test-ranges
   "myTuple = 1 /\\ (2 ^ 3)"
   '((1 7 font-lock-function-name-face)
     (8 8 nil)
     (9 9 font-lock-builtin-face)
     (10 12 nil)
     (13 14 font-lock-builtin-face)
     (15 18 nil)
     (19 19 font-lock-builtin-face)
     (20 23 nil))))

(ert-deftest backtick-operator ()
  (purescript-test-ranges
   "var = 1 `func` 2"
   '((1 3 font-lock-function-name-face)
     (4 4 nil)
     (5 5 font-lock-builtin-face)
     (6 8 nil)
     (9 14 font-lock-builtin-face)
     (15 17 nil))))
