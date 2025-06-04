;;; purescript-indentation-tests.el --- Unit tests for purescript indentation -*- lexical-binding: t -*-

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
(require 'purescript-indentation)

(defun purescript-test-indentation-expected-only (expected)
  (with-temp-buffer
    (insert expected)
    (purescript-mode)
    (turn-on-purescript-indentation)
    (indent-region (point-min) (point-max))
    (should (string= expected (buffer-string)))))

(defun purescript-test-indentation (before after &optional start-line)
  (with-temp-buffer
    (insert before)
    (purescript-mode)
    (turn-on-purescript-indentation)
    (indent-region (if start-line start-line (point-min))
                   (point-max))
    (should (string= after (buffer-string)))))

(ert-deftest newtype-comma-first ()
  (purescript-test-indentation "
newtype Foo = Foo { field1 :: MyType
, field2 :: Int
, field3 :: HashMap ParType Foo }"

"
newtype Foo = Foo { field1 :: MyType
                  , field2 :: Int
                  , field3 :: HashMap ParType Foo }"))

(ert-deftest newtype-comma-end ()
  (purescript-test-indentation "
newtype Foo = Foo { field1 :: MyType,
field2 :: Int,
field3 :: HashMap ParType Foo }"

"
newtype Foo = Foo { field1 :: MyType,
                    field2 :: Int,
                    field3 :: HashMap ParType Foo }"))

(ert-deftest data-bar-first ()
  (purescript-test-indentation "
data Foo = Foo1 Bar
| Foo2 Bar2
| Foo3 Unit"

"
data Foo = Foo1 Bar
         | Foo2 Bar2
         | Foo3 Unit"))

(ert-deftest data-bar-end ()
  (purescript-test-indentation "
data Foo = Foo1 Bar |
Foo2 Bar2 |
Foo3 Unit"

"
data Foo = Foo1 Bar |
           Foo2 Bar2 |
           Foo3 Unit"))

(ert-deftest imports-zero-indented ()
  (purescript-test-indentation "
module MyModule where

import Prelude

    import Data.Array (many)
    import Data.Array as Array
    import Data.Either (Either(..))"

"
module MyModule where

import Prelude

import Data.Array (many)
import Data.Array as Array
import Data.Either (Either(..))"))

(ert-deftest imports-indented-forward ()
  "PureScript allows for imports to have indentation, but the
 indentation must be the same. In this test we skip first indented
 import, and test that further lines inherit indentation level."
  :expected-result :failed
  (purescript-test-indentation "
module MyModule where

    import Prelude

import Data.Array (many)
import Data.Array as Array
import Data.Either (Either(..))"

"
module MyModule where

    import Prelude

    import Data.Array (many)
    import Data.Array as Array
    import Data.Either (Either(..))"
    6))

(ert-deftest imports-qualified-newline-comma-first ()
  :expected-result :failed
  (purescript-test-indentation "
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..)
, fromMaybe)"

"
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe ( Maybe(..)
                  , fromMaybe)"))

(ert-deftest imports-qualified-newline-comma-end ()
  :expected-result :failed
  (purescript-test-indentation "
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..),
fromMaybe)"

"
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..),
                   fromMaybe)"))

(ert-deftest do-impl ()
  (purescript-test-indentation "
main = do
pure unit"

"
main = do
  pure unit"))

(ert-deftest let-bindings ()
  :expected-result :failed
  (purescript-test-indentation "
main = do
  let foo = 1
        bar = 2
  let
            buzz = 1
            fuzz = 2
pure unit"

"
main = do
  let foo = 1
      bar = 2
  let
    buzz = 1
    fuzz = 2
  pure unit"))

(ert-deftest module-exports-list ()
  :expected-result :failed
  (purescript-test-indentation "
module MyModule ( class A
, b, c
, d) where"

"
module MyModule ( class A
                , b, c
                , d) where"))

(ert-deftest module-exports-next-line ()
  "Parentheses should get indented to the mode indentation size"
  :expected-result :failed
  (purescript-test-indentation "
module MyModule
            (class A, b, c, d) where"

"
module MyModule
  (class A, b, c, d) where"))

(ert-deftest keyword-record-values ()
  "PureScript allows keywords to be part of a Record declaration"
  :expected-result :failed
  (purescript-test-indentation "
type MyRec = { data :: Number
, where :: Number
, instance :: Number
}"

"
type MyRec = { data :: Number
             , where :: Number
             , instance :: Number
             }"))

(ert-deftest func-with-do ()
  :expected-result :failed
  (purescript-test-indentation-expected-only "
foo :: Foo
foo = do
  pure unit
"))

(ert-deftest do-bindings ()
  :expected-result :failed
  (purescript-test-indentation "
foo :: Foo
foo = do
_ <- something
identifier :: Array String <- function call
_ <- another call
pure unit"

"
foo :: Foo
foo = do
  _ <- something
  identifier :: Array String <- function call
  _ <- another call
  pure unit"))

(ert-deftest let-in-separate-lines ()
  "Tests bug #12"
  (purescript-test-indentation "
test1 a
= let { x } = a
in x"

"
test1 a
  = let { x } = a
    in x"))

(ert-deftest case-of-separate-lines ()
  "Tests bug #12"
  (purescript-test-indentation "
test3 a
= case a of
{ x: y }
-> y"

 "
test3 a
  = case a of
    { x: y }
      -> y"))

(ert-deftest comma-first-list-after-case-of ()
  "A comma-first list was getting misindented if goes after case-of"
  :expected-result :failed
  (purescript-test-indentation-expected-only "
fun = case _ of
  [ a
  , b ]
"))
