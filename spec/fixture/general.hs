-- SYNTAX TEST "source.haskell"

{-# LANGUAGE DataKinds, KindSignatures #-}

module Test where

import GHC.Types

---------------------------------- Strings -------------------------------------

str1, str2 :: String

-- it tokenizes single-line strings
str1 = "abcde\n\EOT\\EOL"

-- Regression test for #96
str2 = "^\\ "
--     ^^^^^^ string.quoted.double
--          ^ punctuation.definition.string.end
--       ^^ constant.character.escape
--     ^ string.quoted.double punctuation.definition.string.begin

-- it supports type-level string literals
str3 :: Fake "type-level string"
--           ^^^^^^^^^^^^^^^^^^^ meta.function.type-declaration meta.type-signature string.quoted.double
str3 = undefined
type Fake (a :: Symbol) = String

--------------------------- infix function call --------------------------------
infixCall :: t
infixCall = a `func` b
--            ^^^^^^ keyword.operator.function.infix
  where (a, b, func) = undefined

--------------------------- data declarations ---------------------------

data Foo = Foo Int
--             ^^^ meta.declaration.type.data meta.type-signature entity.name.type
--         ^^^ meta.declaration.type.data entity.name.tag
--   ^^^ meta.declaration.type.data meta.type-signature entity.name.type

main :: IO ()
main = undefined
-- >> =source.haskell
