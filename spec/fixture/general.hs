-- SYNTAX TEST "source.haskell"

{-# LANGUAGE DataKinds, KindSignatures, CPP #-}

module Test (
    main,
--  ^^^^ meta.declaration.module meta.declaration.exports entity.name.function
#ifdef TEST
--     ^^^^ meta.preprocessor.c
-- <- meta.preprocessor.c
    module Test
--         ^^^^ meta.declaration.module meta.declaration.exports support.other.module
--  ^^^^^^ meta.declaration.module meta.declaration.exports keyword.other
#else
-- <- meta.preprocessor.c
    str1, str2,
--        ^^^^ meta.declaration.module meta.declaration.exports entity.name.function
--  ^^^^ meta.declaration.module meta.declaration.exports entity.name.function
    DataType(..)
--           ^^ meta.declaration.module meta.declaration.exports meta.other.constructor-list keyword.operator.wildcard
--          ^^^^ meta.declaration.module meta.declaration.exports meta.other.constructor-list
--  ^^^^^^^^ meta.declaration.module meta.declaration.exports entity.name.type
#endif
-- <- meta.preprocessor.c
) where

import GHC.Types

data DataType = TypeCtor Int

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
