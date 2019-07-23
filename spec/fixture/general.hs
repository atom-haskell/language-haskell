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
    str1, Test.str2,
--             ^^^^ meta.declaration.module meta.declaration.exports entity.name.function
--        ^^^^^ support.other.module
--  ^^^^ meta.declaration.module meta.declaration.exports entity.name.function
    DataType(
--  ^^^^^^^^ meta.declaration.module meta.declaration.exports entity.name.type
#ifdef EXPORT_CTOR
-- <- meta.preprocessor.c
      TypeCtor
--    ^^^^^^^^ meta.declaration.module meta.declaration.exports meta.other.constructor-list entity.name.tag
#else
      ..
--    ^^ meta.declaration.module meta.declaration.exports meta.other.constructor-list keyword.operator.wildcard
#endif
-- <- meta.preprocessor.c
    ),
    Test.DataType
--  ^^^^^^^^^^^^^ entity.name.type
--  ^^^^^ support.other.module
#endif
-- <- meta.preprocessor.c
) where

import GHC.Types

data DataType = TypeCtor Int
--                       ^^^ meta.declaration.type.data meta.type-signature entity.name.type support.class.prelude.Int
--              ^^^^^^^^ meta.declaration.type.data entity.name.tag
--   ^^^^^^^^ meta.declaration.type.data meta.type-signature entity.name.type
-- <- meta.declaration.type.data keyword.other.data

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

-- no unnecessary termination in data declarations
data Something ~= Something
--                ^^^^^^^^^ meta.declaration.type.data meta.type-signature entity.name.type
data Something ~--= Something
--                  ^^^^^^^^^ meta.declaration.type.data meta.type-signature entity.name.type
-- proper termination in data declarations
data Something --- Something = Something
--             ^^^^^^^^^^^^^^^^^^^^^^^^^ comment.line.double-dash
data Something = Something
--             ^  keyword.operator.assignment
--               ^^^^^^^^^ entity.name.tag
--   ^^^^^^^^^ entity.name.type
data Something -- = Something
--             ^^^^^^^^^^^^^^ comment.line.double-dash
--   ^^^^^^^^^ entity.name.type
data Something a {- =b -} aa = Other
--               ^^^^^^^^ comment.block
--                       ^^ meta.type-signature
--   ^^^^^^^^^ meta.type-signature entity.name.type
data Something {- = Something -} = Other
--             ^^^^^^^^^^^^^^^^^ comment.block

func2 :: IO () {- comment = also comment -}
--             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ comment.block
func2 :: IO () !{- comment = also comment -}
--              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ comment.block
func :: IO () -- comment = also comment
--            ^^^^^^^^^^^^^^^^^^^^^^^^^ comment.line.double-dash
func :: IO () !-- comment = also comment
--             ^^^^^^^^^^^^^^^^^^^^^^^^^ !comment.line.double-dash
let x :: Int -- comment <- an int
--           ^^^^^^^^^^^^^^^^^^^^ comment.line.double-dash
let x :: Int {- comment <- an int -}
--           ^^^^^^^^^^^^^^^^^^^^^^^ comment.block
let x :: Int !-- comment <- an int
--            ^^^^^^^^^^^^^^^^^^^^ !comment.line.double-dash
let x :: Int !{- comment <- an int -}
--            ^^^^^^^^^^^^^^^^^^^^^^^ comment.block

data PhExpr id
     = PhVar id
     | OpApp (LPhExpr id)
             (LPhExpr id)
             (LPhExpr id)
     | NegApp (LPhExpr id)

-- end

-- >> =source.haskell
