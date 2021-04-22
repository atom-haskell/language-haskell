-- SYNTAX TEST "source.haskell"

{-# LANGUAGE TypeApplications #-}

module Test where

f = undefined @Char undefined
--            ^^^^^ other.type-application
--                  ^^^^^^^^^ identifier support.function.prelude.undefined
f = undefined @[] undefined
--            ^^^ other.type-application
--                ^^^^^^^^^ identifier support.function.prelude.undefined
f = undefined @'[] undefined
--            ^^^^ other.type-application
--                 ^^^^^^^^^ identifier support.function.prelude.undefined
f = undefined @'(Char, String) undefined
--            ^^^^^^^^^^^^^^^^ other.type-application
--                             ^^^^^^^^^ identifier support.function.prelude.undefined
f = undefined @'() undefined
--            ^^^^ other.type-application
--                 ^^^^^^^^^ identifier support.function.prelude.undefined
f = undefined @"asd" undefined
--            ^^^^^^ other.type-application
--                   ^^^^^^^^^ identifier support.function.prelude.undefined
f = undefined @'a' undefined
--           ^^^^^ other.type-application
--                 ^^^^^^^^^ identifier support.function.prelude.undefined
f = undefined @1 undefined
--           ^^^ other.type-application
--               ^^^^^^^^^ identifier support.function.prelude.undefined
f = undefined @(forall a. Show a => a -> String) undefined
--           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ other.type-application
--                                               ^^^^^^^^^ identifier support.function.prelude.undefined

f x@(x:xs) = _
-- ^^^^^^^ !other.type-application
f x@'a' = _
--  ^^^ !other.type-application

-- >> =source.haskell
