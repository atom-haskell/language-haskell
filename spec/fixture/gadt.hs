-- SYNTAX TEST "source.haskell"

{-# LANGUAGE GADTs #-}

module Test where

-- it understands GADT syntax

data Term a where
--          ^^^^^ meta.declaration.type.data keyword.other
--        ^  meta.declaration.type.data meta.type-signature variable.other.generic-type
--   ^^^^ meta.declaration.type.data meta.type-signature entity.name.type
-- <- meta.declaration.type.data keyword.other.data
    LitI :: Int -> Term Int
--                      ^^^ meta.type-signature entity.name.type support.class.prelude.Int
--                 ^^^^ meta.type-signature entity.name.type
--              ^^ meta.type-signature keyword.other.arrow
--          ^^^ meta.type-signature entity.name.type support.class.prelude.Int
--  ^^^^ entity.name.tag
    LitS :: String -> Term String
--                         ^^^^^^ meta.type-signature entity.name.type support.class.prelude.String
--                    ^^^^ meta.type-signature entity.name.type
--                 ^^ meta.type-signature keyword.other.arrow
--          ^^^^^^ meta.type-signature entity.name.type support.class.prelude.String
--  ^^^^ entity.name.tag

f :: a
f = undefined
-- >> =source.haskell
