-- SYNTAX TEST "source.haskell"
-- see https://gitlab.haskell.org/ghc/ghc/-/blob/9c9e40e59214b1e358c85852218f3a67e712a748/testsuite/tests/typecheck/T11342/T11342a.hs
-- https://github.com/atom-haskell/language-haskell/issues/128
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module T11342a where

import Data.Type.Equality

type A = '"' :: Char
--              ^^^^ meta.declaration.type.type meta.type-signature entity.name.type support.class.prelude.Char
--       ^^^ meta.declaration.type.type meta.type-signature string.quoted.single

-- I am a blue comment

t :: '"' :~: '"'
--           ^^^ meta.function.type-declaration meta.type-signature string.quoted.single
--   ^^^ meta.function.type-declaration meta.type-signature string.quoted.single
--       ^^^ meta.function.type-declaration meta.type-signature keyword.operator
t = Refl
--  ^^^^ entity.name.tag
