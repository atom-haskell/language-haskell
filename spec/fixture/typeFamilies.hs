-- SYNTAX TEST "source.haskell"
{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, TypeFamilies, UndecidableInstances #-}

module Type.List where

import GHC.TypeLits

data Lst (l :: [k])

type family FromLst a where FromLst (Lst l) = l
-- <- meta.declaration.type.type keyword.other.type
--                                            ^ variable.other.generic-type
--                                          ^  keyword.operator.assignment
--                                       ^ variable.other.generic-type
--                                   ^^^ entity.name.type
--                          ^^^^^^^ entity.name.type
--                    ^^^^^ keyword.other
--                  ^ variable.other.generic-type
--          ^^^^^^^ entity.name.type
--   ^^^^^^ keyword.other
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.type.type meta.type-signature

type family Removed    (el :: e)    (cont :: c) :: l
-- <- meta.declaration.type.type keyword.other.type
--                                                 ^ variable.other.generic-type
--                                           ^ variable.other.generic-type
--                                        ^^ keyword.operator
--                                   ^^^^ variable.other.generic-type
--                         ^^ keyword.operator
--                      ^^ variable.other.generic-type
--          ^^^^^^^ entity.name.type
--   ^^^^^^ keyword.other
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.type.type meta.type-signature
type family RemovedIdx (idx :: Nat) (cont :: c) :: l
--                             ^^^ meta.declaration.type.type meta.type-signature entity.name.type
type family ElAt       (idx :: Nat) (cont :: c) :: l
--                             ^^^ meta.declaration.type.type meta.type-signature entity.name.type

type family Reverse' lst lst' where
 --                           ^^^^^ keyword.other
 --                      ^^^^ variable.other.generic-type
 --                  ^^^ variable.other.generic-type
 --         ^^^^^^^^ entity.name.type
 --  ^^^^^^ keyword.other
 -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.type.type meta.type-signature
    Reverse' '[]       lst = lst
 --                          ^^^ variable.other.generic-type
 --                    ^^^ variable.other.generic-type
 -- ^^^^^^^^ entity.name.type
 -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.type.type meta.type-signature
    Reverse' (l ': ls) lst = Reverse' ls (l ': lst)
 --                                            ^^^ variable.other.generic-type
 --                                   ^^ variable.other.generic-type
 --                          ^^^^^^^^ entity.name.type
 --                    ^^^ variable.other.generic-type
 --                ^^ variable.other.generic-type
 -- ^^^^^^^^ entity.name.type
 -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.type.type meta.type-signature

type family Zip2 (l1 :: [*]) (l2 :: [*]) :: [*] where
 --                                             ^^^^^ keyword.other
 --                           ^^ variable.other.generic-type
 --               ^^ variable.other.generic-type
 --         ^^^^ entity.name.type
 --  ^^^^^^ keyword.other
 -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.type.type meta.type-signature
    Zip2 (x1 ': xs1) (x2 ': xs2) = (x1,x2) ': Zip2 xs1 xs2
 --                                                    ^^^ variable.other.generic-type
 --                                                ^^^ variable.other.generic-type
 --                                           ^^^^ entity.name.type
 --                                    ^^ variable.other.generic-type
 --                                 ^^ variable.other.generic-type
 --                         ^^^ variable.other.generic-type
 --                   ^^ variable.other.generic-type
 --             ^^^ variable.other.generic-type
 --       ^^ variable.other.generic-type
 -- ^^^^ entity.name.type
 -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.type.type meta.type-signature
    Zip2 l1          l2          = '[]
 --                  ^^ meta.declaration.type.type meta.type-signature variable.other.generic-type
 --      ^^ meta.declaration.type.type meta.type-signature variable.other.generic-type
 -- ^^^^ meta.declaration.type.type meta.type-signature entity.name.type
 -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.type.type meta.type-signature
