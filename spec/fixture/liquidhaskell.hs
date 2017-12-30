-- SYNTAX TEST "source.haskell"
module Intro where
-- <- keyword.other
--     ^^^^^ support.other.module
--           ^^^^^ keyword.other
-- ^^^^^^^^^^^^^^^ meta.declaration.module

import Language.Haskell.Liquid.Prelude  (liquidAssert)
--                                       ^^^^^^^^^^^^ meta.declaration.exports entity.name.function
-- <- keyword.other
--     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ support.other.module

zero' :: Int
zero' = 0

{-@ zero' :: {v: Int | 0 <= v} @-}
--           ^^^^^^^^^^^^^^^^^ liquid.type
--                       ^^ keyword.operator
--                     ^ constant.numeric
--               ^^^ entity.name.type
--  ^^^^^ entity.name.function
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^ block.liquidhaskell.annotation
-- <- block.liquidhaskell

{-@ zero'' :: {v: Int | (0 <= v && v < 100) } @-}
--  ^^^^^^ entity.name.function
--                ^^^ entity.name.type
--            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ liquid.type
--                         ^^ keyword.operator
--                                   ^ keyword.operator
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ block.liquidhaskell.annotation
-- <- block.liquidhaskell
zero'' :: Int
-- <- meta.function.type-declaration
zero'' = 0
-- <- identifier

{-@ zero''' :: {v: Int | ((v mod 2) = 0) } @-}
zero''' :: Int
-- <- meta.function.type-declaration
zero''' = 0
-- <- identifier

{-@ zero'''' :: {v: Int | v = 0 } @-}
zero'''' :: Int
zero'''' = 0

{-@ zero :: {v: Int | ((0 <= v) && ((v mod 2) = 0) && (v < 100)) } @-}
zero     :: Int
zero     =  0

{-@ error' :: {v: String | false } -> a  @-}
error'     :: String -> a
error'     = error

{-@ lAssert     :: {v:Bool | (Prop v)} -> a -> a  @-}
lAssert         :: Bool -> a -> a
lAssert True  x = x
lAssert False _ = error' "lAssert failure"

divide     :: Int -> Int -> Int
divide n 0 = error' "divide by zero"
divide n d = n `div` d

{-@ divide :: Int -> {v: Int | v != 0 } -> Int @-}

{-@ divide' :: Int  -> {v:Int | v /= 0} -> Int @-}
divide'     :: Int -> Int -> Int
divide' n 0 = error' "divide by zero"
divide' n d = lAssert (d /= 0) $ n `div` d

abz               :: Int -> Int
abz n | 0 < n     = n
      | otherwise = 0 - n

{-@ abz :: Int -> {v: Int | 0 <= v } @-}
--                ^^^^^^^^^^^^^^^^^^ liquid.type
--                    ^^^ entity.name.type
--  ^^^ entity.name.function
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ block.liquidhaskell.annotation
-- <- block.liquidhaskell

{-@ truncate :: Int -> Int -> Int @-}
truncate i max
  | i' <= max' = i
  | otherwise  = max' * (i `divide` i')
    where i'   = abz i
          max' = abz max

{-@ truncate' :: Int -> Int -> Int @-}
truncate' i max
  | i' <= max' = i
  | otherwise  = lAssert (i' /= 0) $ max' * (i `divide` i')
    where i'   = abz i
          max' = abz max

{-@ truncate'' :: Int -> Int -> Int @-}
truncate'' i max
  | i' <= max' = i
  | otherwise  = liquidAssert (i' /= 0) $ max' * (i `divide` i')
    where i'   = abz i
          max' = abz max

{-@ listAssoc :: x:List a -> y:List a -> z:List a
     -> {(append x (append y z)) == (append (append x y) z) } @-}
--        ^^^^^^ ^  ^^^^^^ ^ ^       ^^^^^^  ^^^^^^ ^ ^  ^ identifier
{-@ type Something = SomethingElse @-}
    -- <- meta.declaration.type
{-@ instance Something where
    -- <- meta.declaration.instance
    asd = instance
    -- <- identifier
@-}
-- >> =source.haskell
