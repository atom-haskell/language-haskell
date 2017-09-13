module Intro where

import Language.Haskell.Liquid.Prelude  (liquidAssert)

zero' :: Int
zero' = 0

{-@ zero' :: {v: Int | 0 <= v} @-}

{-@ zero'' :: {v: Int | (0 <= v && v < 100) } @-}
zero'' :: Int
zero'' = 0

{-@ zero''' :: {v: Int | ((v mod 2) = 0) } @-}
zero''' :: Int
zero''' = 0

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
{-@ type Something = SomethingElse @-}
{-@ instance Something where
    asd = smth
@-}
