-- SYNTAX TEST "source.haskell"

{-# LANGUAGE TypeOperators, RankNTypes, ScopedTypeVariables, KindSignatures, TypeFamilies, GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-matches #-}

module Test where
--          ^^^^^ meta.declaration.module keyword.other
--     ^^^^ meta.declaration.module support.other.module
-- <- meta.declaration.module keyword.other

someFunc :: String
 --         ^^^^^^ meta.function.type-declaration meta.type-signature entity.name.type support.class.prelude.String
         -> String
--          ^^^^^^ meta.function.type-declaration meta.type-signature entity.name.type support.class.prelude.String

someFunc''
  :: String
  -- ^^^^^^ meta.multiline.type-declaration meta.type-signature entity.name.type support.class.prelude.String
  -> String
--   ^^^^^^ meta.multiline.type-declaration meta.type-signature entity.name.type support.class.prelude.String

someFunc'
    :: String
--     ^ meta.multiline.type-declaration meta.type-signature entity.name.type support.class.prelude.String
 -> String
--  ^ entity.name.tag

someFunc = undefined
someFunc' = undefined
someFunc'' = undefined

f x = do
  stuff :: String <- x
--                   ^ identifier
--         ^ meta.type-signature entity.name.type support.class.prelude.String
  return ()

g x = do
  stuff
    :: String
       -- <- meta.multiline.type-declaration meta.type-signature entity.name.type support.class.prelude.String
    -> String <- x
--               ^ identifier
--     ^ meta.multiline.type-declaration meta.type-signature entity.name.type support.class.prelude.String
  return ()
  -- <- identifier support.function.prelude.return

h x = do
  stuff
    :: String
       -- <- meta.multiline.type-declaration meta.type-signature entity.name.type support.class.prelude.String
    ->> String
    -- <- meta.multiline.type-declaration meta.type-signature keyword.operator
        -- <- meta.multiline.type-declaration meta.type-signature entity.name.type support.class.prelude.String
    -- <- meta.multiline.type-declaration meta.type-signature keyword.operator
    --> IO String <- x
--  ^ meta.multiline.type-declaration meta.type-signature keyword.operator
--                   ^ identifier
--         ^ meta.multiline.type-declaration meta.type-signature entity.name.type support.class.prelude.String
--      ^ meta.multiline.type-declaration meta.type-signature entity.name.type support.class.prelude.IO
  return ()
data (->>) a b = DArr a b
data (-->) a b = DDArr a b

acquire
  :: forall res r (s :: * -> *)
                          -- <- meta.multiline.type-declaration meta.type-signature keyword.other.arrow
                        -- <- meta.multiline.type-declaration meta.type-signature keyword.operator support.operator.prelude
            -- <- meta.multiline.type-declaration meta.type-signature variable.other.generic-type
     -- <- meta.multiline.type-declaration meta.type-signature keyword.other.forall
   . ( s ~ Maybe
           -- <- meta.multiline.type-declaration meta.type-signature entity.name.type support.class.prelude.Maybe
         -- <- meta.multiline.type-declaration meta.type-signature keyword.operator
       -- <- meta.multiline.type-declaration meta.type-signature variable.other.generic-type
   -- <- meta.multiline.type-declaration meta.type-signature keyword.operator support.operator.prelude
     , Monad s
             -- <- meta.multiline.type-declaration meta.type-signature variable.other.generic-type
       -- <- meta.multiline.type-declaration meta.type-signature entity.name.type entity.other.inherited-class.prelude.Monad
     )
  => Either res r
                -- <- meta.multiline.type-declaration meta.type-signature variable.other.generic-type
            -- <- meta.multiline.type-declaration meta.type-signature variable.other.generic-type
  -- <- meta.multiline.type-declaration meta.type-signature keyword.other.big-arrow
     -- <- meta.multiline.type-declaration meta.type-signature entity.name.type support.class.prelude.Either
  -> Either [res] [r]
                   -- <- meta.multiline.type-declaration meta.type-signature variable.other.generic-type
             -- <- meta.multiline.type-declaration meta.type-signature variable.other.generic-type
     -- <- meta.multiline.type-declaration meta.type-signature entity.name.type support.class.prelude.Either
  -- <- meta.multiline.type-declaration meta.type-signature keyword.other.arrow
acquire = undefined
--        ^^^^^^^^^ identifier support.function.prelude.undefined
-- <- identifier

function :: String -> Maybe Int = Just . read
--                                ^ entity.name.tag
-- <- identifier

function' :: String -> Maybe Int
             -- <- meta.function.type-declaration meta.type-signature entity.name.type support.class.prelude.String
  -- <- meta.function.type-declaration entity.name.function
  = Just . read
--  ^ entity.name.tag

smth = do
  x :: Int
       -- <- meta.function.type-declaration meta.type-signature entity.name.type support.class.prelude.Int
    <- Just 1
--          ^ constant.numeric.decimal
--     ^ entity.name.tag
  return ()
-- >> =source.haskell
