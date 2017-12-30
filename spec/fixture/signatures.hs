-- SYNTAX TEST "source.haskell"

{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}

module Test where

data Type = Type
data OtherType = OtherType
type (<--) a b = a b

--  it parses newline declarations
function :: Type -> OtherType
--                  ^^^^^^^^^ meta.function.type-declaration meta.type-signature entity.name.type
--               ^^ meta.function.type-declaration meta.type-signature keyword.other.arrow
--          ^^^^ meta.function.type-declaration meta.type-signature entity.name.type
--         ^^^^^^^^^^^^^^^^^^ meta.function.type-declaration meta.type-signature
--       ^^ meta.function.type-declaration keyword.other.double-colon
-- <- meta.function.type-declaration
function = undefined

-- it parses in-line parenthesised declarations
main :: IO ()
main = (putStrLn :: String -> IO ()) ("Hello World" :: String)
--                                                     ^^^^^^ meta.type-signature entity.name.type
--                                                  ^^ keyword.other.double-colon
--                                    ^^^^^^^^^^^^^ string.quoted.double
--                               ^^ meta.type-signature constant.language.unit
--                            ^^ meta.type-signature entity.name.type
--                         ^^ meta.type-signature keyword.other.arrow
--                  ^^^^^^ meta.type-signature entity.name.type
--               ^^ keyword.other.double-colon
--      ^^^^^^^^ identifier support.function.prelude.putStrLn
--   ^ keyword.operator
-- <- identifier

-- it doesnt get confused by quoted ::
smth1 :: String -> String
smth1 var = ("x :: String -> IO ()" ++ var)
--           ^^^^^^^^^^^^^^^^^^^^^^ string.quoted.double

-- it parses in-line non-parenthesised declarations
main2 :: IO ()
main2 = putStrLn "Hello World" :: IO ()
--                                   ^^ meta.type-signature constant.language.unit
--                                ^^ meta.type-signature entity.name.type support.class.prelude.IO
--                             ^^ keyword.other.double-colon
--               ^^^^^^^^^^^^^ string.quoted.double
--      ^^^^^^^^ identifier
-- <- identifier


--------------------------- regression test for 71 ---------------------------

regressionTest71 :: IO ()
regressionTest71 = do
    -- it stops parsing on <-
    xx :: String <- undefined
--                  ^^^^^^^^^ identifier support.function.prelude.undefined
--               ^^ keyword.operator
--        ^^^^^^ meta.type-signature entity.name.type support.class.prelude.String
--     ^^ keyword.other.double-colon
--  ^^ identifier
    return ()
  where
    -- it stops parsing on =
    yy :: String = undefined
--                 ^^^^^^^^^ identifier support.function.prelude.undefined
--               ^ keyword.operator.assignment
--        ^^^^^^ meta.type-signature entity.name.type support.class.prelude.String
--     ^^ keyword.other.double-colon
--  ^^ identifier
--  it still works for type-operator signatures
    smth :: a <-- b
--  ^^^^^^^^^^^^^^^ meta.function.type-declaration
--  ^^^^ entity.name.function
--       ^^ keyword.other.double-colon
--         ^^^^^^^^ meta.function.type-declaration meta.type-signature
--          ^ variable.other.generic-type
--            ^^^ keyword.operator
--                ^ variable.other.generic-type
    smth = undefined

-- >> =source.haskell
