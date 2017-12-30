-- SYNTAX TEST "source.haskell"
module Test where

main :: IO ()
-- <- meta.function.type-declaration entity.name.function
main = do
-- <- identifier

-- it tokenizes identifiers
    aSimpleIdentifier
--  ^^^^^^^^^^^^^^^^^ identifier

-- it tokenizes identifiers with module names
    Some.Module.identifier
--  ^^^^^^^^^^^^^^^^^^^^^^ identifier
--  ^^^^^^^^^^^^ support.other.module

-- it tokenizes type constructors
    SomeCtor
--  ^^^^^^^^ entity.name.tag

-- it tokenizes type constructors with module names
    Some.Module.SomeCtor
--  ^^^^^^^^^^^^^^^^^^^^ entity.name.tag
--  ^^^^^^^^^^^^ support.other.module

-- it tokenizes identifiers with numeric parts
    numer123ident
--  ^^^^^^^^^^^^^ identifier
    numerident123
--  ^^^^^^^^^^^^^ identifier
    123numerident
--  ^^^ constant.numeric.decimal
--     ^^^^^^^^^^ identifier

-- it doesnt confuse identifiers starting with type (issue 84)
    typeIdentifier
--  ^^^^^^^^^^^^^^ identifier

-- it doesnt confuse identifiers starting with data
    dataIdentifier
--  ^^^^^^^^^^^^^^ identifier

-- it doesnt confuse identifiers starting with newtype
    newtypeIdentifier
--  ^^^^^^^^^^^^^^^^^ identifier

-- it parses identifiers with '
    anIdentifierWith'
--  ^^^^^^^^^^^^^^^^^ identifier
    anIdenti'fierWit'h
--  ^^^^^^^^^^^^^^^^^^ identifier

-- it doesn't confuse identifiers with and without '
    foldl
--  ^^^^^ identifier support.function.prelude.foldl
    foldl'
--  ^^^^^^ identifier


-- >> =source.haskell
