-- SYNTAX TEST "source.haskell"
module Test where

-- it understands record syntax
data Car1 = Car1 {
--               ^ meta.declaration.type.data meta.declaration.type.data.record.block keyword.operator.record.begin
--   ^^^^ meta.declaration.type.data meta.type-signature entity.name.type
--          ^^^^ meta.declaration.type.data entity.name.tag
    company :: String,
--          ^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration keyword.other.double-colon
--  ^^^^^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration entity.other.attribute-name
--             ^^^^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration meta.type-signature entity.name.type support.class.prelude.String
    model :: String,
--        ^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration keyword.other.double-colon
--           ^^^^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration meta.type-signature entity.name.type support.class.prelude.String
--  ^^^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration entity.other.attribute-name
    year :: Int
--       ^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration keyword.other.double-colon
--  ^^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration entity.other.attribute-name
--          ^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration meta.type-signature entity.name.type support.class.prelude.Int
   } deriving (Show)
--             ^^^^ meta.declaration.type.data meta.deriving entity.other.inherited-class
--   ^^^^^^^^ meta.declaration.type.data meta.deriving keyword.other
-- ^ meta.declaration.type.data meta.declaration.type.data.record.block keyword.operator.record.end

-- it understands comments in records
data Car2 = Car2 {
    company2 :: String, -- comment
--              ^^^^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration meta.type-signature entity.name.type support.class.prelude.String
--  ^^^^^^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration entity.other.attribute-name
    -- model :: String, -- commented field
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration meta.type-signature comment.line.double-dash
    year2 :: Int -- another comment
--           ^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration meta.type-signature entity.name.type support.class.prelude.Int
--  ^^^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration entity.other.attribute-name
  }

-- it understands comments in start of records
newtype Car3 = Car3 {
    -- company :: String
--  ^^^^^^^^^^^^^^^^^^^^ meta.declaration.type.data meta.declaration.type.data.record.block comment.line.double-dash
    model3 :: String
--            ^^^^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration meta.type-signature entity.name.type support.class.prelude.String
--  ^^^^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration entity.other.attribute-name
  }

--------------- regression test for #65 ---------------

-- it works without space

data Foo = Foo{bar :: Int}
--            ^ meta.declaration.type.data meta.declaration.type.data.record.block keyword.operator.record.begin
--                       ^ meta.declaration.type.data meta.declaration.type.data.record.block keyword.operator.record.end
--                    ^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration meta.type-signature entity.name.type
--             ^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration entity.other.attribute-name
--         ^^^ meta.declaration.type.data entity.name.tag
--   ^^^ meta.declaration.type.data meta.type-signature entity.name.type

-- it works with space
data Foo1 = Foo1 {bar1 :: Int}
--               ^ meta.declaration.type.data meta.declaration.type.data.record.block keyword.operator.record.begin
--                           ^ meta.declaration.type.data meta.declaration.type.data.record.block keyword.operator.record.end
--                        ^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration meta.type-signature entity.name.type support.class.prelude.Int
--                ^^^^ meta.declaration.type.data meta.declaration.type.data.record.block meta.record-field.type-declaration entity.other.attribute-name
--          ^^^^ meta.declaration.type.data entity.name.tag
--   ^^^^ meta.declaration.type.data meta.type-signature entity.name.type

f :: a
f = undefined
-- >> =source.haskell
