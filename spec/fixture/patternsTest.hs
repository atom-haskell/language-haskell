module SomeModule
  ( pattern SomePat
--  ^^^^^^^ meta.declaration.module meta.declaration.exports meta.declaration.export.qualified.pattern keyword.other.pattern
--          ^^^^^^^ meta.declaration.module meta.declaration.exports meta.declaration.export.qualified.pattern entity.name.tag
  , type OtherPat
--  ^^^^ meta.declaration.module meta.declaration.exports meta.declaration.export.qualified.type keyword.other.type
--       ^^^^^^^^ meta.declaration.module meta.declaration.exports meta.declaration.export.qualified.type entity.name.type
  , OtherPat
--  ^^^^^^^^ meta.declaration.module meta.declaration.exports entity.name.type
  , Eq(..)
--     ^^ meta.declaration.module meta.declaration.exports meta.other.constructor-list keyword.operator.wildcard
  , type (++)
--  ^^^^ meta.declaration.module meta.declaration.exports meta.declaration.export.qualified.type keyword.other.type
--        ^^ meta.declaration.module meta.declaration.exports meta.declaration.export.qualified.type keyword.operator support.operator.prelude
  , pattern (:|)
--  ^^^^^^^ meta.declaration.module meta.declaration.exports meta.declaration.export.qualified.pattern keyword.other.pattern
--          ^^^^ meta.declaration.module meta.declaration.exports meta.declaration.export.qualified.pattern entity.name.function.operator
  ) where

fun :: (++)
--      ^^ meta.function.type-declaration meta.type-signature keyword.operator support.operator.prelude

pattern SomePat :: a -> a
pattern SomePat x <- x
