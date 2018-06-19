-- Regression test for #118

module MyList
  ( MyList(Nil,(:>))
--             ^^^^ meta.declaration.module meta.declaration.exports meta.other.constructor-list entity.name.function.operator
--         ^^^ meta.declaration.module meta.declaration.exports meta.other.constructor-list entity.name.tag
--  ^^^^^^ meta.declaration.module meta.declaration.exports entity.name.type
  , myHead
--  ^^^^^^ meta.declaration.module meta.declaration.exports entity.name.function
  ) where
