-- SYNTAX TEST "source.haskell"

data New = Somtehing deriving stock Something
--                                  ^^^^^^^^^ meta.deriving entity.other.inherited-class
--                            ^^^^^ meta.deriving meta.deriving.strategy keyword.other
--                   ^^^^^^^^ meta.declaration.type.data meta.deriving keyword.other

data New = Somtehing deriving newtype Something
--                            ^^^^^^^ meta.deriving meta.deriving.strategy keyword.other

data New = Somtehing deriving anyclass Something
--                            ^^^^^^^^ meta.deriving meta.deriving.strategy keyword.other

data New = Somtehing deriving stock (Something, Other)
--                                   ^^^^^^^^^ meta.deriving entity.other.inherited-class
--                            ^^^^^ meta.deriving meta.deriving.strategy keyword.other
--                   ^^^^^^^^ meta.declaration.type.data meta.deriving keyword.other

data New = Somtehing deriving newtype (Something, Other)
--                            ^^^^^^^ meta.deriving meta.deriving.strategy keyword.other

data New = Somtehing deriving anyclass (Something, Other)
--                            ^^^^^^^^ meta.deriving meta.deriving.strategy keyword.other

data New = Somtehing deriving stock
--                            ^^^^^ meta.deriving meta.deriving.strategy keyword.other
--                   ^^^^^^^^ meta.declaration.type.data meta.deriving keyword.other

data New = Somtehing deriving newtype
--                            ^^^^^^^ meta.deriving meta.deriving.strategy keyword.other

data New = Somtehing deriving anyclass
--                            ^^^^^^^^ meta.deriving meta.deriving.strategy keyword.other

-- standalone deriving

deriving instance Something New
--                          ^^^ meta.type-signature entity.name.type
--                ^^^^^^^^^ meta.type-signature entity.name.type
--       ^^^^^^^^ keyword.other
-- <- meta.declaration.instance.deriving keyword.other

deriving stock instance Something New
--       ^^^^^ meta.deriving.strategy keyword.other
--                                ^^^ meta.type-signature entity.name.type
--                      ^^^^^^^^^ meta.type-signature entity.name.type
--             ^^^^^^^^ keyword.other
-- <- meta.declaration.instance.deriving keyword.other

deriving anyclass instance Something New
--       ^^^^^^^^ meta.deriving.strategy keyword.other

deriving newtype instance Something New
--       ^^^^^^^ meta.deriving.strategy keyword.other

-- deriving via

newtype New = New Int deriving Generic via Int
--                                         ^^^ meta.via entity.name.type
--                                     ^^^ meta.declaration.type.data meta.via keyword.other

newtype New = New Int deriving Generic via (Hex Int)
--                                     ^^^ keyword.other
--                                          ^^^ entity.name.type
--                                              ^^^ entity.name.type
--                                     ^^^^^^^^^^^^^ meta.declaration.type.data meta.via
deriving via Lift (ReaderT r) m instance (MonadExit m) => (MonadExit (ReaderT r m))
--                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.instance.deriving meta.type-signature
--           ^^^^^^^^^^^^^^^^^^ meta.declaration.instance.deriving meta.type-signature
--       ^^^                    ^^^^^^^^ meta.declaration.instance.deriving keyword.other

data Foo = Bar deriving Class via (Baz Quux FooBar)
--                                 ^^^ ^^^^ ^^^^^^ meta.declaration.type.data meta.via entity.name.type
data Foo = Bar deriving Class via Baz Quux FooBar
--                                ^^^ ^^^^ ^^^^^^ meta.declaration.type.data meta.via entity.name.type
data Foo = Bar deriving Class
  via Baz
    Quux FooBar
--  ^^^^ ^^^^^^ meta.declaration.type.data meta.via entity.name.type
data Foo = Bar deriving Class
    via Baz
    Quux FooBar
--  ^^^^ ^^^^^^ meta.declaration.type.data meta.via entity.name.type
