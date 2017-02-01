describe 'GADT', ->
  grammar = null

  zip = () ->
    lengthArray = (arr.length for arr in arguments)
    length = Math.max(lengthArray...)
    for i in [0...length]
      arr[i] for arr in arguments

  check = (line, exp) ->
    for t,i in zip(line,exp)
      t[0] ?= {}
      t[1] ?= {}
      t[0].index=i
      t[1].index=i
      expect(t[0]).toEqual(t[1])

  beforeEach ->
    waitsForPromise ->
      atom.packages.activatePackage("language-haskell")

    runs ->
      grammar = atom.grammars.grammarForScopeName("source.haskell")

  it 'understands GADT syntax', ->
    string = """
        data Term a where
          Lit :: Int -> Term Int
        """
    lines = grammar.tokenizeLines(string)
    exp = [[
          {
            value:'data',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'keyword.other.data.haskell'
            ]
          },
          {
            value:' ',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell'
            ]
          },
          {
            value:'Term',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.type-signature.haskell',
              'entity.name.type.haskell'
            ]
          },
          {
            value:' ',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.type-signature.haskell'
            ]
          },
          {
            value:'a',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.type-signature.haskell',
              'variable.other.generic-type.haskell'
            ]
          },
          {
            value:' ',
            scopes:[
              'source.haskell'
              'meta.declaration.type.data.haskell'
              'meta.type-signature.haskell'
            ]
          },
          {
            value:'where',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'keyword.other.haskell'
            ]
          }
        ],
        [
          {
            value:'  ',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.ctor.type-declaration.haskell'
            ]
          },
          {
            value:'Lit',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.ctor.type-declaration.haskell',
              'entity.name.tag.haskell'
            ]
          },
          {
            value:' ',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.ctor.type-declaration.haskell'
            ]
          },
          {
            value:'::',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.ctor.type-declaration.haskell',
              'keyword.other.double-colon.haskell'
            ]
          },
          {
            value:' ',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.ctor.type-declaration.haskell',
              'meta.type-signature.haskell'
            ]
          },
          {
            value:'Int',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.ctor.type-declaration.haskell',
              'meta.type-signature.haskell',
              'entity.name.type.haskell',
              'support.class.prelude.Int.haskell'
            ]
          },
          {
            value:' ',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.ctor.type-declaration.haskell',
              'meta.type-signature.haskell'
            ]
          },
          {
            value:'->',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.ctor.type-declaration.haskell',
              'meta.type-signature.haskell',
              'keyword.other.arrow.haskell'
            ]
          },
          {
            value:' ',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.ctor.type-declaration.haskell',
              'meta.type-signature.haskell'
            ]
          },
          {
            value:'Term',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.ctor.type-declaration.haskell',
              'meta.type-signature.haskell',
              'entity.name.type.haskell'
            ]
          },
          {
            value:' ',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.ctor.type-declaration.haskell',
              'meta.type-signature.haskell'
            ]
          },
          {
            value:'Int',
            scopes:[
              'source.haskell',
              'meta.declaration.type.data.haskell',
              'meta.ctor.type-declaration.haskell',
              'meta.type-signature.haskell',
              'entity.name.type.haskell',
              'support.class.prelude.Int.haskell'
            ]
          }
        ]]
    for l in zip(lines, exp)
      check l[0], l[1]
