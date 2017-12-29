{grammarExpect, customMatchers} = require './util'

describe "Literate Haskell", ->
  grammar = null

  beforeEach ->
    @addMatchers(customMatchers)
    waitsForPromise ->
      atom.packages.activatePackage("language-haskell")

    runs ->
      grammar = atom.grammars.grammarForScopeName("text.tex.latex.haskell")

  it "parses the grammar", ->
    expect(grammar).toBeTruthy()
    expect(grammar.scopeName).toBe "text.tex.latex.haskell"

  describe "regression test for 64", ->
    it "parses inline signatures", ->
      g = grammarExpect grammar, 'a signature |f::Type| should be contained'
      g.toHaveTokens [['a signature ', '|', 'f', '::', 'Type', '|', ' should be contained']]
      g.toHaveScopes [['text.tex.latex.haskell']]
      g.tokenToHaveScopes [
        1: ['meta.embedded.text.haskell.latex.haskell']
        2: ['meta.embedded.text.haskell.latex.haskell', 'meta.function.type-declaration.haskell']
        3: ['meta.embedded.text.haskell.latex.haskell', 'keyword.other.double-colon.haskell']
        4: ['meta.embedded.text.haskell.latex.haskell'
            , 'meta.type-signature.haskell'
            , 'entity.name.type.haskell']
        5: ['meta.embedded.text.haskell.latex.haskell']
        ]
    it "parses inline signatures with dots", ->
      g = grammarExpect grammar, 'a signature |f::Type|. should be contained'
      g.toHaveTokens [['a signature ', '|', 'f', '::', 'Type', '|', '. should be contained']]
      g.toHaveScopes [['text.tex.latex.haskell']]
      g.tokenToHaveScopes [
        1: ['meta.embedded.text.haskell.latex.haskell']
        2: ['meta.embedded.text.haskell.latex.haskell', 'meta.function.type-declaration.haskell']
        3: [ 'meta.embedded.text.haskell.latex.haskell'
           , 'keyword.other.double-colon.haskell']
        4: ['meta.embedded.text.haskell.latex.haskell'
           , 'meta.type-signature.haskell'
           , 'entity.name.type.haskell']
        5: ['meta.embedded.text.haskell.latex.haskell']
        ]
    it "parses inline code with pipes", ->
      g = grammarExpect grammar, 'a code |type Bool = True || False| should parse correctly'
      g.toHaveTokens [['a code ', '|', 'type', ' ', 'Bool', ' ', '=', ' ', 'True', ' '
                      , '||', ' ', 'False', '|', ' should parse correctly']]
      g.toHaveScopes [['text.tex.latex.haskell']]
      g.tokenToHaveScopes [
        1: ['meta.embedded.text.haskell.latex.haskell']
        2: ["keyword.other.type.haskell"]
        3: ["meta.type-signature.haskell"]
        4: ["entity.name.type.haskell"]
        6:  ['keyword.operator.assignment.haskell']
        8:  ['entity.name.type.haskell']
        10: ['keyword.operator.haskell']
        12: ['entity.name.type.haskell']
        13: ['meta.embedded.text.haskell.latex.haskell']
      ]
    it "parses inline code with pipes", ->
      g = grammarExpect grammar, 'a |code||||| should parse correctly'
      g.toHaveTokens [['a ', '|', 'code', '||||', '|', ' should parse correctly']]
      g.toHaveScopes [['text.tex.latex.haskell']]
      g.tokenToHaveScopes [
        1: ['meta.embedded.text.haskell.latex.haskell']
        2: ["identifier.haskell"]
        3: ["keyword.operator.haskell"]
        4: ['meta.embedded.text.haskell.latex.haskell']
      ]
