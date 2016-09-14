{grammarExpect, customMatchers} = require './util'

describe "Language-Haskell", ->
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
      g.tokenToHaveScopes [[[1, ['meta.embedded.text.haskell.latex.haskell']]
                          , [2, ['meta.embedded.text.haskell.latex.haskell', 'identifier.haskell']]
                          , [3, ['meta.embedded.text.haskell.latex.haskell', 'keyword.other.double-colon.haskell']]
                          , [4, ['meta.embedded.text.haskell.latex.haskell'
                                , 'meta.type-signature.haskell'
                                , 'entity.name.type.haskell']]
                          , [5, ['meta.embedded.text.haskell.latex.haskell']]
                          ]]
