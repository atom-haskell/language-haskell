{grammarExpect, customMatchers} = require './util'
prelude = require '../src/include/prelude'

describe "Language-Haskell Operators", ->
  grammar = null

  beforeEach ->
    @addMatchers(customMatchers)
    waitsForPromise ->
      atom.packages.activatePackage("language-haskell")

    runs ->
      grammar = atom.grammars.grammarForScopeName("source.haskell")

  describe "operators", ->
    it "tokenizes the / arithmetic operator when separated by newlines", ->
      g = grammarExpect grammar, """
        1
        / 2
      """
      g.toHaveTokens [
        ['1']
        ['/', ' ', '2']
      ]
      g.toHaveScopes [['source.haskell'], ['source.haskell']]
      g.tokensToHaveScopes {
        '1': ['constant.numeric.decimal.haskell']
        '/': ['keyword.operator.haskell']
        '2': ['constant.numeric.decimal.haskell']
      }
    prelude.operators.forEach (i) ->
      it "tokenizes #{i} operator", ->
        g = grammarExpect grammar, "a #{i} b"
        g.toHaveTokens [['a', ' ', i, ' ', 'b']]
        g.toHaveScopes [['source.haskell']]
        g.tokenToHaveScopes [2: ['keyword.operator.haskell', 'support.operator.prelude.haskell']]

      it "tokenizes (#{i}) operator function", ->
        g = grammarExpect grammar, "(#{i}) a b"
        g.toHaveTokens [["(#{i})", ' ', 'a', ' ', 'b']]
        g.toHaveScopes [['source.haskell']]
        g.tokenToHaveScopes [0: ['entity.name.function.operator.haskell', 'support.operator.prelude.haskell']]

      it "tokenizes qualified #{i} operator", ->
        g = grammarExpect grammar, "a Prelude.#{i} b"
        g.toHaveTokens [['a', ' ', 'Prelude.', i, ' ', 'b']]
        g.toHaveScopes [['source.haskell']]
        g.tokenToHaveScopes [
          2: ['keyword.operator.haskell', 'support.other.module.haskell']
          3: ['keyword.operator.haskell']
        ]

      it "tokenizes qualified (#{i}) operator function", ->
        g = grammarExpect grammar, "(Prelude.#{i}) a b"
        g.toHaveTokens [['(', 'Prelude.', "#{i})", ' ', 'a', ' ', 'b']]
        g.toHaveScopes [['source.haskell']]
        g.tokenToHaveScopes [
          0: ['entity.name.function.operator.haskell']
          1: ['entity.name.function.operator.haskell', 'support.other.module.haskell']
          2: ['entity.name.function.operator.haskell']
          ]
