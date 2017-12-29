{grammarExpect, customMatchers} = require './util'

describe "Language-Haskell multiline signatures", ->
  grammar = null
  lhs = null

  testBoth = (ex, test) ->
    it "works in Haskell", ->
      test(grammar, ex)
    it "works in Literate Haskell", ->
      test(lhs, '> ' + ex.split('\n').join('\n> '))

  beforeEach ->
    @addMatchers(customMatchers)
    waitsForPromise ->
      atom.packages.activatePackage("language-haskell")

    runs ->
      grammar = atom.grammars.grammarForScopeName("source.haskell")
      lhs = atom.grammars.grammarForScopeName("text.tex.latex.haskell")

  topSigs = [
    '''
    someFunc
      :: String -> String
    '''
    '''
    someFunc
      :: String
      -> String
    '''
    '''
    someFunc
      :: forall res r (s :: * -> *)
       . ( s ~ Maybe
         , Monad s
         )
      => Either res r
      -> Either [res] [r]
    '''
  ]
  negativeTopSigs = [
    # This one's supposed to fail
    '''
    someFunc
        :: String
     -> Integer
    '''
  ]
  doNotationScoped = [
    '''
    f x = do
      stuff :: String <- x
      return ()
    '''
    '''
    g x = do
      stuff
        :: String
        -> String <- x
      return ()
    '''
    '''
    g x = do
      stuff
        :: String
        -> a <- x
      return ()
    '''
    '''
    h x = do
      stuff
        :: String
        ->> String
        --> IO String <- x
      return ()
    '''
  ]

  describe "Top level signatures", ->
    topSigs.forEach (ex) ->
      it "correctly gets line scopes for #{ex}", ->
        g = grammarExpect grammar, ex
        g.toHaveScopes [['identifier.haskell']].concat(
          [2..ex.split('\n').length].map ->
            ['meta.multiline.type-declaration.haskell']
        )
    topSigs.forEach (ex) ->
      describe "parses #{ex}", ->
        testBoth ex, (grammar, ex) ->
          g = grammarExpect grammar, ex
          # g.toHaveScopes [['identifier.haskell']].concat(
          #   [2..ex.split('\n').length].map ->
          #     ['meta.multiline.type-declaration.haskell']
          # )
          g.tokensToHaveScopes(
            'String': ['entity.name.type.haskell']
            'someFunc': ['identifier.haskell']
          )
  describe "Top level signatures failures", ->
    negativeTopSigs.forEach (ex) ->
      describe "does not parse #{ex}", ->
        testBoth ex, (grammar, ex) ->
          g = grammarExpect grammar, ex
          # g.notToHaveScopes [['identifier.haskell']].concat(
          #   [2..ex.split('\n').length].map ->
          #     ['meta.multiline.type-declaration.haskell']
          # )
          g.tokensToHaveScopes(
            'String': ['entity.name.type.haskell']
            'someFunc': ['identifier.haskell']
          )
          g.tokensNotToHaveScopes('Integer': ['entity.name.type.haskell'])
  describe "Scoped do notation signatures", ->
    doNotationScoped.forEach (ex) ->
      describe "parses #{ex}", ->
        testBoth ex, (grammar, ex) ->
          g = grammarExpect grammar, ex
          g.tokensToHaveScopes(
            'String': ['entity.name.type.haskell']
            'IO': ['entity.name.type.haskell']
            'x': ['identifier.haskell']
            'a': ['variable.other.generic-type.haskell']
          )
          g.tokensNotToHaveScopes(
            'x': ['entity.name.type.haskell', 'variable.other.generic-type.haskell']
            'a': ['identifier.haskell']
          )
