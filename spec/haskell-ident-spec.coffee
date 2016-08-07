describe "Language-Haskell", ->
  grammar = null

  beforeEach ->
    waitsForPromise ->
      atom.packages.activatePackage("language-haskell")

    runs ->
      grammar = atom.grammars.grammarForScopeName("source.haskell")

  describe "identifiers", ->
    it 'tokenizes identifiers', ->
      {tokens} = grammar.tokenizeLine('aSimpleIdentifier')
      expect(tokens).toEqual [
        { value : 'aSimpleIdentifier', scopes : [ 'source.haskell', 'identifier.haskell' ] }
      ]
    it 'tokenizes identifiers with module names', ->
      {tokens} = grammar.tokenizeLine('Some.Module.identifier')
      expect(tokens).toEqual [
        { value : 'Some.Module.', scopes : [ 'source.haskell', 'identifier.haskell', 'support.other.module.haskell' ] }
        { value : 'identifier', scopes : [ 'source.haskell', 'identifier.haskell' ] }
      ]
    it 'tokenizes type constructors', ->
      {tokens} = grammar.tokenizeLine('SomeCtor')
      expect(tokens).toEqual [
        { value : 'SomeCtor', scopes : [ 'source.haskell', 'entity.name.tag.haskell' ] }
      ]
    it 'tokenizes type constructors with module names', ->
      {tokens} = grammar.tokenizeLine('Some.Module.SomeCtor')
      expect(tokens).toEqual [
        { value : 'Some.Module.', scopes : [ 'source.haskell', 'entity.name.tag.haskell', 'support.other.module.haskell' ] }
        { value : 'SomeCtor', scopes : [ 'source.haskell', 'entity.name.tag.haskell' ] }
      ]
    it 'tokenizes identifiers with numeric parts', ->
      {tokens} = grammar.tokenizeLine('numer123ident')
      expect(tokens).toEqual [
        { value : 'numer123ident', scopes : [ 'source.haskell', 'identifier.haskell' ] }
      ]
      {tokens} = grammar.tokenizeLine('numerident123')
      expect(tokens).toEqual [
        { value : 'numerident123', scopes : [ 'source.haskell', 'identifier.haskell' ] }
      ]
      {tokens} = grammar.tokenizeLine('123numerident')
      expect(tokens).toEqual [
        { value : '123', scopes : [ 'source.haskell', 'constant.numeric.decimal.haskell' ] }
        { value : 'numerident', scopes : [ 'source.haskell', 'identifier.haskell' ] }
      ]
