{grammarExpect, customMatchers} = require './util'

describe "Language-Haskell", ->
  grammar = null

  beforeEach ->
    @addMatchers(customMatchers)
    waitsForPromise ->
      atom.packages.activatePackage("language-haskell")

    runs ->
      grammar = atom.grammars.grammarForScopeName("source.haskell")

  it "parses the grammar", ->
    expect(grammar).toBeTruthy()
    expect(grammar.scopeName).toBe "source.haskell"

  describe "chars", ->
    it 'tokenizes general chars', ->
      chars = ['a', '0', '9', 'z', '@', '0', '"']

      for scope, char of chars
        g = grammarExpect grammar, "'#{char}'"
        g.toHaveTokens [["'", char, "'"]]
        g.toHaveScopes [['source.haskell', "string.quoted.single.haskell"]]
        g.tokenToHaveScopes [
          0: ['punctuation.definition.string.begin.haskell']
          2: ['punctuation.definition.string.end.haskell']
        ]

    it 'tokenizes escape chars', ->
      escapeChars = ['\\t', '\\n', '\\\'']
      for scope, char of escapeChars
        g = grammarExpect grammar, "'#{char}'"
        g.toHaveTokens [["'", char, "'"]]
        g.toHaveScopes [['source.haskell', "string.quoted.single.haskell"]]
        g.tokenToHaveScopes [
          0: ['punctuation.definition.string.begin.haskell']
          1: ['constant.character.escape.haskell']
          2: ['punctuation.definition.string.end.haskell']
        ]
    it 'tokenizes control chars', ->
      escapeChars = [64..95].map (x) -> "\\^#{String.fromCharCode(x)}"
      for scope, char of escapeChars
        g = grammarExpect grammar, "'#{char}'"
        g.toHaveTokens [["'", char, "'"]]
        g.toHaveScopes [['source.haskell', "string.quoted.single.haskell"]]
        g.tokenToHaveScopes [1: ["constant.character.escape.control.haskell"]]

  describe "keywords", ->
    { controlKeywords, otherKeywords } = require '../src/include/util'

    controlKeywords.forEach (keyword) ->
      it "tokenizes #{keyword} as a control keyword", ->
        g = grammarExpect grammar, keyword
        g.toHaveTokens [[keyword]]
        g.toHaveScopes [["keyword.control.#{keyword}.haskell"]]

    otherKeywords.forEach (keyword) ->
      it "tokenizes #{keyword} as a keyword", ->
        g = grammarExpect grammar, keyword
        g.toHaveTokens [[keyword]]
        g.toHaveScopes [["keyword.other.#{keyword}.haskell"]]

    ['infix', 'infixl', 'infixr'].forEach (keyword) ->
      it "tokenizes #{keyword} as a keyword", ->
        g = grammarExpect grammar, keyword
        g.toHaveTokens [[keyword]]
        g.toHaveScopes [["keyword.operator.#{keyword}.haskell"]]

  describe "Prelude", ->
    prelude = require '../src/include/prelude'
    # classes,funct,constr,types,operators
    test = (what, template, tokens, scope) ->
      describe what, ->
        prelude[what].forEach (x) ->
          it "handles #{what} #{x}", ->
            g = grammarExpect grammar, template(x)
            g.toHaveTokens [tokens(x)]
            g.tokenToHaveScopes [scope(x)]
    test "classes",
      (x) -> "func :: #{x} a => a",
      (x) -> ['func', ' ', '::', ' ', x, ' ', 'a', ' ', '=>', ' ', 'a']
      (x) -> 4: ["entity.name.type.haskell", "entity.other.inherited-class.prelude.#{x}.haskell"]
    test "funct",
      (x) -> "#{x}",
      (x) -> [x]
      (x) -> 0: ["identifier.haskell", "support.function.prelude.#{x}.haskell"]
    test "constr",
      (x) -> "#{x}",
      (x) -> [x]
      (x) -> 0: ["entity.name.tag.haskell", "support.tag.prelude.#{x}.haskell"]
    test "types",
      (x) -> "type Test = #{x}",
      (x) -> ['type', ' ', 'Test', ' ', '=', ' ', x]
      (x) -> 6: ["entity.name.type.haskell", "support.class.prelude.#{x}.haskell"]
    # operators are handled separately

  describe "identifiers", ->
    it 'doesnt highlight partial prelude names', ->
      g = grammarExpect(grammar, "top'n'tail")
      g.toHaveScopes [['source.haskell', 'identifier.haskell']]
      g.toHaveTokens [["top'n'tail"]]
      g.tokensToHaveScopes {
        "top'n'tail": ['identifier.haskell']
      }

  describe ':: declarations', ->
    it 'parses newline declarations', ->
      g = grammarExpect(grammar, 'function :: Type -> OtherType')
      g.toHaveScopes [['source.haskell', 'meta.function.type-declaration.haskell']]
      g.toHaveTokens [[ 'function', ' ', '::', ' ', 'Type', ' ', '->', ' ', 'OtherType' ]]
      g.tokensToHaveScopes {
        'function': ['entity.name.function.haskell']
        '::': ['keyword.other.double-colon.haskell']
        'Type': ['entity.name.type.haskell']
        '->': ['keyword.other.arrow.haskell']
        'OtherType': ['entity.name.type.haskell']
      }

    it 'parses in-line parenthesised declarations', ->
      g = grammarExpect(grammar, 'main = (putStrLn :: String -> IO ()) ("Hello World" :: String)')
      g.toHaveScopes [['source.haskell']]
      g.toHaveTokens [[
          "main", " ", "=", " ", "(", "putStrLn", " ", "::", " ", "String", " ",
          "->", " ", "IO", " ", "()", ")", " ", "(", "\"", "Hello World", "\"",
          " ", "::", " ", "String", ")"
        ]]
      g.tokensToHaveScopes {
        "main" : ['identifier.haskell']
        "=" : ['keyword.operator.haskell']
        "putStrLn" : ['support.function.prelude.putStrLn.haskell' ]
        "::" : ['keyword.other.double-colon.haskell']
        "String" : ['entity.name.type.haskell', 'support.class.prelude.String.haskell']
        "->" : ['keyword.other.arrow.haskell']
        "IO" : ['entity.name.type.haskell', 'support.class.prelude.IO.haskell']
        "()" : ['constant.language.unit.haskell' ]
        "Hello World" : ['string.quoted.double.haskell']
      }

    it 'doesnt get confused by quoted ::', ->
      g = grammarExpect(grammar, '("x :: String -> IO ()" ++ var)')
      g.toHaveScopes [['source.haskell']]
      g.toHaveTokens [[ "(", "\"", "x :: String -> IO ()", "\"", " ", "++", " ", "var", ")"]]
      g.tokensToHaveScopes {
        "x :: String -> IO ()" : ['string.quoted.double.haskell']
        "++" : ['keyword.operator.haskell']
        "var" : ['identifier.haskell']
      }

    it 'parses in-line non-parenthesised declarations', ->
      g = grammarExpect(grammar, 'main = putStrLn "Hello World" :: IO ()')
      g.toHaveScopes [['source.haskell']]
      g.toHaveTokens [[
        'main', ' ', '=', ' ', 'putStrLn', ' ', '"', 'Hello World', '"', ' ', '::', ' ', 'IO', ' ', '()'
      ]]
      g.tokensToHaveScopes {
        'main' : [ 'identifier.haskell' ]
        '=' : [ 'keyword.operator.haskell' ]
        'putStrLn' : [ 'identifier.haskell', 'support.function.prelude.putStrLn.haskell' ]
        '"' : [ 'string.quoted.double.haskell' ]
        'Hello World' : [ 'string.quoted.double.haskell' ]
        '::' : [ 'keyword.other.double-colon.haskell' ]
        'IO' : [ 'meta.type-signature.haskell', 'entity.name.type.haskell', 'support.class.prelude.IO.haskell' ]
        '()' : [ 'meta.type-signature.haskell', 'constant.language.unit.haskell' ]
      }
      g.tokenToHaveScopes [
        6: [ 'punctuation.definition.string.begin.haskell' ]
        8: [ 'punctuation.definition.string.end.haskell' ]
      ]

  describe "type operators", ->
    it "parses type operators", ->
      data = ":: a *** b"
      {tokens} = grammar.tokenizeLine(data)
      expect(tokens[4].value).toEqual '***'
      expect(tokens[4].scopes).toContain 'keyword.operator.haskell'
    it "doesn't confuse arrows and type operators", ->
      g = grammarExpect(grammar, ":: a --> b")
      g.toHaveTokens [['::', ' ', 'a', ' ', '-->', ' ', 'b']]
      g.toHaveScopes [['source.haskell']]
      g.tokenToHaveScopes [4: ['keyword.operator.haskell', 'meta.type-signature.haskell']]

      g = grammarExpect(grammar, ":: a ->- b")
      g.toHaveTokens [['::', ' ', 'a', ' ', '->-', ' ', 'b']]
      g.toHaveScopes [['source.haskell']]
      g.tokenToHaveScopes [4: ['keyword.operator.haskell', 'meta.type-signature.haskell']]

      g = grammarExpect(grammar, ":: a ==> b")
      g.toHaveTokens [['::', ' ', 'a', ' ', '==>', ' ', 'b']]
      g.toHaveScopes [['source.haskell']]
      g.tokenToHaveScopes [4: ['keyword.operator.haskell', 'meta.type-signature.haskell']]

      g = grammarExpect(grammar, ":: a =>= b")
      g.toHaveTokens [['::', ' ', 'a', ' ', '=>=', ' ', 'b']]
      g.toHaveScopes [['source.haskell']]
      g.tokenToHaveScopes [4: ['keyword.operator.haskell', 'meta.type-signature.haskell']]

  describe "comments", ->
    it "parses block comments", ->
      g = grammarExpect grammar, "{- this is a block comment -}"
      g.toHaveTokens [['{-', ' this is a block comment ', '-}']]
      g.toHaveScopes [['source.haskell', 'comment.block.haskell']]
      g.tokenToHaveScopes [
        0: ['punctuation.definition.comment.block.start.haskell']
        2: ['punctuation.definition.comment.block.end.haskell']
      ]

    it "parses nested block comments", ->
      g = grammarExpect grammar, "{- this is a {- nested -} block comment -}"
      g.toHaveTokens [['{-', ' this is a ', '{-', ' nested ', '-}', ' block comment ', '-}']]
      g.toHaveScopes [['source.haskell', 'comment.block.haskell']]
      g.tokenToHaveScopes [
        0: ['punctuation.definition.comment.block.start.haskell']
        2: ['punctuation.definition.comment.block.start.haskell']
        4: ['punctuation.definition.comment.block.end.haskell']
        6: ['punctuation.definition.comment.block.end.haskell']
      ]

    it "parses pragmas as comments in block comments", ->
      g = grammarExpect grammar, '{- this is a {-# nested #-} block comment -}'
      g.toHaveTokens [['{-', ' this is a ', '{-', '# nested #', '-}', ' block comment ', '-}']]
      g.toHaveScopes [['source.haskell', 'comment.block.haskell']]
      g.tokenToHaveScopes [
        0: ['punctuation.definition.comment.block.start.haskell']
        2: ['punctuation.definition.comment.block.start.haskell']
        4: ['punctuation.definition.comment.block.end.haskell']
        6: ['punctuation.definition.comment.block.end.haskell']
      ]

  describe "pragmas", ->
    it "parses pragmas", ->
      g = grammarExpect grammar, '{-# LANGUAGE OverloadedStrings #-}'
      g.toHaveTokens [['{-#', ' ', 'LANGUAGE', ' OverloadedStrings ', '#-}']]
      g.toHaveScopes [['source.haskell', 'meta.preprocessor.haskell']]
      g.tokenToHaveScopes [2: ['keyword.other.preprocessor.haskell']]

    it "parses lowercase pragmas", ->
      g = grammarExpect grammar, '{-# language OverloadedStrings #-}'
      g.toHaveTokens [['{-#', ' ', 'language', ' OverloadedStrings ', '#-}']]
      g.toHaveScopes [['source.haskell', 'meta.preprocessor.haskell']]
      g.tokenToHaveScopes [2: ['keyword.other.preprocessor.haskell']]

    it "parses mixed case pragmas", ->
      g = grammarExpect grammar, '{-# lanGuaGE OverloadedStrings #-}'
      g.toHaveTokens [['{-#', ' ', 'lanGuaGE', ' OverloadedStrings ', '#-}']]
      g.toHaveScopes [['source.haskell', 'meta.preprocessor.haskell']]
      g.tokenToHaveScopes [2: ['keyword.other.preprocessor.haskell']]

  describe "instance", ->
    it "recognizes instances", ->
      g = grammarExpect grammar, 'instance Class where'
      g.toHaveTokens [['instance', ' ', 'Class', ' ', 'where']]
      g.toHaveScopes [['source.haskell', 'meta.declaration.instance.haskell']]
      g.tokenToHaveScopes [
        1: ['meta.type-signature.haskell']
        2: ['meta.type-signature.haskell', 'entity.name.type.haskell']
        3: ['meta.type-signature.haskell']
        4: ['keyword.other.haskell']
      ]
    it "recognizes instance pragmas", ->
      for p in [ 'OVERLAPS', 'OVERLAPPING', 'OVERLAPPABLE', 'INCOHERENT' ]
        g = grammarExpect grammar, "instance {-# #{p} #-} Class where"
        g.toHaveTokens [['instance', ' ', '{-#', ' ', p, ' ', '#-}', ' ', 'Class', ' ', 'where']]
        g.toHaveScopes [['source.haskell', 'meta.declaration.instance.haskell']]
        g.tokenToHaveScopes [
          2: ['meta.preprocessor.haskell']
          3: ['meta.preprocessor.haskell']
          4: ['meta.preprocessor.haskell', 'keyword.other.preprocessor.haskell']
          5: ['meta.preprocessor.haskell']
          6: ['meta.preprocessor.haskell']
        ]

    it "recognizes lowercase instance pragmas", ->
      for p in [ 'overlaps', 'overlapping', 'overlappable', 'incoherent' ]
        g = grammarExpect grammar, "instance {-# #{p} #-} Class where"
        g.toHaveTokens [['instance', ' ', '{-#', ' ', p, ' ', '#-}', ' ', 'Class', ' ', 'where']]
        g.toHaveScopes [['source.haskell', 'meta.declaration.instance.haskell']]
        g.tokenToHaveScopes [
          2: ['meta.preprocessor.haskell']
          3: ['meta.preprocessor.haskell']
          4: ['meta.preprocessor.haskell', 'keyword.other.preprocessor.haskell']
          5: ['meta.preprocessor.haskell']
          6: ['meta.preprocessor.haskell']
        ]
  describe "module", ->
    it "understands module declarations", ->
      g = grammarExpect grammar, 'module Module where'
      g.toHaveTokens [['module', ' ', 'Module', ' ', 'where']]
      g.toHaveScopes [['source.haskell', 'meta.declaration.module.haskell']]
      g.tokenToHaveScopes [2: ['support.other.module.haskell']]
    it "understands module declarations with exports", ->
      g = grammarExpect grammar, 'module Module (export1, export2) where'
      g.toHaveTokens [['module', ' ', 'Module', ' ', '(', 'export1', ',', ' ', 'export2', ')', ' ', 'where']]
      g.toHaveScopes [['source.haskell', 'meta.declaration.module.haskell']]
      g.tokenToHaveScopes [
        2: ['support.other.module.haskell']
        5: ['meta.declaration.exports.haskell', 'entity.name.function.haskell']
        8: ['meta.declaration.exports.haskell', 'entity.name.function.haskell']
      ]
    it "understands module declarations with operator exports", ->
      g = grammarExpect grammar, 'module Module ((<|>), export2) where'
      g.toHaveTokens [['module', ' ', 'Module', ' ', '(', '(<|>)', ',', ' ', 'export2', ')', ' ', 'where']]
      g.toHaveScopes [['source.haskell', 'meta.declaration.module.haskell']]
      g.tokenToHaveScopes [
        2: ['support.other.module.haskell']
        5: ['meta.declaration.exports.haskell', 'entity.name.function.operator.haskell']
        8: ['meta.declaration.exports.haskell', 'entity.name.function.haskell']
      ]
    it "understands module declarations with export lists", ->
      g = grammarExpect grammar, 'module Module (export1 (..), export2 (Something)) where'
      g.toHaveTokens [['module', ' ', 'Module', ' ', '(', 'export1', ' (' , '..', ')',
                       ',', ' ', 'export2', ' (', 'Something', ')', ')', ' ', 'where']]
      g.toHaveScopes [['source.haskell', 'meta.declaration.module.haskell']]
      g.tokenToHaveScopes [
        2: ['support.other.module.haskell']
        5: ['meta.declaration.exports.haskell', 'entity.name.function.haskell']
        7: ['meta.declaration.exports.haskell', 'meta.other.constructor-list.haskell',
             'keyword.operator.wildcard.haskell']
        11: ['meta.declaration.exports.haskell', 'entity.name.function.haskell']
        13: ['meta.declaration.exports.haskell', 'meta.other.constructor-list.haskell',
              'entity.name.tag.haskell']
      ]
  describe "regression test for comments after module name in imports", ->
    it "parses comments after module names", ->
      g = grammarExpect grammar, 'import Module -- comment'
      g.toHaveTokens [['import', ' ', 'Module', ' ', '--', ' comment']]
      g.toHaveScopes [['source.haskell', 'meta.import.haskell']]
      g.tokenToHaveScopes [
        2: ['support.other.module.haskell']
        4: ['comment.line.double-dash.haskell', 'punctuation.definition.comment.haskell']
        5: ['comment.line.double-dash.haskell']
      ]

  describe "quasiqotes", ->
    it "parses unqualified quasiquotes", ->
      g = grammarExpect grammar, '[q| do maybe String|]'
      g.toHaveTokens [['[', 'q', '|', ' do maybe String', '|', ']']]
      g.toHaveScopes [['source.haskell']]
      g.tokenToHaveScopes [
        0: ['punctuation.definition.quasiquotes.begin.haskell']
        1: ['entity.name.tag.haskell']
        3: ['quoted.quasiquotes.haskell']
        5: ['punctuation.definition.quasiquotes.end.haskell']
      ]

    it "parses qualified quasiquotes", ->
      g = grammarExpect grammar, '[Some.Module.Name.q| do maybe String|]'
      g.toHaveTokens [['[', 'Some.Module.Name.', 'q', '|', ' do maybe String', '|', ']']]
      g.toHaveScopes [['source.haskell']]
      g.tokenToHaveScopes [
        0: ['punctuation.definition.quasiquotes.begin.haskell']
        1: ['entity.name.tag.haskell', 'support.other.module.haskell']
        2: ['entity.name.tag.haskell']
        4: ['quoted.quasiquotes.haskell']
        6: ['punctuation.definition.quasiquotes.end.haskell']
      ]
