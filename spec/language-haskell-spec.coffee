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
        {tokens} = grammar.tokenizeLine("'" + char + "'")
        expect(tokens).toEqual [
          {value:"'", scopes: ["source.haskell", 'string.quoted.single.haskell', "punctuation.definition.string.begin.haskell"]}
          {value: char, scopes: ["source.haskell", 'string.quoted.single.haskell']}
          {value:"'", scopes: ["source.haskell", 'string.quoted.single.haskell', "punctuation.definition.string.end.haskell"]}
        ]

    it 'tokenizes escape chars', ->
      escapeChars = ['\\t', '\\n', '\\\'']
      for scope, char of escapeChars
        {tokens} = grammar.tokenizeLine("'" + char + "'")
        expect(tokens).toEqual [
          {value:"'", scopes: ["source.haskell", 'string.quoted.single.haskell', "punctuation.definition.string.begin.haskell"]}
          {value: char, scopes: ["source.haskell", 'string.quoted.single.haskell', 'constant.character.escape.haskell']}
          {value:"'", scopes: ["source.haskell", 'string.quoted.single.haskell', "punctuation.definition.string.end.haskell"]}
        ]
    it 'tokenizes control chars', ->
      escapeChars = [64..95].map (x) -> "\\^#{String.fromCharCode(x)}"
      for scope, char of escapeChars
        g = grammarExpect grammar, "'#{char}'"
        g.toHaveTokens [["'", char, "'"]]
        g.toHaveScopes [['source.haskell', "string.quoted.single.haskell"]]
        g.tokenToHaveScopes [[ [1, ["constant.character.escape.control.haskell"]] ]]

  describe "strings", ->
    it "tokenizes single-line strings", ->
      {tokens} = grammar.tokenizeLine '"abcde\\n\\EOT\\EOL"'
      expect(tokens).toEqual  [
        { value : '"', scopes : [ 'source.haskell', 'string.quoted.double.haskell', 'punctuation.definition.string.begin.haskell' ] }
        { value : 'abcde', scopes : [ 'source.haskell', 'string.quoted.double.haskell' ] }
        { value : '\\n', scopes : [ 'source.haskell', 'string.quoted.double.haskell', 'constant.character.escape.haskell' ] }
        { value : '\\EOT', scopes : [ 'source.haskell', 'string.quoted.double.haskell', 'constant.character.escape.haskell' ] }
        { value : '\\EOL', scopes : [ 'source.haskell', 'string.quoted.double.haskell' ] }
        { value : '"', scopes : [ 'source.haskell', 'string.quoted.double.haskell', 'punctuation.definition.string.end.haskell' ] }
      ]
    it "Regression test for 96", ->
      g = grammarExpect grammar, '"^\\\\ "'
      g.toHaveTokens [["\"", "^", "\\\\", " ", "\""]]
      g.toHaveScopes [['source.haskell', "string.quoted.double.haskell"]]
      g.tokenToHaveScopes [[ [2, ["constant.character.escape.haskell"]] ]]
    it "Supports type-level string literals", ->
      g = grammarExpect grammar, ':: "type-level string"'
      g.toHaveTokens [["::", " ", "\"", "type-level string", "\""]]
      g.toHaveScopes [['source.haskell']]
      g.tokenToHaveScopes [[ [3, ["string.quoted.double.haskell"]] ]]


  describe "backtick function call", ->
    it "finds backtick function names", ->
      {tokens} = grammar.tokenizeLine("\`func\`")
      expect(tokens[0]).toEqual value: '`', scopes: ['source.haskell', 'keyword.operator.function.infix.haskell', 'punctuation.definition.entity.haskell']
      expect(tokens[1]).toEqual value: 'func', scopes: ['source.haskell', 'keyword.operator.function.infix.haskell']
      expect(tokens[2]).toEqual value: '`', scopes: ['source.haskell', 'keyword.operator.function.infix.haskell', 'punctuation.definition.entity.haskell']

  describe "keywords", ->
    controlKeywords = ['do', 'if', 'then', 'else', 'case', 'of', 'let', 'in', 'default', 'mdo', 'rec', 'proc']

    for scope, keyword of controlKeywords
      it "tokenizes #{keyword} as a keyword", ->
        {tokens} = grammar.tokenizeLine(keyword)
        expect(tokens[0]).toEqual value: keyword, scopes: ['source.haskell', 'keyword.control.haskell']

  describe "operators", ->
    it "tokenizes the / arithmetic operator when separated by newlines", ->
      lines = grammar.tokenizeLines """
        1
        / 2
      """
      expect(lines).toEqual  [
          [
            { value : '1', scopes : [ 'source.haskell', 'constant.numeric.decimal.haskell' ] }
          ],
          [
            { value : '/', scopes : [ 'source.haskell', 'keyword.operator.haskell' ] }
            { value : ' ', scopes : [ 'source.haskell' ] }
            { value : '2', scopes : [ 'source.haskell', 'constant.numeric.decimal.haskell' ] }
          ]
        ]

  describe "ids", ->
    it 'handles type_ids', ->
      typeIds = ['Char', 'Data', 'List', 'Int', 'Integral', 'Float', 'Date']

      for scope, id of typeIds
        {tokens} = grammar.tokenizeLine(id)
        expect(tokens[0]).toEqual value: id, scopes: ['source.haskell', 'entity.name.tag.haskell']

  describe "identifiers", ->
    it 'doesnt highlight partial prelude names', ->
      g = grammarExpect(grammar, "top'n'tail")
      g.toHaveScopes [['source.haskell', 'identifier.haskell']]
      g.toHaveTokenScopes [
        [ "top'n'tail" : [ 'identifier.haskell' ]]
      ]

  describe ':: declarations', ->
    it 'parses newline declarations', ->
      g = grammarExpect(grammar, 'function :: Type -> OtherType')
      g.toHaveScopes [['source.haskell', 'meta.function.type-declaration.haskell']]
      g.toHaveTokenScopes [
        [ 'function' : [ 'entity.name.function.haskell' ]
        , ' '
        , '::' : [ 'keyword.other.double-colon.haskell' ]
        , ' '
        , 'Type' : [ 'meta.type-signature.haskell', 'entity.name.type.haskell' ]
        , ' '
        , '->' : [ 'meta.type-signature.haskell', 'keyword.other.arrow.haskell' ]
        , ' '
        , 'OtherType' : [ 'meta.type-signature.haskell', 'entity.name.type.haskell' ]
        ]]

    it 'parses in-line parenthesised declarations', ->
      g = grammarExpect(grammar, 'main = (putStrLn :: String -> IO ()) ("Hello World" :: String)')
      g.toHaveScopes [['source.haskell']]
      g.toHaveTokenScopes [
        [ "main" : ['identifier.haskell']
        , " "
        , "=" : ['keyword.operator.haskell']
        , " "
        , "("
        , "putStrLn" : ['support.function.prelude.putStrLn.haskell' ]
        , " "
        , "::" : ['keyword.other.double-colon.haskell']
        , " "
        , "String" : ['entity.name.type.haskell', 'support.class.prelude.String.haskell']
        , " "
        , "->" : ['keyword.other.arrow.haskell']
        , " "
        , "IO" : ['entity.name.type.haskell', 'support.class.prelude.IO.haskell']
        , " "
        , "()" : ['constant.language.unit.haskell' ]
        , ")"
        , " "
        , "("
        , "\""
        , "Hello World" : ['string.quoted.double.haskell']
        , "\""
        , " "
        , "::" : ['keyword.other.double-colon.haskell']
        , " "
        , "String" : ['entity.name.type.haskell', 'support.class.prelude.String.haskell']
        , ")"
        ]
      ]

    it 'doesnt get confused by quoted ::', ->
      g = grammarExpect(grammar, '("x :: String -> IO ()" ++ var)')
      g.toHaveScopes [['source.haskell']]
      g.toHaveTokenScopes [
        [ "("
        , "\""
        , "x :: String -> IO ()" : ['string.quoted.double.haskell']
        , "\""
        , " "
        , "++" : ['keyword.operator.haskell']
        , " "
        , "var" : ['identifier.haskell']
        , ")"
        ]
      ]

    it 'parses in-line non-parenthesised declarations', ->
      g = grammarExpect(grammar, 'main = putStrLn "Hello World" :: IO ()')
      g.toHaveScopes [['source.haskell']]
      g.toHaveTokenScopes [
        [ 'main' : [ 'identifier.haskell' ]
        , ' '
        , '=' : [ 'keyword.operator.haskell' ]
        , ' '
        , 'putStrLn' : [ 'identifier.haskell', 'support.function.prelude.putStrLn.haskell' ]
        , ' '
        , {'"' : [ 'string.quoted.double.haskell', 'punctuation.definition.string.begin.haskell' ]}
        , {'Hello World' : [ 'string.quoted.double.haskell' ]}
        , {'"' : [ 'string.quoted.double.haskell', 'punctuation.definition.string.end.haskell' ]}
        , ' '
        , '::' : [ 'keyword.other.double-colon.haskell' ]
        , ' '
        , 'IO' : [ 'meta.type-signature.haskell', 'entity.name.type.haskell', 'support.class.prelude.IO.haskell' ]
        , ' '
        , '()' : [ 'meta.type-signature.haskell', 'constant.language.unit.haskell' ]
        ]
      ]

  describe 'regression test for 65', ->
    it 'works with space', ->
      g = grammarExpect(grammar, 'data Foo = Foo {bar :: Bar}')
      g.toHaveScopes [['source.haskell', 'meta.declaration.type.data.haskell']]
      g.toHaveTokenScopes [
        [ 'data' : [ 'keyword.other.data.haskell' ]
        , ' '
        , 'Foo' : [ 'meta.type-signature.haskell', 'entity.name.type.haskell' ]
        , ' ' : [ 'meta.type-signature.haskell' ]
        , '=' : [ 'keyword.operator.assignment.haskell' ]
        , ' '
        , 'Foo' : [ 'entity.name.tag.haskell' ]
        , ' '
        , '{' : [ 'meta.declaration.type.data.record.block.haskell', 'keyword.operator.record.begin.haskell' ]
        , 'bar' : [ 'meta.record-field.type-declaration.haskell', 'entity.other.attribute-name.haskell' ]
        , ' '
        , '::' : [ 'keyword.other.double-colon.haskell' ]
        , ' ' : [ 'meta.type-signature.haskell' ]
        , 'Bar' : [ 'meta.type-signature.haskell', 'entity.name.type.haskell' ]
        , '}' : [ 'meta.declaration.type.data.record.block.haskell', 'keyword.operator.record.end.haskell' ]
        ]
      ]

    it 'works without space', ->
      data = 'data Foo = Foo{bar :: Bar}'
      {tokens} = grammar.tokenizeLine(data)
      expect(tokens).toEqual [
        { value : 'data', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'keyword.other.data.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell' ] }
        { value : 'Foo', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.type-signature.haskell', 'entity.name.type.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.type-signature.haskell' ] }
        { value : '=', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'keyword.operator.assignment.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell' ] }
        { value : 'Foo', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'entity.name.tag.haskell' ] }
        { value : '{', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'keyword.operator.record.begin.haskell' ] }
        { value : 'bar', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell', 'entity.other.attribute-name.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell' ] }
        { value : '::', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell', 'keyword.other.double-colon.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell', 'meta.type-signature.haskell' ] }
        { value : 'Bar', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell', 'meta.type-signature.haskell', 'entity.name.type.haskell' ] }
        { value : '}', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'keyword.operator.record.end.haskell' ] }
      ]

  it "properly highlights data declarations", ->
    data = 'data Foo = Foo Bar'
    {tokens} = grammar.tokenizeLine(data)
    # console.log JSON.stringify(tokens, undefined, 2)
    expect(tokens).toEqual [
        {
          "value": "data",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "keyword.other.data.haskell"
          ]
        }
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell"
          ]
        },
        {
          "value": "Foo",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.type-signature.haskell",
            "entity.name.type.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.type-signature.haskell"
          ]
        },
        {
          "value": "=",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "keyword.operator.assignment.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell"
          ]
        },
        {
          "value": "Foo",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "entity.name.tag.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell"
          ]
        },
        {
          "value": "Bar",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.type-signature.haskell"
            "entity.name.type.haskell"
          ]
        }
      ]
  describe "regression test for 71", ->
    it "<-", ->
      data = "x :: String <- undefined"
      {tokens} = grammar.tokenizeLine(data)
      expect(tokens).toEqual [
        { value : 'x', scopes : [ 'source.haskell', 'identifier.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell' ] }
        { value : '::', scopes : [ 'source.haskell', 'keyword.other.double-colon.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.type-signature.haskell' ] }
        { value : 'String', scopes : [ 'source.haskell', 'meta.type-signature.haskell', 'entity.name.type.haskell', 'support.class.prelude.String.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.type-signature.haskell' ] }
        { value : '<-', scopes : [ 'source.haskell', 'keyword.operator.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell' ] }
        { value : 'undefined', scopes : [ 'source.haskell', 'identifier.haskell', 'support.function.prelude.undefined.haskell' ] }
        ]
    it "=", ->
      data = "x :: String = undefined"
      {tokens} = grammar.tokenizeLine(data)
      # console.log JSON.stringify(tokens, undefined, 2)
      expect(tokens).toEqual [
        { value : 'x', scopes : [ 'source.haskell', 'identifier.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell' ] }
        { value : '::', scopes : [ 'source.haskell', 'keyword.other.double-colon.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.type-signature.haskell' ] }
        { value : 'String', scopes : [ 'source.haskell', 'meta.type-signature.haskell', 'entity.name.type.haskell', 'support.class.prelude.String.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.type-signature.haskell' ] }
        { value : '=', scopes : [ 'source.haskell', 'keyword.operator.assignment.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell' ] }
        { value : 'undefined', scopes : [ 'source.haskell', 'identifier.haskell', 'support.function.prelude.undefined.haskell' ] }
        ]
    it "still works for type-op signatures", ->
      data = "smth :: a <-- b"
      {tokens} = grammar.tokenizeLine(data)
      expect(tokens).toEqual [
        { value : 'smth', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'entity.name.function.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell' ] }
        { value : '::', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'keyword.other.double-colon.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'meta.type-signature.haskell' ] }
        { value : 'a', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'meta.type-signature.haskell', 'variable.other.generic-type.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'meta.type-signature.haskell' ] }
        { value : '<--', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'meta.type-signature.haskell', 'keyword.operator.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'meta.type-signature.haskell' ] }
        { value : 'b', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'meta.type-signature.haskell', 'variable.other.generic-type.haskell' ] }
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
      g.tokenToHaveScopes [[[4, ['keyword.operator.haskell', 'meta.type-signature.haskell']]]]

      g = grammarExpect(grammar, ":: a ->- b")
      g.toHaveTokens [['::', ' ', 'a', ' ', '->-', ' ', 'b']]
      g.toHaveScopes [['source.haskell']]
      g.tokenToHaveScopes [[[4, ['keyword.operator.haskell', 'meta.type-signature.haskell']]]]

      g = grammarExpect(grammar, ":: a ==> b")
      g.toHaveTokens [['::', ' ', 'a', ' ', '==>', ' ', 'b']]
      g.toHaveScopes [['source.haskell']]
      g.tokenToHaveScopes [[[4, ['keyword.operator.haskell', 'meta.type-signature.haskell']]]]

      g = grammarExpect(grammar, ":: a =>= b")
      g.toHaveTokens [['::', ' ', 'a', ' ', '=>=', ' ', 'b']]
      g.toHaveScopes [['source.haskell']]
      g.tokenToHaveScopes [[[4, ['keyword.operator.haskell', 'meta.type-signature.haskell']]]]

  describe "comments", ->
    it "parses block comments", ->
      g = grammarExpect grammar, "{- this is a block comment -}"
      g.toHaveTokens [['{-', ' this is a block comment ', '-}']]
      g.toHaveScopes [['source.haskell', 'comment.block.haskell']]
      g.tokenToHaveScopes [[[0, ['punctuation.definition.comment.block.start.haskell']],
                            [2, ['punctuation.definition.comment.block.end.haskell']]]]

    it "parses nested block comments", ->
      g = grammarExpect grammar, "{- this is a {- nested -} block comment -}"
      g.toHaveTokens [['{-', ' this is a ', '{-', ' nested ', '-}', ' block comment ', '-}']]
      g.toHaveScopes [['source.haskell', 'comment.block.haskell']]
      g.tokenToHaveScopes [[[0, ['punctuation.definition.comment.block.start.haskell']]
                            [2, ['punctuation.definition.comment.block.start.haskell']]
                            [4, ['punctuation.definition.comment.block.end.haskell']]
                            [6, ['punctuation.definition.comment.block.end.haskell']]]]

    it "parses pragmas as comments in block comments", ->
      g = grammarExpect grammar, '{- this is a {-# nested #-} block comment -}'
      g.toHaveTokens [['{-', ' this is a ', '{-', '# nested #', '-}', ' block comment ', '-}']]
      g.toHaveScopes [['source.haskell', 'comment.block.haskell']]
      g.tokenToHaveScopes [[[0, ['punctuation.definition.comment.block.start.haskell']]
                            [2, ['punctuation.definition.comment.block.start.haskell']]
                            [4, ['punctuation.definition.comment.block.end.haskell']]
                            [6, ['punctuation.definition.comment.block.end.haskell']]]]
  describe "instance", ->
    it "recognizes instances", ->
      g = grammarExpect grammar, 'instance Class where'
      g.toHaveTokens [['instance', ' ', 'Class', ' ', 'where']]
      g.toHaveScopes [['source.haskell', 'meta.declaration.instance.haskell']]
      g.tokenToHaveScopes [[[1, ['meta.type-signature.haskell']]
                            [2, ['meta.type-signature.haskell', 'entity.name.type.haskell']]
                            [3, ['meta.type-signature.haskell']]
                            [4, ['keyword.other.haskell']]
                            ]]
    it "recognizes instance pragmas", ->
      for p in [ 'OVERLAPS', 'OVERLAPPING', 'OVERLAPPABLE', 'INCOHERENT' ]
        g = grammarExpect grammar, "instance {-# #{p} #-} Class where"
        g.toHaveTokens [['instance', ' ', '{-#', ' ', p, ' ', '#-}', ' ', 'Class', ' ', 'where']]
        g.toHaveScopes [['source.haskell', 'meta.declaration.instance.haskell']]
        g.tokenToHaveScopes [[[2, ['meta.preprocessor.haskell']]
                              [3, ['meta.preprocessor.haskell']]
                              [4, ['meta.preprocessor.haskell', 'keyword.other.preprocessor.haskell']]
                              [5, ['meta.preprocessor.haskell']]
                              [6, ['meta.preprocessor.haskell']]
                              ]]
  describe "module", ->
    it "understands module declarations", ->
      g = grammarExpect grammar, 'module Module where'
      g.toHaveTokens [['module', ' ', 'Module', ' ', 'where']]
      g.toHaveScopes [['source.haskell', 'meta.declaration.module.haskell']]
      g.tokenToHaveScopes [[[2, ['support.other.module.haskell']]]]
    it "understands module declarations with exports", ->
      g = grammarExpect grammar, 'module Module (export1, export2) where'
      g.toHaveTokens [['module', ' ', 'Module', ' ', '(', 'export1', ',', ' ', 'export2', ')', ' ', 'where']]
      g.toHaveScopes [['source.haskell', 'meta.declaration.module.haskell']]
      g.tokenToHaveScopes [[[2, ['support.other.module.haskell']]
                            [5, ['meta.declaration.exports.haskell', 'entity.name.function.haskell']]
                            [8, ['meta.declaration.exports.haskell', 'entity.name.function.haskell']]
                            ]]
    it "understands module declarations with operator exports", ->
      g = grammarExpect grammar, 'module Module ((<|>), export2) where'
      g.toHaveTokens [['module', ' ', 'Module', ' ', '(', '(<|>)', ',', ' ', 'export2', ')', ' ', 'where']]
      g.toHaveScopes [['source.haskell', 'meta.declaration.module.haskell']]
      g.tokenToHaveScopes [[[2, ['support.other.module.haskell']]
                            [5, ['meta.declaration.exports.haskell', 'entity.name.function.infix.haskell']]
                            [8, ['meta.declaration.exports.haskell', 'entity.name.function.haskell']]
                            ]]
    it "understands module declarations with export lists", ->
      g = grammarExpect grammar, 'module Module (export1 (..), export2 (Something)) where'
      g.toHaveTokens [['module', ' ', 'Module', ' ', '(', 'export1', ' (' , '..', ')',
                       ',', ' ', 'export2', ' (', 'Something', ')', ')', ' ', 'where']]
      g.toHaveScopes [['source.haskell', 'meta.declaration.module.haskell']]
      g.tokenToHaveScopes [[[2, ['support.other.module.haskell']]
                            [5, ['meta.declaration.exports.haskell', 'entity.name.function.haskell']]
                            [7, ['meta.declaration.exports.haskell', 'meta.other.constructor-list.haskell',
                                 'keyword.operator.wildcard.haskell']]
                            [11, ['meta.declaration.exports.haskell', 'entity.name.function.haskell']]
                            [13, ['meta.declaration.exports.haskell', 'meta.other.constructor-list.haskell',
                                  'entity.name.tag.haskell']]
                            ]]
