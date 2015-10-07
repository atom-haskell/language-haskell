describe "Language-Haskell", ->
  grammar = null

  beforeEach ->
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

  describe "strings", ->
    it "tokenizes single-line strings", ->
      delimsByScope =
        "string.quoted.double.haskell": '"'

      for scope, delim of delimsByScope
        {tokens} = grammar.tokenizeLine(delim + "x" + delim)
        expect(tokens[0].value).toEqual delim
        expect(tokens[0].scopes).toEqual ["source.haskell", scope]
        expect(tokens[1].value).toEqual "x"
        expect(tokens[1].scopes).toEqual ["source.haskell", scope]
        expect(tokens[2].value).toEqual delim
        expect(tokens[2].scopes).toEqual ["source.haskell", scope]

  describe "backtick function call", ->
    it "finds backtick function names", ->
      {tokens} = grammar.tokenizeLine("\`func\`")
      expect(tokens[0]).toEqual value: '`', scopes: ['source.haskell', 'keyword.operator.function.infix.haskell','punctuation.definition.entity.haskell']
      expect(tokens[1]).toEqual value: 'func', scopes: ['source.haskell', 'keyword.operator.function.infix.haskell']
      expect(tokens[2]).toEqual value: '`', scopes: ['source.haskell', 'keyword.operator.function.infix.haskell','punctuation.definition.entity.haskell']

  describe "keywords", ->
    controlKeywords = ['case', 'of', 'in', 'where', 'if', 'then', 'else']

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
            { value : '1', scopes : [ 'source.haskell', 'constant.numeric.haskell' ] }
          ],
          [
            { value : '/', scopes : [ 'source.haskell', 'keyword.operator.haskell' ] },
            { value : ' ', scopes : [ 'source.haskell' ] },
            { value : '2', scopes : [ 'source.haskell', 'constant.numeric.haskell' ] }
          ]
        ]

  it "tokenizes {-  -} comments", ->
    {tokens} = grammar.tokenizeLine('{--}')

    expect(tokens).toEqual [
        { value : '{-', scopes : [ 'source.haskell', 'comment.block.haskell', 'punctuation.definition.comment.haskell' ] },
        { value : '-}', scopes : [ 'source.haskell', 'comment.block.haskell' ] }
      ]

    {tokens} = grammar.tokenizeLine('{- foo -}')
    expect(tokens).toEqual  [
        { value : '{-', scopes : [ 'source.haskell', 'comment.block.haskell', 'punctuation.definition.comment.haskell' ] },
        { value : ' foo ', scopes : [ 'source.haskell', 'comment.block.haskell' ] },
        { value : '-}', scopes : [ 'source.haskell', 'comment.block.haskell' ] }
      ]

  describe "ids", ->
    it 'handles type_ids', ->
      typeIds = ['Char', 'Data', 'List', 'Int', 'Integral', 'Float', 'Date']

      for scope, id of typeIds
        {tokens} = grammar.tokenizeLine(id)
        expect(tokens[0]).toEqual value: id, scopes: ['source.haskell', 'entity.name.tag.haskell']
