{list, listMaybe, concat, include, makeGrammar} = require './syntax-tools'

haskellGrammar =
  name: 'Haskell'
  fileTypes: [ 'hs' ]
  firstLineMatch: '^\\#\\!.*\\brunhaskell\\b'
  scopeName: 'source.haskell'

  macros: include 'macros'
  repository: include 'repository'
  patterns: include 'haskell-patterns'

makeGrammar haskellGrammar, "grammars/haskell.cson"

completionHintGrammar =
  name: 'Haskell Autocompletion Hint'
  fileTypes: []
  scopeName: 'hint.haskell'

  macros: haskellGrammar.macros
  patterns: [
      include: '#function_type_declaration'
    ,
      include: '#ctor_type_declaration'
  ]
  repository: haskellGrammar.repository

makeGrammar completionHintGrammar, "grammars/haskell autocompletion hint.cson"

typeHintGrammar =
  name: 'Haskell Type Hint'
  fileTypes: []
  scopeName: 'hint.type.haskell'

  macros: haskellGrammar.macros
  patterns: [
      include: '#type_signature'
  ]
  repository: haskellGrammar.repository

makeGrammar typeHintGrammar, "grammars/haskell type hint.cson"

messageHintGrammar =
  name: 'Haskell Message Hint'
  fileTypes: []
  scopeName: 'hint.message.haskell'

  macros: haskellGrammar.macros
  patterns: [
      match: /^[^:]*:(.+)$/
      captures:
        1:
          patterns: [
            include: 'source.haskell'
          ]
    ,
      begin: /^[^:]*:$/
      end: /^(?=\S)/
      patterns: [
        include: 'source.haskell'
      ]
    ,
      begin: /‘/
      end: /’/
      patterns: [
        include: 'source.haskell'
      ]
  ]
  repository: haskellGrammar.repository

makeGrammar messageHintGrammar, "grammars/haskell message hint.cson"

literateHaskellGrammar =
  name: 'Literate Haskell'
  fileTypes: [ 'lhs' ]
  scopeName: 'text.tex.latex.haskell'

  macros: haskellGrammar.macros
  patterns: [
      begin: /^((\\)begin)({)(code|spec)(})(\s*\n)?/
      beginCaptures:
        1:
          name: 'support.function.be.latex'
        2:
          name: 'punctuation.definition.function.latex'
        3:
          name: 'punctuation.definition.arguments.begin.latex'
        5:
          name: 'punctuation.definition.arguments.end.latex'
      end: /^((\\)end)({)\4(})/
      endCaptures:
        1:
          name: 'support.function.be.latex'
        2:
          name: 'punctuation.definition.function.latex'
        3:
          name: 'punctuation.definition.arguments.begin.latex'
        4:
          name: 'punctuation.definition.arguments.end.latex'
      contentName: 'source.haskell.embedded.latex'
      name: 'meta.embedded.block.haskell.latex'
      patterns: [
          include: 'source.haskell'
      ]
    ,
      begin: /^(?=[><] )/
      end: /^(?![><] )/
      name: 'meta.embedded.haskell'
      patterns: haskellGrammar.patterns.concat
        match: /^> /
        name: 'punctuation.definition.bird-track.haskell'
    ,
      begin: '(?<!\\\\verb)\\|'
      end: /\|/
      name: 'meta.embedded.text.haskell.latex'
      patterns: haskellGrammar.patterns
    ,
      include: 'text.tex.latex'
  ]
  repository: haskellGrammar.repository

literateHaskellGrammar.macros.maybeBirdTrack = /^(?:>|<) /
literateHaskellGrammar.macros.indentBlockEnd =
  /^(?!(?:>|<) \1{indentChar}|(?:>|<) {indentChar}*$)|^(?!(?:>|<) )/
literateHaskellGrammar.macros.operatorChar = /[\p{S}\p{P}&&[^(),;\[\]`{}_"'\|]]/

makeGrammar literateHaskellGrammar, "grammars/literate haskell.cson"
