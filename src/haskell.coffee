{list, listMaybe, concat, include, makeGrammar} = require './syntax-tools'

prelude = include 'prelude'

toString = (rx) ->
  if rx instanceof RegExp
    rx.source
  else
    rx

list = (item, s, sep) ->
  #recursive regexp, caution advised
  "(?<#{item}>(?:#{toString s})(?:\\s*(?:#{toString sep})\\s*\\g<#{item}>)?)"

listMaybe = (item, s, sep) ->
  #recursive regexp, caution advised
  "(?<#{item}>(?:#{toString s})(?:\\s*(?:#{toString sep})\\s*\\g<#{item}>)?)?"

concat = (list...) ->
  r = ''.concat (list.map (i) -> "(?:#{toString i})")...
  "(?:#{r})"

haskellGrammar =
  name: 'Haskell'
  fileTypes: [ 'hs' ]
  firstLineMatch: '^\\#\\!.*\\brunhaskell\\b'
  scopeName: 'source.haskell'

  macros: include 'macros'
  repository: include 'repository'

  patterns: [
      name: 'block.liquidhaskell'
      contentName: 'block.liquidhaskell.annotation'
      begin: '\\{-@(?!#)'
      end: '@-\\}'
      patterns: [
          include: '$self'
      ]
    ,
      name: 'comment.line.shebang.haskell'
      match: '^\\#\\!.*\\brunhaskell\\b.*$'
    ,
      name: 'keyword.operator.function.infix.haskell'
      match: /(`){functionName}(`)/
      captures:
        1: name: 'punctuation.definition.entity.haskell'
        2: name: 'punctuation.definition.entity.haskell'
      ###
      In case this regex seems unusual for an infix operator, note
      that Haskell allows any ordinary function application (elem 4 [1..10])
      to be rewritten as an infix expression (4 `elem` [1..10]).
      ###
    ,
      name: 'constant.language.unit.haskell'
      match: /\(\)/
    ,
      name: 'constant.language.empty-list.haskell'
      match: /\[\]/
    ,
      begin: /(\[)({functionNameOne})(\|)/
      end: /(\|)(\])/
      beginCaptures:
        1: name: 'punctuation.definition.quasiquotes.begin.haskell'
        2: name: 'entity.name.tag.haskell'
        3: name: 'string.quoted.quasiquotes.haskell'
      endCaptures:
        1: name: 'string.quoted.quasiquotes.haskell'
        2: name: 'punctuation.definition.quasiquotes.end.haskell'
      contentName: 'string.quoted.quasiquotes.haskell'
    ,
      name: 'meta.declaration.module.haskell'
      begin: /{lb}(module){rb}/
      end: /{lb}(where){rb}/
      beginCaptures:
        1: name: 'keyword.other.haskell'
      endCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          include: '#comments'
        ,
          include: '#module_name'
        ,
          include: '#module_exports'
        ,
          name: 'invalid'
          match: /[a-z]+/
      ]
    ,
      name: 'meta.declaration.class.haskell'
      begin: /{lb}(class){rb}/
      end: /{lb}(where){rb}|$/
      beginCaptures:
        1: name: 'storage.type.class.haskell'
      endCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          name: 'support.class.prelude.haskell'
          match: "{lb}(#{prelude.classes.join('|')}){rb}"
        ,
          include: '#type_name'
        ,
          include: '#generic_type'
      ]
    ,
      name: 'meta.declaration.instance.haskell'
      begin: /{lb}(instance){rb}/
      end: /{lb}(where){rb}|$/
      contentName: 'meta.type-signature.haskell'
      beginCaptures:
        1: name: 'keyword.other.haskell'
      endCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          include: '#type_signature'
      ]
    ,
      name: 'meta.foreign.haskell'
      begin: /{maybeBirdTrack}(\s*)(foreign)\s+(import|export){rb}/
      end: /{indentBlockEnd}/
      beginCaptures:
        2: name: 'keyword.other.haskell'
        3: name: 'keyword.other.haskell'
      patterns:[
          match: /(?:un)?safe/
          captures:
            0: name: 'keyword.other.haskell'
        ,
          include: '$self'
      ]
    ,
      name: 'meta.import.haskell'
      begin: /{lb}(import){rb}/
      end: /($|;|(?=--))/
      beginCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          include: '#module_name'
        ,
          include: '#module_exports'
        ,
          match: /{lb}(qualified|as|hiding){rb}/
          captures:
            1: name: 'keyword.other.haskell'
      ]
    ,
      name: 'meta.declaration.type.GADT.haskell'
      begin: /{maybeBirdTrack}(\s*)(data|newtype)\s+({typeDecl})(?=\s+where{rb})/
      end: /{indentBlockEnd}/
      beginCaptures:
        2: name: 'storage.type.data.haskell'
        3:
          name: 'meta.type-signature.haskell'
          patterns: [include: '#type_signature']
      patterns: [
          include: '#comments'
        ,
          include: '#deriving'
        ,
          match: /{ctor}/
          captures:
            1: patterns: [include: '#type_ctor']
            2:
              name: 'meta.type-signature.haskell'
              patterns: [include: '#type_signature']
        ,
          name: 'keyword.other.haskell'
          match: /{lb}where{rb}/
        ,
          include: '#type_name'
        ,
          include: '#ctor_type_declaration'
      ]
    ,
      name: 'meta.declaration.type.data.haskell'
      begin: /{maybeBirdTrack}(\s*)(data|newtype)\s+({typeDecl})/
      end: /{indentBlockEnd}/
      beginCaptures:
        2: name: 'storage.type.data.haskell'
        3:
          name: 'meta.type-signature.haskell'
          patterns: [include: '#type_signature']
      patterns: [
          include: '#comments'
        ,
          include: '#deriving'
        ,
          match: /=/
          captures:
            0: name: 'keyword.operator.assignment.haskell'
        ,
          match: /{ctor}/
          captures:
            1: patterns: [include: '#type_ctor']
            2:
              name: 'meta.type-signature.haskell'
              patterns: [include: '#type_signature']
        ,
          match: /\|/
          captures:
            0: name: 'punctuation.separator.pipe.haskell'
        ,
          name: 'meta.declaration.type.data.record.block.haskell'
          begin: /\{/
          beginCaptures:
            0: name: 'keyword.operator.record.begin.haskell'
          end: /\}/
          endCaptures:
            0: name: 'keyword.operator.record.end.haskell'
          patterns: [
              name: 'punctuation.separator.comma.haskell'
              match: /,/
            ,
              include: '#record_field_declaration'
          ]
      ]
    ,
      name: 'meta.declaration.type.type.haskell'
      begin: /{maybeBirdTrack}(\s*)(type(?:\s+(?:family|instance))?)\s+({typeDecl})/
      end: /{indentBlockEnd}|(?={lb}where{rb})/
      contentName: 'meta.type-signature.haskell'
      beginCaptures:
        2: name: 'storage.type.data.haskell'
        3:
          name: 'meta.type-signature.haskell'
          patterns: [include: '#type_signature']
      patterns: [
          include: '#comments'
        ,
          match: /=/
          captures:
            0: name: 'keyword.operator.assignment.haskell'
        ,
          include: '#type_signature'
      ]
    ,
      name: 'keyword.other.haskell'
      match: /{lb}(deriving|where|data|type|newtype){rb}/
    ,
      name: 'storage.type.haskell'
      match: /{lb}(data|type|newtype){rb}/
    ,
      name: 'keyword.operator.haskell'
      match: /{lb}infix[lr]?{rb}/
    ,
      name: 'keyword.control.haskell'
      match: /{lb}(do|if|then|else|case|of|let|in|default){rb}/
    ,
      name: 'meta.preprocessor.c'
      begin: /{maybeBirdTrack}(?=#)/
      end: '(?<!\\\\)(?=\\n)'
      patterns: [
        include: 'source.c'
      ]
      ###
      In addition to Haskell's "native" syntax, GHC permits the C
      preprocessor to be run on a source file.
      ###
    ,
      include: '#pragma'
    ,
      name: 'string.quoted.double.haskell'
      begin: /"/
      end: /"/
      beginCaptures:
        0: name: 'punctuation.definition.string.begin.haskell'
      endCaptures:
        0: name: 'punctuation.definition.string.end.haskell'
      patterns: [
          include: '#characters'
        ,
          begin: /\\\s/
          end: /\\/
          beginCaptures:
            0: name: 'markup.other.escape.newline.begin.haskell'
          endCaptures:
            0: name: 'markup.other.escape.newline.end.haskell'
          patterns: [
              match: /\S+/
              name: 'invalid.illegal.character-not-allowed-here.haskell'
          ]
      ]
    ,
      name: 'markup.other.escape.newline.haskell'
      match: /\\$/
    ,
      name: 'string.quoted.single.haskell'
      match: /(')({character})(')/
      captures:
        1: name: 'punctuation.definition.string.begin.haskell'
        2:
          patterns:[
            include: '#characters'
          ]
        3: name: 'punctuation.definition.string.end.haskell'
    ,
      include: '#function_type_declaration'
    ,
      match: '\\((?<paren>(?:[^()]|\\(\\g<paren>\\))*)(::|∷)(?<paren2>(?:[^()]|\\(\\g<paren2>\\))*)\\)'
      captures:
        1: patterns: [include: 'source.haskell']
        2: name: 'keyword.other.double-colon.haskell'
        3: {name: 'meta.type-signature.haskell', patterns: [include: '#type_signature']}
    ,
      # match: '(::|∷)((?:(?:{className}|{functionName}|->|=>|[→⇒()\\[\\]]|\\s)(?!:<-|=))*)'
      match: '(::|∷)((?:{className}|{functionName}|\\->|=>|[→⇒()\\[\\]]|\\s)*)'
      captures:
        1: name: 'keyword.other.double-colon.haskell'
        2: {name: 'meta.type-signature.haskell', patterns: [include: '#type_signature']}
    ,
      name: 'support.tag.haskell'
      match: "{lb}(#{prelude.constr.join('|')}){rb}"
    ,
      include: '#comments'
    ,
      name: 'support.function.prelude.haskell'
      match: "{lb}(#{prelude.funct.join('|')}){rb}"
    ,
      include: '#infix_op'
    ,
      name: 'punctuation.separator.comma.haskell'
      match: /,/
    ,
      name: 'constant.numeric.hexadecimal.haskell'
      match: '(?<!{identCharClass})0[xX][0-9a-fA-F]+'
    ,
      name: 'constant.numeric.octal.haskell'
      match: '(?<!{identCharClass})0[oO][0-7]+'
    ,
      name: 'constant.numeric.float.haskell'
      match: '(?<!{identCharClass})[0-9]+\\.[0-9]+([eE][+-]?[0-9]+)?'
    ,
      name: 'constant.numeric.float.haskell'
      match: '(?<!{identCharClass})[0-9]+[eE][+-]?[0-9]+'
      # Floats are always decimal
    ,
      name: 'constant.numeric.decimal.haskell'
      match: '(?<!{identCharClass})[0-9]+'
    ,
      name: 'keyword.operator.haskell'
      match: /{operator}/
    ,
      include: '#type_ctor'
    ,
      match: '{lb}{functionName}{rb}'
      name: 'identifier.haskell'
      captures:
        0:
          patterns: [
            name: 'support.other.module.haskell'
            match: /^(?:{className}\.)*{className}\.?/
          ]
  ]

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
