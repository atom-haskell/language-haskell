{include, makeGrammar} = require './syntax-tools'
_ = require 'underscore-plus'

makeGrammar "grammars/haskell.cson",
  name: 'Haskell'
  fileTypes: [ 'hs', 'hs-boot', 'cpphs' ]
  firstLineMatch: '^\\#\\!.*\\brunhaskell\\b'
  scopeName: 'source.haskell'

  macros: include 'macros'
  repository: include 'repository'
  patterns: include 'haskell-patterns'

makeGrammar "grammars/module signature.cson",
  name: 'Haskell Module Signature'
  fileTypes: [ 'hsig' ]
  scopeName: 'source.hsig'

  macros: include('macros')
  repository: include 'repository'
  patterns: include 'hsig-patterns'

makeGrammar "grammars/haskell autocompletion hint.cson",
  # name: 'Haskell Autocompletion Hint'
  fileTypes: []
  scopeName: 'hint.haskell'

  macros: include 'macros'
  patterns: [
      {include: '#function_type_declaration'}
      {include: '#ctor_type_declaration'}
  ]
  repository: include 'repository'

makeGrammar "grammars/haskell type hint.cson",
  # name: 'Haskell Type Hint'
  fileTypes: []
  scopeName: 'hint.type.haskell'

  macros: include 'macros'
  patterns: [
      include: '#type_signature'
  ]
  repository: include 'repository'

makeGrammar "grammars/haskell message hint.cson",
  # name: 'Haskell Message Hint'
  fileTypes: []
  scopeName: 'hint.message.haskell'

  macros: include 'macros'
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
  repository: include 'repository'

makeGrammar "grammars/literate haskell.cson",
  name: 'Literate Haskell'
  fileTypes: [ 'lhs' ]
  scopeName: 'text.tex.latex.haskell'

  macros: _.extend (require 'clone')(include('macros')),
    maybeBirdTrack: /^(?:>|<) /
    indentBlockEnd:
      /^(?!(?:>|<) \1{indentChar}|(?:>|<) {indentChar}*$)|^(?!(?:>|<) )/
    indentBlockCont:
      /^(?!(?:>|<) \1|(?:>|<) {indentChar}*$)|^(?!(?:>|<) )/
    operatorChar: '(?:[\\p{S}\\p{P}](?<![(),;\\[\\]`{}_"\'\\|]))'
  patterns: include 'lhs-patterns'
  repository: include 'repository'

makeGrammar "grammars/liquid haskell.cson",
  # name: 'Liquid Haskell'
  fileTypes: []
  scopeName: 'annotation.liquidhaskell.haskell'

  macros: _.extend (require 'clone')(include('macros')),
    maybeBirdTrack: '(?:\\G(?:\\s*\\w+\\s)?|^)'
    indentBlockEnd: /(?:^(?!\1{indentChar}|{indentChar}*$)|(?=@-}))/
    indentBlockCont: /(?:^(?!\1|{indentChar}*$)|(?=@-}))/
  patterns: include 'liquid-patterns'
  repository: _.extend (require 'clone')(include 'repository'),
    type_signature_hs: (include 'repository').type_signature
    type_signature:
      patterns: [
        { include: '#liquid_id' }
        { include: '#liquid_type' }
        { include: '#type_signature_hs' }
      ]
    liquid_id:
      match: /{functionName}\s*:/
      captures:
        0: patterns: [ include: '#identifier' ]
    liquid_type:
      begin: /\{/
      end: /\}/
      name: 'liquid.type.haskell'
      patterns: [
        {
          match: /\G(.*?)\|/
          captures: 1: patterns: [include: '#type_signature']
        }
        { include: '#haskell_expr' }
      ]
