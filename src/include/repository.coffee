{list, listMaybe, concat} = require './util'

prelude = require './prelude'

pragmas = [
  'LANGUAGE'
  'OPTIONS_GHC'
  'INCLUDE'
  'WARNING'
  'DEPRECATED'
  'INLINE'
  'NOINLINE'
  'ANN'
  'LINE'
  'RULES'
  'SPECIALIZE'
  'UNPACK'
  'SOURCE'
]

module.exports=
  block_comment:
    patterns: [
        name: 'comment.block.haddock.haskell'
        begin: /\{-\s*[|^]/
        end: /-\}/
        applyEndPatternLast: 1
        beginCaptures:
          0: name: 'punctuation.definition.comment.haddock.haskell'
        endCaptures:
          0: name: 'punctuation.definition.comment.haddock.haskell'
        patterns: [
            include: '#block_comment'
        ]
      ,
        name: 'comment.block.haskell'
        begin: /\{-(?!#)/
        end: /-\}/
        applyEndPatternLast: 1
        beginCaptures:
          0: name: 'punctuation.definition.comment.haskell'
        patterns: [
            include: '#block_comment'
        ]
    ]
  comments:
    patterns: [
        begin: /({maybeBirdTrack}[ \t]+)?(?=--+\s+[|^])/
        end: /(?!\G)/
        beginCaptures:
          1: name: 'punctuation.whitespace.comment.leading.haskell'
        patterns: [
            name: 'comment.line.double-dash.haddock.haskell'
            begin: /(--+)\s+([|^])/
            end: /\n/
            beginCaptures:
              1: name: 'punctuation.definition.comment.haskell'
              2: name: 'punctuation.definition.comment.haddock.haskell'
        ]
      ,
        ###
        Operators may begin with -- as long as they are not
        entirely composed of - characters. This means comments can't be
        immediately followed by an allowable operator character.
        ###
        begin: /({maybeBirdTrack}[ \t]+)?(?=--+(?!{operatorChar}))/
        end: /(?!\G)/
        beginCaptures:
          1: name: 'punctuation.whitespace.comment.leading.haskell'
        patterns: [
            name: 'comment.line.double-dash.haskell'
            begin: /--/
            end: /\n/
            beginCaptures:
              0: name: 'punctuation.definition.comment.haskell'
        ]
      ,
        include: '#block_comment'
    ]
  characters:
    patterns: [
        {match: '{escapeChar}', name: 'constant.character.escape.haskell'}
        {match: '{octalChar}', name: 'constant.character.escape.octal.haskell'}
        {match: '{hexChar}', name: 'constant.character.escape.hexadecimal.haskell'}
        {match: '{controlChar}', name: 'constant.character.escape.control.haskell'}
      ]
  infix_op:
    name: 'entity.name.function.infix.haskell'
    match: /{operatorFun}/
  module_exports:
    name: 'meta.declaration.exports.haskell'
    begin: /\(/
    end: /\)/
    patterns: [
        include: '#comments'
      ,
        name: 'entity.name.function.haskell'
        match: /{lb}{functionName}{rb}/
      ,
        include: '#type_name'
      ,
        include: '#comma'
      ,
        include: '#infix_op'
      ,
        name: 'meta.other.constructor-list.haskell'
        begin: /\(/
        end: /\)/
        patterns: [
          include: '#type_ctor'
        ]
    ]
  module_name:
    name: 'support.other.module.haskell'
    match: /{lb}{className}{rb}/
  module_name_prefix:
    name: 'support.other.module.haskell'
    match: /{lb}{className}\./
  pragma:
    name: 'meta.preprocessor.haskell'
    begin: /\{-#/
    end: /#-\}/
    patterns: [
        match: "{lb}(#{pragmas.join('|')}){rb}"
        name: 'keyword.other.preprocessor.haskell'
    ]
  function_type_declaration:
    name: 'meta.function.type-declaration.haskell'
    begin: concat /{indentBlockStart}/, /{functionTypeDeclaration}/
    end: /{indentBlockEnd}/
    contentName: 'meta.type-signature.haskell'
    beginCaptures:
      2:
        patterns: [
            name: 'entity.name.function.haskell'
            match: /{functionName}/
          ,
            include: '#infix_op'
        ]
      3: name: 'keyword.other.double-colon.haskell'
    patterns: [
        include: '#type_signature'
    ]
  ctor_type_declaration:
    name: 'meta.ctor.type-declaration.haskell'
    begin: concat /{indentBlockStart}/, /{ctorTypeDeclaration}/
    end: /{indentBlockEnd}/
    contentName: 'meta.type-signature.haskell'
    beginCaptures:
      2:
        patterns: [
            name: 'entity.name.tag.haskell'
            match: /{className}/
          ,
            include: '#infix_op'
        ]
      3: name: 'keyword.other.double-colon.haskell'
    patterns: [
        include: '#type_signature'
    ]
  record_field_declaration:
    name: 'meta.record-field.type-declaration.haskell'
    begin: /{lb}{functionTypeDeclaration}/
    end: /(?={functionTypeDeclaration}|})/
    contentName: 'meta.type-signature.haskell'
    beginCaptures:
      1:
        patterns: [
            name: 'entity.other.attribute-name.haskell'
            match: /{lb}{functionName}{rb}/
          ,
            include: '#infix_op'
        ]
      2: name: 'keyword.other.double-colon.haskell'
    patterns: [
        include: '#type_signature'
    ]
  type_signature:
    patterns: [
        name: 'meta.class-constraints.haskell'
        match: concat /\(/,
          list('classConstraints', /{classConstraint}/, /,/),
          /\)/, /\s*(=>|⇒)/
        captures:
          1: patterns: [{include: '#class_constraint'}]
          #2,3 are from classConstraint
          4: name: 'keyword.other.big-arrow.haskell'
      ,
        name: 'meta.class-constraints.haskell'
        match: /({classConstraint})\s*(=>|⇒)/
        captures:
          1: patterns: [{include: '#class_constraint'}]
          #2,3 are from classConstraint
          4: name: 'keyword.other.big-arrow.haskell'
      ,
        include: '#pragma'
      ,
        name: 'keyword.other.arrow.haskell'
        match: /->|→/
      ,
        name: 'keyword.other.big-arrow.haskell'
        match: /=>|⇒/
      ,
        name: 'support.class.prelude.haskell'
        match: "{lb}(#{prelude.types.join('|')}){rb}"
      ,
        include: '#generic_type'
      ,
        include: '#type_name'
      ,
        include: '#unit'
      ,
        include: '#comments'
    ]
  unit:
    name: 'constant.language.unit.haskell'
    match: /\(\)/
  empty_list:
    name: 'constant.language.empty-list.haskell'
    match: /\[\]/
  generic_type:
    name: 'variable.other.generic-type.haskell'
    match: /{lb}{functionName}{rb}/
  class_constraint:
    name: 'meta.class-constraint.haskell'
    match: /{classConstraint}/
    captures:
      1: patterns: [
        name: 'entity.other.inherited-class.haskell'
        match: /{lb}{className}{rb}/
      ]
      2: patterns: [
          include: '#type_name'
        ,
          include: '#generic_type'
      ]
  deriving:
    patterns: [
        include: '#deriving_list'
      ,
        include: '#deriving_simple'
      ,
        include: '#deriving_keyword'
    ]
  deriving_keyword:
    name: 'meta.deriving.haskell'
    match: /{lb}(deriving){rb}/
    captures:
      1: name: 'keyword.other.haskell'
  deriving_list:
    name: 'meta.deriving.haskell'
    begin: /{lb}(deriving)\s*\(/
    end: /\)/
    beginCaptures:
      1: name: 'keyword.other.haskell'
    patterns: [
        match: /{lb}({className}){rb}/
        captures:
          1: name: 'entity.other.inherited-class.haskell'
    ]
  deriving_simple:
    name: 'meta.deriving.haskell'
    match: /{lb}(deriving)\s*({className}){rb}/
    captures:
      1: name: 'keyword.other.haskell'
      2: name: 'entity.other.inherited-class.haskell'
  infix_function:
    name: 'keyword.operator.function.infix.haskell'
    match: /(`){functionName}(`)/
    captures:
      1: name: 'punctuation.definition.entity.haskell'
      2: name: 'punctuation.definition.entity.haskell'
  quasi_quotes:
    begin: /(\[)({functionNameOne})(\|)/
    end: /(\|)(\])/
    beginCaptures:
      1: name: 'punctuation.definition.quasiquotes.begin.haskell'
      2: name: 'entity.name.tag.haskell'
    endCaptures:
      2: name: 'punctuation.definition.quasiquotes.end.haskell'
    contentName: 'string.quoted.quasiquotes.haskell'
  module_decl:
    name: 'meta.declaration.module.haskell'
    begin: /{indentBlockStart}(module){rb}/
    end: /{lb}(where){rb}/
    beginCaptures:
      2: name: 'keyword.other.haskell'
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
  class_decl:
    name: 'meta.declaration.class.haskell'
    begin: /{indentBlockStart}(class){rb}/
    end: /{lb}(where){rb}|$/
    beginCaptures:
      2: name: 'storage.type.class.haskell'
    endCaptures:
      1: name: 'keyword.other.haskell'
    patterns: [
        include: '#type_signature'
    ]
  instance_decl:
    name: 'meta.declaration.instance.haskell'
    begin: /{indentBlockStart}(instance){rb}/
    end: /{lb}(where){rb}|$/
    contentName: 'meta.type-signature.haskell'
    beginCaptures:
      2: name: 'keyword.other.haskell'
    endCaptures:
      1: name: 'keyword.other.haskell'
    patterns: [
        include: '#type_signature'
    ]
  foreign_import:
    name: 'meta.foreign.haskell'
    begin: /{indentBlockStart}(foreign)\s+(import|export){rb}/
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
  regular_import:
    name: 'meta.import.haskell'
    begin: /{indentBlockStart}(import){rb}/
    end: /{indentBlockEnd}/
    beginCaptures:
      2: name: 'keyword.other.haskell'
    patterns: [
        include: '#module_name'
      ,
        include: '#module_exports'
      ,
        match: /{lb}(qualified|as|hiding){rb}/
        captures:
          1: name: 'keyword.other.haskell'
    ]
  data_decl:
    name: 'meta.declaration.type.data.haskell'
    begin: /{indentBlockStart}(data|newtype)\s+((?:(?!=|where).)*)/
    end: /{indentBlockEnd}/
    beginCaptures:
      2: name: 'storage.type.data.haskell'
      3:
        name: 'meta.type-signature.haskell'
        patterns: [include: '#type_signature']
    patterns: [
        include: '#comments'
      ,
        match: '{lb}where{rb}'
        name: 'keyword.other.haskell'
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
            include: '#comma'
          ,
            include: '#record_field_declaration'
        ]
      ,
        include: '#ctor_type_declaration' #GADT
    ]
  type_alias:
    name: 'meta.declaration.type.type.haskell'
    begin: /{indentBlockStart}(type(?:\s+(?:family|instance))?)\s+({typeDecl})/
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
  keywords: [
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
  ]
  c_preprocessor:
    name: 'meta.preprocessor.c'
    begin: /{maybeBirdTrack}(?=#)/
    end: '(?<!\\\\)(?=\\n)'
    patterns: [
      include: 'source.c'
    ]
  string:
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
  newline_escape:
    name: 'markup.other.escape.newline.haskell'
    match: /\\$/
  quoted_character:
    name: 'string.quoted.single.haskell'
    match: /(')({character})(')/
    captures:
      1: name: 'punctuation.definition.string.begin.haskell'
      2:
        patterns:[
          include: '#characters'
        ]
      3: name: 'punctuation.definition.string.end.haskell'
  scoped_type: [
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
  ]
  prelude:[
    name: 'support.tag.haskell'
    match: "{lb}(#{prelude.constr.join('|')}){rb}"
  ,
    name: 'support.function.prelude.haskell'
    match: "{lb}(#{prelude.funct.join('|')}){rb}"
  ]
  comma:
    name: 'punctuation.separator.comma.haskell'
    match: /,/
  lit_num: [
    name: 'constant.numeric.hexadecimal.haskell'
    match: '0[xX][0-9a-fA-F]+'
  ,
    name: 'constant.numeric.octal.haskell'
    match: '0[oO][0-7]+'
  ,
    name: 'constant.numeric.float.haskell'
    match: '[0-9]+(\\.|[eE][+-]?|\\.[0-9]+[eE][+-]?)[0-9]+'
  ,
    name: 'constant.numeric.decimal.haskell'
    match: '[0-9]+'
  ]
  operator:
    name: 'keyword.operator.haskell'
    match: /{operator}/
  identifier:
    match: '{lb}{functionName}{rb}'
    name: 'identifier.haskell'
    captures: 0: patterns: [ include: '#module_name_prefix' ]
  type_name:
    name: 'entity.name.type.haskell'
    match: /{lb}{className}{rb}/
    captures: 0: patterns: [ include: '#module_name_prefix' ]
  type_ctor:
    name: 'entity.name.tag.haskell'
    match: /{lb}{className}{rb}/
    captures: 0: patterns: [ include: '#module_name_prefix' ]
