{list, listMaybe, concat, include, makeGrammar} = require '../syntax-tools'

prelude = require './prelude'

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
        {match: 'controlChar', name: 'constant.character.escape.control.haskell'}
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
        name: 'punctuation.separator.comma.haskell'
        match: /,/
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
    match: /(?:{className}\.)*{className}\.?/
  pragma:
    name: 'meta.preprocessor.haskell'
    begin: /\{-#/
    end: /#-\}/
    patterns: [
        match: /{lb}(LANGUAGE|OPTIONS_GHC|INCLUDE|WARNING|DEPRECATED|INLINE|NOINLINE|ANN|LINE|RULES|SPECIALIZE|UNPACK|SOURCE){rb}/
        name: 'keyword.other.preprocessor.haskell'
    ]
  function_type_declaration:
    name: 'meta.function.type-declaration.haskell'
    begin: concat /{maybeBirdTrack}(\s*)/, /{functionTypeDeclaration}/
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
    begin: concat /{maybeBirdTrack}(\s*)/, /{ctorTypeDeclaration}/
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
  type_name:
    name: 'entity.name.type.haskell'
    match: /{lb}{className}{rb}/
  type_ctor:
    name: 'entity.name.tag.haskell'
    match: /{lb}{className}{rb}/
  unit:
    name: 'constant.language.unit.haskell'
    match: /\(\)/
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
