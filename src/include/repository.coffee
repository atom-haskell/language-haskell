prelude = require './prelude'
pragmas = require './pragmas'
{ balanced, floatPattern, controlKeywords, otherKeywords } = require './util'

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
        begin: /\{-/
        end: /-\}/
        applyEndPatternLast: 1
        beginCaptures:
          0: name: 'punctuation.definition.comment.block.start.haskell'
        endCaptures:
          0: name: 'punctuation.definition.comment.block.end.haskell'
        patterns: [
            include: '#block_comment'
        ]
    ]
  comments:
    patterns: [
        begin: /({maybeBirdTrack}[ \t]+)?(?=--+\s+[|^])/
        end: /(?!\G)/
        patterns: [
            name: 'comment.line.double-dash.haddock.haskell'
            begin: /(--+)\s+([|^])/
            end: /$/
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
        patterns: [
            name: 'comment.line.double-dash.haskell'
            begin: /--/
            end: /$/
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
  module_exports:
    name: 'meta.declaration.exports.haskell'
    begin: /\(/
    end: /\)/
    applyEndPatternLast: 1
    patterns: [
        include: '#comments'
      ,
        include: '#c_preprocessor'
      ,
        begin: /{lb}(module){rb}/
        end: /{lb}({className}){rb}/
        beginCaptures:
          1: name: 'keyword.other.haskell'
        endCaptures:
          1: name: 'support.other.module.haskell'
        patterns: [
            {include: '#invalid'}
        ]
      ,
        include: '#function_name'
      ,
        include: '#type_name'
      ,
        include: '#comma'
      ,
        name: 'meta.other.constructor-list.haskell'
        begin: /{rb}\s*\(/
        end: /\)/
        patterns: [
          { include: '#comments' }
          { include: '#c_preprocessor' }
          { include: '#type_ctor' }
          { include: '#attribute_name' }
          { include: '#comma' }
          {
            match: /\.\./
            name: 'keyword.operator.wildcard.haskell'
          }
        ]
      ,
        include: '#infix_op'
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
        match: "{lb}((?i:#{pragmas.join('|')})){rb}"
        name: 'keyword.other.preprocessor.haskell'
    ]
  function_type_declaration:
    name: 'meta.function.type-declaration.haskell'
    begin: /{indentBlockStart}{functionTypeDeclaration}/
    end: '{indentBlockEnd}|(?=(?<!{operatorChar})(<-|=)(?!{operatorChar}))'
    contentName: 'meta.type-signature.haskell'
    beginCaptures:
      2:
        patterns: [
            {include: '#function_name'}
            {include: '#infix_op'}
        ]
      3: name: 'keyword.other.double-colon.haskell'
    patterns: [
        include: '#type_signature'
    ]
  multiline_type_declaration:
    name: 'meta.multiline.type-declaration.haskell'
    begin: /{indentBlockStart}({doubleColonOperator})/
    end: '{indentBlockCont}|(?=(?<!{operatorChar})(<-|=)(?!{operatorChar}))'
    contentName: 'meta.type-signature.haskell'
    beginCaptures:
      2: name: 'keyword.other.double-colon.haskell'
    patterns: [
      {include: '#type_signature'}
    ]
  lazy_function_type_signature:
    name: 'meta.function.type-declaration.haskell'
    begin: /{indentBlockStart}({functionList})\s*$/
    end: /{indentBlockEnd}/
    contentName: 'meta.type-signature.haskell'
    beginCaptures:
      2:
        patterns: [
            {include: '#function_name'}
            {include: '#infix_op'}
        ]
    patterns: [
        {include: '#double_colon_operator'}
        {include: '#type_signature'}
    ]
  double_colon_operator:
    name: 'keyword.other.double-colon.haskell'
    match: '{doubleColonOperator}'
  ctor_type_declaration:
    name: 'meta.ctor.type-declaration.haskell'
    begin: /{indentBlockStart}{ctorTypeDeclaration}/
    end: /{indentBlockEnd}/
    contentName: 'meta.type-signature.haskell'
    beginCaptures:
      2:
        patterns: [
            include: '#type_ctor'
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
            include: '#attribute_name'
          ,
            include: '#infix_op'
        ]
      2: name: 'keyword.other.double-colon.haskell'
    patterns: [
        include: '#type_signature'
    ]
  type_signature:
    patterns: [
      #TODO: Type operators, type-level integers etc
        include: '#pragma'
      ,
        include: '#comments'
      ,
        name: 'keyword.other.forall.haskell'
        match: '{lb}forall{rb}'
      ,
        match: /'(\(\))/
        name: 'constant.language.unit.promoted.haskell'
        captures: 1: patterns: [
          {include: '#unit'}
        ]
      ,
        include: '#unit'
      ,
        match: /'(\[\])/
        name: 'constant.language.empty-list.promoted.haskell'
        captures: 1: patterns: [
          {include: '#empty_list'}
        ]
      ,
        include: '#empty_list'
      ,
        include: '#string'
      ,
        name: 'keyword.other.arrow.haskell'
        match: '(?<!{operatorChar})(->|→)(?!{operatorChar})'
      ,
        name: 'keyword.other.big-arrow.haskell'
        match: '(?<!{operatorChar})(=>|⇒)(?!{operatorChar})'
      ,
        match: "'({operator})"
        name: 'keyword.operator.promoted.haskell'
        captures: 1: patterns: [
          {include: '#operator'}
        ]
      ,
        include: '#operator'
      ,
        name: 'variable.other.generic-type.haskell'
        match: /{lb}{functionName}{rb}/
      ,
        name: 'entity.name.type.promoted.haskell'
        match: /{lbrel}'({className}){rb}/
        captures: 1: patterns: [
          include: '#type_name'
        ]
      ,
        include: '#type_name'
      ,
        include: '#lit_num'
    ]
  unit:
    name: 'constant.language.unit.haskell'
    match: /\(\)/
  empty_list:
    name: 'constant.language.empty-list.haskell'
    match: /\[\]/
  deriving:
    patterns: [
        {include: '#deriving_list'}
        {include: '#deriving_simple'}
        {include: '#deriving_keyword'}
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
    begin: /(\[)({functionName})(\|)/
    end: /(\|)(\])/
    beginCaptures:
      1: name: 'punctuation.definition.quasiquotes.begin.haskell'
      2:
        name: 'entity.name.tag.haskell'
        patterns: { include: '#module_name_prefix' }
    endCaptures:
      2: name: 'punctuation.definition.quasiquotes.end.haskell'
    contentName: 'string.quoted.quasiquotes.haskell'
  module_decl:
    name: 'meta.declaration.module.haskell'
    begin: /{indentBlockStart}(module){rb}/
    end: /{lb}(where){rb}|{indentBlockEnd}/
    beginCaptures:
      2: name: 'keyword.other.haskell'
    endCaptures:
      1: name: 'keyword.other.haskell'
    patterns: [
        {include: '#comments'}
        {include: '#module_name'}
        {include: '#module_exports'}
        {include: '#invalid'}
    ]
  hsig_decl:
    name: 'meta.declaration.module.haskell'
    begin: /{indentBlockStart}(signature){rb}/
    end: /{lb}(where){rb}|{indentBlockEnd}/
    beginCaptures:
      2: name: 'keyword.other.haskell'
    endCaptures:
      1: name: 'keyword.other.haskell'
    patterns: [
        {include: '#comments'}
        {include: '#module_name'}
        {include: '#module_exports'}
        {include: '#invalid'}
    ]
  class_decl:
    name: 'meta.declaration.class.haskell'
    begin: /{indentBlockStart}(class){rb}/
    end: /{lb}(where){rb}|{indentBlockEnd}/
    beginCaptures:
      2: name: 'keyword.other.class.haskell'
    endCaptures:
      1: name: 'keyword.other.haskell'
    patterns: [
        include: '#type_signature'
    ]
  instance_decl:
    name: 'meta.declaration.instance.haskell'
    begin: /{indentBlockStart}(instance){rb}/
    end: /{lb}(where){rb}|{indentBlockEnd}/
    contentName: 'meta.type-signature.haskell'
    beginCaptures:
      2: name: 'keyword.other.haskell'
    endCaptures:
      1: name: 'keyword.other.haskell'
    patterns: [
        {include: '#pragma'}
        {include: '#type_signature'}
    ]
  deriving_instance_decl:
    name: 'meta.declaration.instance.deriving.haskell'
    begin: /{indentBlockStart}(deriving\s+instance){rb}/
    end: /{indentBlockEnd}/
    contentName: 'meta.type-signature.haskell'
    beginCaptures:
      2: name: 'keyword.other.haskell'
    patterns: [
        {include: '#pragma'}
        {include: '#type_signature'}
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
        include: '#function_type_declaration'
      ,
        include: '#haskell_expr'
      ,
        include: '#comments'
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
      ,
        include: '#comments'
    ]
  data_decl:
    name: 'meta.declaration.type.data.haskell'
    begin: /{indentBlockStart}(data|newtype)\s+((?:(?!=|where).)*)/
    end: /{indentBlockEnd}/
    beginCaptures:
      2: name: 'keyword.other.data.haskell'
      3:
        name: 'meta.type-signature.haskell'
        patterns: [
          {include: '#family_and_instance'}
          {include: '#type_signature'}
        ]
    patterns: [
        include: '#comments'
      ,
        include: '#where'
      ,
        include: '#deriving'
      ,
        include: '#assignment_op'
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
            {include: '#comments'}
            {include: '#comma'}
            {include: '#record_field_declaration'}
        ]
      ,
        include: '#ctor_type_declaration' #GADT
    ]
  type_alias:
    name: 'meta.declaration.type.type.haskell'
    begin: /{indentBlockStart}(type){rb}/
    end: /{indentBlockEnd}/
    contentName: 'meta.type-signature.haskell'
    beginCaptures:
      2: name: 'keyword.other.type.haskell'
    patterns: [
        {include: '#comments'}
        {include: '#family_and_instance'}
        {include: '#where'}
        {include: '#assignment_op'}
        {include: '#type_signature'}
    ]
  keywords: [
    name: 'keyword.other.$1.haskell'
    match: "{lb}(#{otherKeywords.join('|')}){rb}"
  ,
    name: 'keyword.operator.$1.haskell'
    match: /{lb}(infix[lr]?){rb}/
  ,
    name: 'keyword.control.$1.haskell'
    match: "{lb}(#{controlKeywords.join('|')}){rb}"
  ]
  c_preprocessor:
    name: 'meta.preprocessor.c'
    begin: /{maybeBirdTrack}(?=#)/
    end: '(?<!\\\\)(?=$)'
    patterns: [
      {
        match: '^#\\S+'
        name: 'keyword.control.c'
      }
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
            {include: '#invalid'}
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
    match: "\\((#{balanced 'paren', '\\(', '\\)'}{doubleColonOperator}#{balanced 'paren2', '\\(', '\\)'})\\)"
    captures:
      1: patterns: [
        include: '#haskell_expr'
      ]
  ,
    match: '({doubleColonOperator})(.*?)(?=(?<!{operatorChar})(<-|=)(?!{operatorChar})|$)'
    captures:
      1: name: 'keyword.other.double-colon.haskell'
      2: {name: 'meta.type-signature.haskell', patterns: [include: '#type_signature']}
  ]
  scoped_type_override:
    match: '{indentBlockStart}{functionTypeDeclaration}(.*)(?<!{operatorChar})(<-|=)(?!{operatorChar})'
    captures:
      2: patterns: [include: '#identifier']
      3: name: 'keyword.other.double-colon.haskell'
      4: {name: 'meta.type-signature.haskell', patterns: [include: '#type_signature']}
      5: patterns: [
          {include: '#assignment_op'}
          {include: '#operator'}
      ]
  comma:
    name: 'punctuation.separator.comma.haskell'
    match: /,/
  lit_num: [
    name: 'constant.numeric.hexfloat.haskell'
    match: "0[xX]#{floatPattern('[0-9a-fA-F_]', '[pP]')}"
  ,
    name: 'constant.numeric.hexadecimal.haskell'
    match: '0[xX][_0-9a-fA-F]+'
  ,
    name: 'constant.numeric.octal.haskell'
    match: '0[oO][_0-7]+'
  ,
    name: 'constant.numeric.binary.haskell'
    match: '0[bB][_01]+'
  ,
    name: 'constant.numeric.float.haskell'
    match: "[0-9]#{floatPattern('[0-9_]', '[eE]')}"
  ,
    name: 'constant.numeric.decimal.haskell'
    match: '[0-9][_0-9]*'
  ]
  operator:
    name: 'keyword.operator.haskell'
    match: /{operator}/
    captures:
      0: patterns: [
        { include: '#module_name_prefix' }
        {
          name: 'support.operator.prelude.haskell'
          match: "^(#{prelude.operators.map((x) -> x.replace(/./g, (y) -> '\\'+y)).join('|')})$"
        }
      ]
  infix_op:
    name: 'entity.name.function.operator.haskell'
    match: /{operatorFun}/
    captures:
      0: patterns: [
        { include: '#module_name_prefix' }
        {
          name: 'support.operator.prelude.haskell'
          match: "^\\((#{prelude.operators.map((x) -> x.replace(/./g, (y) -> '\\'+y)).join('|')})\\)$"
        }
      ]
  identifier:
    match: '{lb}{functionName}{rb}'
    name: 'identifier.haskell'
    captures: 0: patterns: [
      { include: '#module_name_prefix' }
      {
        name: 'support.function.prelude.$1.haskell'
        match: "{lb}(#{prelude.funct.join('|')}){rb}"
      }
    ]
  type_name:
    name: 'entity.name.type.haskell'
    match: /{lb}{className}{rb}/
    captures: 0: patterns: [
      { include: '#module_name_prefix' }
      {
          name: 'entity.other.inherited-class.prelude.$1.haskell'
          match: "{lb}(#{prelude.classes.join('|')}){rb}"
      }
      {
          name: 'support.class.prelude.$1.haskell'
          match: "{lb}(#{prelude.types.join('|')}){rb}"
      }
    ]
  type_ctor:
    name: 'entity.name.tag.haskell'
    match: /{lb}{className}{rb}/
    captures: 0: patterns: [
      { include: '#module_name_prefix' }
      {
        name: 'support.tag.prelude.$1.haskell'
        match: "{lb}(#{prelude.constr.join('|')}){rb}"
      }
    ]
  where:
    match: '{lb}where{rb}'
    name: 'keyword.other.haskell'
  family_and_instance:
    match: '{lb}(family|instance){rb}'
    name: 'keyword.other.haskell'
  invalid:
    match: /\S+/
    name: 'invalid.illegal.character-not-allowed-here.haskell'
  function_name:
    name: 'entity.name.function.haskell'
    match: /{lb}{functionName}{rb}/
  assignment_op:
    match: /=/
    captures:
      0: name: 'keyword.operator.assignment.haskell'
  attribute_name:
    name: 'entity.other.attribute-name.haskell'
    match: /{lb}{functionName}{rb}/
  liquidhaskell_annotation:
    name: 'block.liquidhaskell'
    contentName: 'block.liquidhaskell.annotation'
    begin: '\\{-@(?!#)'
    end: '@-\\}'
    patterns: [
      { include: 'annotation.liquidhaskell.haskell' }
    ]
  shebang:
    name: 'comment.line.shebang.haskell'
    match: '^\\#\\!.*\\brunhaskell\\b.*$'
  haskell_expr: [
    { include: '#infix_function' }
    { include: '#unit' }
    { include: '#empty_list' }
    { include: '#quasi_quotes' }
    { include: '#keywords' }
    { include: '#pragma' }
    { include: '#string' }
    { include: '#newline_escape' }
    { include: '#quoted_character' }
    { include: '#comments' }
    { include: '#infix_op' }
    { include: '#comma' }
    { include: '#lit_num' }
    { include: '#scoped_type' }
    { include: '#operator' }
    { include: '#identifier' }
    { include: '#type_ctor' }
  ]
  common_toplevel: [
    { include: '#class_decl' }
    { include: '#instance_decl' }
    { include: '#deriving_instance_decl' }
    { include: '#foreign_import' }
    { include: '#regular_import' }
    { include: '#data_decl' }
    { include: '#type_alias' }
    { include: '#c_preprocessor' }
  ]
  function_type_declaration_with_scoped_type: [
    { include: '#scoped_type_override' }
    { include: '#function_type_declaration' }
    { include: '#multiline_type_declaration' }
  ]
  haskell_toplevel: [
    { include: '#liquidhaskell_annotation' }
    { include: '#common_toplevel' }
    { include: '#function_type_declaration_with_scoped_type' }
    { include: '#haskell_expr' }
  ]
  hsig_toplevel: [
    { include: '#common_toplevel' }
    { include: '#function_type_declaration' }
    { include: '#lazy_function_type_signature' }
    { include: '#comments' }
  ]
  haskell_source: [
    { include: '#shebang' }
    { include: '#module_decl' }
    { include: '#haskell_toplevel' }
  ]
  hsig_source: [
    { include: '#hsig_decl' }
    { include: '#hsig_toplevel' }
  ]
