makeGrammar = require 'atom-syntax-tools'

makeGrammar_ = (p,g) -> makeGrammar g,p

toString = (rx) ->
  if rx instanceof RegExp
    rx.source
  else
    rx

list = (item,s,sep) ->
  #recursive regexp, caution advised
  "(?<#{item}>(?:#{toString s})(?:\\s*(?:#{toString sep})\\s*\\g<#{item}>)?)"

listMaybe = (item,s,sep) ->
  #recursive regexp, caution advised
  "(?<#{item}>(?:#{toString s})(?:\\s*(?:#{toString sep})\\s*\\g<#{item}>)?)?"

concat = (list...) ->
  r=''.concat (list.map (i) -> "(?:#{toString i})")...
  "(?:#{r})"

makeGrammar_ "grammars/haskell.cson",
  name: 'Haskell'
  fileTypes: [ 'hs' ]
  scopeName: 'source.haskell'

  macros:
    functionName: /[\p{Ll}_][\p{Ll}_\p{Lu}\p{Lt}\p{Nd}']*/
    className: /[\p{Lu}\p{Lt}][\p{Ll}_\p{Lu}\p{Lt}\p{Nd}']*/
    operatorChar: /[\p{S}\p{P}&&[^(),;\[\]`{}_"']]/
    ###
    In case this regex seems overly general, note that Haskell
    permits the definition of new operators which can be nearly any string
    of punctuation characters, such as $%^&*.
    ###
    operator: /{operatorChar}+/
    operatorFun: ///
      (?:
        \(
          (?!--+\)) # An operator cannot be composed entirely of `-` characters
          {operator}
        \)
      )
      ///
    character: ///
      (?:
        [\ -\[\]-~]                # Basic Char
        | (\\(?:NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE
          |DC1|DC2|DC3|DC4|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS
          |US|SP|DEL|[abfnrtv\\\"'\&]))    # Escapes
        | (\\o[0-7]+)                # Octal Escapes
        | (\\x[0-9A-Fa-f]+)            # Hexadecimal Escapes
        | (\^[A-Z@\[\]\\\^_])            # Control Chars
      )
      ///
    classConstraint: concat /({className})\s+/,
      list('classConstraint',/{className}|{functionName}/,/\s+/)
    functionTypeDeclaration:
      concat list('functionTypeDeclaration',/{functionName}|{operatorFun}/,/,/),
        /\s*(::|∷)/
    ctorArgs: ///
      (?!deriving)
      (?:
      {className}     #proper type
      |{functionName} #type variable
      |(?:[\w\(\)\s'->→=⇒](?!deriving))+ #anything goes!
      )
      ///
    ctor: concat /({className})\s+/,
      listMaybe('ctorArgs',/{ctorArgs}/,/\s+/)
    typeDecl: /.+?/

  patterns: [
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
      name: 'meta.declaration.module.haskell'
      begin: /\b(module)\b/
      end: /(where)/
      beginCaptures:
        1: name: 'keyword.other.haskell'
      endCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          include: '#module_name'
        ,
          include: '#module_exports'
        ,
          name: 'invalid'
          match: /[a-z]+/
      ]
    ,
      name: 'meta.declaration.class.haskell'
      begin: /\b(class)\b/
      end: /\b(where)\b|$/
      beginCaptures:
        1: name: 'storage.type.class.haskell'
      endCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          name: 'support.class.prelude.haskell'
          match: ///
            \b
            (Monad
            |Functor
            |Eq
            |Ord
            |Read
            |Show
            |Num
            |(Frac|Ra)tional
            |Enum
            |Bounded
            |Real(Frac|Float)?
            |Integral
            |Floating
            )\b
            ///
        ,
          include: '#type_name'
        ,
          include: '#generic_type'
      ]
    ,
      name: 'meta.declaration.instance.haskell'
      begin: /\b(instance)\b/
      end: /\b(where)\b|$/
      contentName: 'meta.type-signature.haskell'
      beginCaptures:
        1: name: 'keyword.other.haskell'
      endCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          include: '#type_signature'
      ]
    ,
      name: 'meta.import.haskell'
      begin: /\b(import)\b/
      end: /($|;|(?=--))/
      beginCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          match: /(qualified|as|hiding)/
          name: 'keyword.other.haskell'
        ,
          include: '#module_name'
        ,
          include: '#module_exports'
      ]
    ,
      name: 'meta.declaration.type.data.record.haskell'
      begin: /^(\s)*(data|newtype)\s+({className})\s*(=)\s*({className})\s+(\{)/
      end: /^\1(?![ \t])/
      beginCaptures:
        2: name: 'storage.type.data.haskell'
        3: patterns: [include: '#type_name']
        4: name: 'keyword.operator.assignment.haskell'
        5: patterns: [include: '#type_ctor']
        6: name: 'keyword.operator.record.begin.haskell'
      patterns: [
          include: '#comments'
        ,
          include: '#deriving'
        ,
          name: 'punctuation.separator.comma.haskell'
          match: /,/
        ,
          include: '#record_field_declaration'
        ,
          name: 'keyword.operator.record.end.haskell'
          match: '\}'
      ]
    ,
      name: 'meta.declaration.type.data.haskell'
      begin: /^(\s)*(data)\s+({typeDecl})\s*(=)\s*/
      end: /^\1(?![ \t])/
      beginCaptures:
        2: name: 'storage.type.data.haskell'
        3:
          name: 'meta.type-signature.haskell'
          patterns: [include: '#type_signature']
        4: name: 'keyword.operator.assignment.haskell'
      patterns: [
          include: '#comments'
        ,
          include: '#deriving'
        ,
          match: list 'ctorVariants',/{ctor}/,/\|/
          captures:
            1: patterns: [
              match: /{ctor}/
              captures:
                1: patterns: [include: '#type_ctor']
                2:
                  name: 'meta.type-signature.haskell'
                  patterns: [include: '#type_signature']
            ]
      ]
    ,
      name: 'meta.declaration.type.newtype.haskell'
      begin: /^(\s)*(newtype)\s+({typeDecl})\s*(=)\s*/
      end: /^\1(?![ \t])/
      beginCaptures:
        2: name: 'storage.type.data.haskell'
        3:
          name: 'meta.type-signature.haskell'
          patterns: [include: '#type_signature']
        4: name: 'keyword.operator.assignment.haskell'
      patterns: [
          include: '#comments'
        ,
          include: '#deriving'
        ,
          match: /({className})\s+({ctorArgs})/,
          captures:
            1: patterns: [include: '#type_ctor']
            2:
              name: 'meta.type-signature.haskell'
              patterns: [include: '#type_signature']
      ]
    ,
      name: 'meta.declaration.type.type.haskell'
      begin: /^(\s)*(type)\s+({typeDecl})\s*(=)\s*/
      end: /^\1(?![ \t])/
      contentName: 'meta.type-signature.haskell'
      beginCaptures:
        2: name: 'storage.type.data.haskell'
        3:
          name: 'meta.type-signature.haskell'
          patterns: [include: '#type_signature']
        4: name: 'keyword.operator.assignment.haskell'
      patterns: [
          include: '#comments'
        ,
          include: '#deriving'
        ,
          include: '#type_signature'
      ]
    ,
      name: 'keyword.other.haskell'
      match: /\b(deriving|where|data|type|newtype)\b/
    ,
      name: 'storage.type.haskell'
      match: /\b(data|type|newtype)\b/
    ,
      name: 'keyword.operator.haskell'
      match: /\binfix[lr]?\b/
    ,
      name: 'keyword.control.haskell'
      match: /\b(do|if|then|else|case|of|let|in|default)\b/
    ,
      name: 'constant.numeric.float.haskell'
      match: /\b([0-9]+\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+)\b/
      # Floats are always decimal
    ,
      name: 'constant.numeric.haskell'
      match: /\b([0-9]+|0([xX][0-9a-fA-F]+|[oO][0-7]+))\b/
    ,
      name: 'meta.preprocessor.c'
      match: /^\s*(#)\s*\w+/
      captures:
        1: name: 'punctuation.definition.preprocessor.c'
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
        # {character} macro has 4 capture groups, here 3-6
        7: name: 'punctuation.definition.string.end.haskell'
    ,
      include: '#function_type_declaration'
    ,
      match: /\b(Just|Left|Right|Nothing|True|False|LT|EQ|GT)\b/
      name: 'support.tag.haskell'
    ,
      include: '#type_ctor'
    ,
      include: '#comments'
    ,
      name: 'support.function.prelude.haskell'
      match: ///
        \b(abs|acos|acosh|all|and|any|appendFile|applyM|asTypeOf|asin|asinh
        |atan|atan2|atanh|break|catch|ceiling|compare|concat|concatMap|const
        |cos|cosh|curry|cycle|decodeFloat|div|divMod|drop|dropWhile|elem
        |encodeFloat|enumFrom|enumFromThen|enumFromThenTo|enumFromTo
        |error|even|exp|exponent|fail|filter|flip|floatDigits|floatRadix
        |floatRange|floor|fmap|foldl|foldl1|foldr|foldr1|fromEnum|fromInteger
        |fromIntegral|fromRational|fst|gcd|getChar|getContents|getLine|head
        |id|init|interact|ioError|isDenormalized|isIEEE|isInfinite|isNaN
        |isNegativeZero|iterate|last|lcm|length|lex|lines|log|logBase|lookup
        |map|mapM|mapM_|max|maxBound|maximum|maybe|min|minBound|minimum|mod
        |negate|not|notElem|null|odd|or|otherwise|pi|pred|print|product
        |properFraction|putChar|putStr|putStrLn|quot|quotRem|read|readFile
        |readIO|readList|readLn|readParen|reads|readsPrec|realToFrac|recip
        |rem|repeat|replicate|return|reverse|round|scaleFloat|scanl|scanl1
        |scanr|scanr1|seq|sequence|sequence_|show|showChar|showList|showParen
        |showString|shows|showsPrec|significand|signum|sin|sinh|snd|span
        |splitAt|sqrt|subtract|succ|sum|tail|take|takeWhile|tan|tanh|toEnum
        |toInteger|toRational|truncate|uncurry|undefined|unlines|until
        |unwords|unzip|unzip3|userError|words|writeFile|zip|zip3
        |zipWith|zipWith3)\b
        ///
    ,
      include: '#infix_op'
    ,
      name: 'keyword.operator.haskell'
      match: /{operator}/
    ,
      name: 'punctuation.separator.comma.haskell'
      match: /,/
  ]
  repository:
    block_comment:
      name: 'comment.block.haskell'
      begin: /\{-(?!#)/
      end: /-\}/
      applyEndPatternLast: 1
      captures:
        0: name: 'punctuation.definition.comment.haskell'
      patterns: [
          include: '#block_comment'
      ]
    comments:
      patterns: [
          ###
          Operators may begin with -- as long as they are not
          entirely composed of - characters. This means comments can't be
          immediately followed by an allowable operator character.
          ###
          begin: /(^[ \t]+)?(?=--+(?!{operatorChar}))/
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
      match: /{character}/
      captures:
        1: name: 'constant.character.escape.haskell'
        2: name: 'constant.character.escape.octal.haskell'
        3: name: 'constant.character.escape.hexadecimal.haskell'
        4: name: 'constant.character.escape.control.haskell'
    infix_op:
      name: 'entity.name.function.infix.haskell'
      match: /{operatorFun}/
    module_exports:
      name: 'meta.declaration.exports.haskell'
      begin: /\(/
      end: /\)/
      patterns: [
          name: 'entity.name.function.haskell'
          match: /\b{functionName}/
        ,
          include: '#type_name'
        ,
          name: 'punctuation.separator.comma.haskell'
          match: /,/
        ,
          include: '#infix_op'
        ,
          name: 'meta.other.constructor-list.haskell'
          match: /\(.*?\)/
      ]
    module_name:
      name: 'support.other.module.haskell'
      match: /(?:{className}\.)*{className}/
    pragma:
      name: 'meta.preprocessor.haskell'
      begin: /\{-#/
      end: /#-\}/
      patterns: [
          match: /\b(LANGUAGE|UNPACK|INLINE)\b/
          name: 'keyword.other.preprocessor.haskell'
      ]
    function_type_declaration:
      name: 'meta.function.type-declaration.haskell'
      begin: concat /^(\s*)/,/{functionTypeDeclaration}/
      end: /^\1(?![ \t])/
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
    record_field_declaration:
      name: 'meta.record-field.type-declaration.haskell'
      begin: /{functionTypeDeclaration}/
      end: /,|$\n?/
      contentName: 'meta.type-signature.haskell'
      beginCaptures:
        1:
          patterns: [
              name: 'entity.other.attribute-name.haskell'
              match: /{functionName}/
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
            list('classConstraints',/{classConstraint}/,/,/),
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
          match: ///\b
            (Int(eger)?
            |Maybe
            |Either
            |Bool
            |Float
            |Double
            |Char
            |String
            |Ordering
            |ShowS
            |ReadS
            |FilePath
            |IO(Error)?
            )\b
            ///
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
      match: /\b{className}\b/
    type_ctor:
      name: 'entity.name.tag.haskell'
      match: /\b{className}\b/
    unit:
      name: 'constant.language.unit.haskell'
      match: /\(\)/
    generic_type:
      name: 'variable.other.generic-type.haskell'
      match: /\b{functionName}\b/
    class_constraint:
      name: 'meta.class-constraint.haskell'
      match: /{classConstraint}/
      captures:
        1: patterns: [
          name: 'entity.other.inherited-class.haskell'
          match: /\b{className}\b/
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
      match: /(deriving)/
      captures:
        1: name: 'keyword.other.haskell'
    deriving_list:
      name: 'meta.deriving.haskell'
      begin: /(deriving)\s*\(/
      end: /\)/
      beginCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          match: /\b({className})\b/
          captures:
            1: name: 'entity.other.inherited-class.haskell'
      ]
    deriving_simple:
      name: 'meta.deriving.haskell'
      match: /(deriving)\s*({className})/
      captures:
        1: name: 'keyword.other.haskell'
        2: name: 'entity.other.inherited-class.haskell'
