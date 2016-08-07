{list, listMaybe, concat} = require './util'

module.exports=
  identStartCharClass: /[\p{Ll}_\p{Lu}\p{Lt}]/
  identCharClass: /[\p{Ll}_\p{Lu}\p{Lt}\p{Nd}']/
  functionNameOne: /[\p{Ll}_]{identCharClass}*/
  classNameOne: /[\p{Lu}\p{Lt}]{identCharClass}*/
  functionName: /(?:{className}\.)?{functionNameOne}/
  className: /{classNameOne}(?:\.{classNameOne})*/
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
  basicChar: /[\ -\[\]-~]/
  escapeChar: ///
    \\(?:NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE
      |DC1|DC2|DC3|DC4|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS
      |US|SP|DEL|[abfnrtv\\\"'\&])    # Escapes
    ///
  octalChar: /(?:\\o[0-7]+)/
  hexChar: /(?:\\x[0-9A-Fa-f]+)/
  controlChar: /(?:\^[A-Z@\[\]\\\^_])/
  character: '(?:{basicChar}|{escapeChar}|{octalChar}|{hexChar}|{controlChar})'
  classConstraint: concat /({className})\s+/,
    list('classConstraint', /{className}|{functionName}/, /\s+/)
  functionTypeDeclaration:
    concat list('functionTypeDeclaration', /{functionName}|{operatorFun}/, /,/),
      /\s*(::|∷)/
  ctorTypeDeclaration:
    concat list('ctorTypeDeclaration', /{className}|{operatorFun}/, /,/),
      /\s*(::|∷)/
  ctorArgs: ///
    (?!deriving)
    (?:
    {className}     #proper type
    |{functionName} #type variable
    |(?:(?!deriving)(?:[\w()'→⇒\[\],]|->|=>)+\s*)+ #anything goes!
    )
    ///
  ctor: concat /{lb}({className}){rb}/,
    listMaybe('ctorArgs', /\s+{ctorArgs}/, '')
  typeDeclOne: /(?:(?!{lb}where{rb})(?:{className}|{functionName}))/
  typeDecl: '(?>(?:{typeDeclOne})(?:\\s+{typeDeclOne})*)'
  indentChar: /[ \t]/
  indentBlockEnd: /^(?!\1{indentChar}|{indentChar}*$)/
  maybeBirdTrack: /^/
  lb: '(?:(?={identStartCharClass})(?<!{identStartCharClass}))'
  rb: '(?:(?<={identCharClass})(?!{identCharClass}))'
  b: '(?:{lb}|{rb})'
