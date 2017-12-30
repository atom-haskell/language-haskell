{list, listMaybe, concat} = require './util'

module.exports=
  identStartCharClass: /[\p{Ll}_\p{Lu}\p{Lt}]/
  identContCharClass: /[\p{Ll}_\p{Lu}\p{Lt}']/
  identCharClass: /[\p{Ll}_\p{Lu}\p{Lt}\p{Nd}']/
  functionNameOne: /[\p{Ll}_]{identCharClass}*/
  classNameOne: /[\p{Lu}\p{Lt}]{identCharClass}*/
  functionName: /(?:{className}\.)?{functionNameOne}/
  className: /{classNameOne}(?:\.{classNameOne})*/
  operatorChar: '(?:[\\p{S}\\p{P}](?<![(),;\\[\\]`{}_"\']))'
  ###
  In case this regex seems overly general, note that Haskell
  permits the definition of new operators which can be nearly any string
  of punctuation characters, such as $%^&*.
  ###
  operator: /(?:{lb}{className}\.)?{operatorChar}+/
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
  controlChar: /(?:\\\^[A-Z@\[\]\\^_])/
  character: '(?:{basicChar}|{escapeChar}|{octalChar}|{hexChar}|{controlChar}|{operatorChar})'
  functionList: list(/{functionName}|{operatorFun}/, /\s*,\s*/)
  functionTypeDeclaration:
    concat /{functionList}\s*({doubleColonOperator})/
  doubleColonOperator: '(?<!{operatorChar})(?:::|∷)(?!{operatorChar})'
  ctorTypeDeclaration:
    concat list(/{className}|{operatorFun}/, /\s*,\s*/),
      /\s*({doubleColonOperator})/
  ctorArgs: ///
    (?!deriving)
    (?:
    {className}     #proper type
    |{functionName} #type variable
    |(?:(?!deriving)(?:[\w()'→⇒\[\],]|->|=>)+\s*)+ #anything goes!
    )
    ///
  ctor: concat /{lb}({className})\s*/, listMaybe(/{ctorArgs}/, /\s+/)
  typeDeclOne: /(?:(?!{lb}where{rb})(?:{className}|{functionName}))/
  typeDecl: '(?>(?:{typeDeclOne})(?:\\s+{typeDeclOne})*)'
  indentChar: /[ \t]/
  indentBlockStart: '{maybeBirdTrack}({indentChar}*)'
  indentBlockEnd: /^(?!\1{indentChar}|{indentChar}*$)/
  indentBlockCont: /^(?!\1|{indentChar}*$)/
  maybeBirdTrack: /^/
  lb: '(?:(?={identStartCharClass})(?<!{identContCharClass}))'
  lbrel: '(?:(?={identContCharClass})(?<!{identContCharClass}))'
  rb: '(?:(?<={identCharClass})(?!{identCharClass}))'
  b: '(?:{lb}|{rb})'
