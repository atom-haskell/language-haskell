module.exports =
  [
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
      patterns: (require './haskell-patterns').concat
        match: /^> /
        name: 'punctuation.definition.bird-track.haskell'
    ,
      begin: '(?<!\\\\verb)\\|'
      end: /\|/
      name: 'meta.embedded.text.haskell.latex'
      patterns: require './haskell-patterns'
    ,
      include: 'text.tex.latex'
  ]
