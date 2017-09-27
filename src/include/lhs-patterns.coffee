module.exports =
  [
      begin: /^((\\)begin)({)(code|spec)(})(\s*$)?/
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
      match: '(?<!\\\\verb)\\|((:?[^|]|\\|\\|)+)\\|'
      name: 'meta.embedded.text.haskell.latex'
      captures:
        1: patterns: [include: 'source.haskell']
    ,
      include: 'text.tex.latex'
  ]
