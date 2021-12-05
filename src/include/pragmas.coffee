_ = require 'underscore-plus'

# Taken from https://github.com/ghc/ghc/blob/master/compiler/parser/Lexer.x#L2673
pragmas = [
  # Line pragma
  'LINE'
  'COLUMN'
  # Header pragmas
  'OPTIONS'
  'OPTIONS_GHC'
  'OPTIONS_HADDOCK'
  'LANGUAGE'
  'INCLUDE'
  # Ignored pragmas
  'OPTIONS_HUGS'
  'OPTIONS_NHC98'
  'OPTIONS_JHC'
  'OPTIONS_YHC'
  'OPTIONS_CATCH'
  'OPTIONS_DERIVE'
  'CFILES'
  'CONTRACT'
  # One-word pragmas
  'RULES'
  'INLINE'
  'INLINABLE'
  'INLINEABLE'
  'NOTINLINE'
  'SPECIALIZE'
  'SOURCE'
  'WARNING'
  'DEPRECATED'
  'SCC'
  'GENERATED'
  'CORE'
  'UNPACK'
  'NOUNPACK'
  'ANN'
  'VECTORIZE'
  'NOVECTORIZE'
  'MINIMAL'
  'OVERLAPS'
  'OVERLAPPABLE'
  'OVERLAPPING'
  'INCOHERENT'
  'CTYPE'
  'COMPLETE'
  # Two word pragmas
  'INLINE CONLIKE'
  'NOTINLINE CONLIKE'
  'SPECIALIZE INLINE'
  'SPECIALIZE NOTINLINE'
  'VECTORIZE SCALAR'
]

# Spelling variants
spell_var = _.invert {
  'NOINLINE': 'NOTINLINE'
  'SPECIALISE': 'SPECIALIZE'
  'VECTORISE': 'VECTORIZE'
  'NOVECTORISE': 'NOVECTORIZE'
  'CONSTRUCTORLIKE': 'CONLIKE'
}

variants = (p) ->
  len = p.split(' ').length
  mask0 = Math.pow(2, len) - 1
  for mask in [0..mask0]
    ws = p.split(' ')
    for w, i in ws
      if mask & Math.pow(2, i)
        ws[i] = spell_var[w] ? w
    ws.join(' ')

for p in pragmas
  pragmas.push variants(p)...

sortf = (a, b) ->
  d = b.length - a.length
  if d isnt 0
    d
  else
    a.localeCompare b

module.exports = _.uniq pragmas.sort sortf
