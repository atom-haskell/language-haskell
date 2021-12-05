rxToStr = (rx) ->
  if typeof rx is 'object'
    rx.source
  else
    rx

list = (s, sep) ->
  # "(?<#{item}>(?:#{rxToStr s})(?:\\s*(?:#{rxToStr sep})\\s*\\g<#{item}>)?)"
  "((?:#{rxToStr s})(?:(?:#{rxToStr sep})(?:#{rxToStr s}))*)"

listMaybe = (s, sep) ->
  # "(?<#{item}>(?:#{rxToStr s})(?:\\s*(?:#{rxToStr sep})\\s*\\g<#{item}>)?)?"
  "#{list(s, sep)}?"

concat = (list...) ->
  r = ''.concat (list.map (i) -> "(?:#{rxToStr i})")...
  "(?:#{r})"

balanced = (name, left, right) ->
  "(?<#{name}>(?:(?!#{left}|#{right}).|#{left}\\g<#{name}>#{right})*)"

floatPattern = (digit, exp) ->
  exponent = "#{exp}[+-]?[0-9_]+"
  "#{digit}*(?:\\.#{digit}+(?:#{exponent})?|#{exponent})"

guarded = (pattern) ->
  "(?:(?<!{operatorChar})(?:#{pattern})(?!{operatorChar}))"

controlKeywords = [
  'do', 'if', 'then', 'else', 'case', 'of', 'let', 'in', 'default', 'mdo', 'rec', 'proc'
]

otherKeywords = [
  'deriving', 'where', 'data', 'type', 'newtype', 'pattern'
]

module.exports = {list, listMaybe, concat, balanced, guarded, floatPattern, controlKeywords, otherKeywords}
