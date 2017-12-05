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

balanced = (name, left, right, inner, ignore = '') ->
  if inner?
    "(?<#{name}>(?:#{inner}|[^#{left}#{right}#{ignore}]|#{left}\\g<#{name}>#{right})*)"
  else
    "(?<#{name}>(?:[^#{left}#{right}#{ignore}]|#{left}\\g<#{name}>#{right})*)"

floatPattern = (digit, exp) ->
  "#{digit}*(?:(?:\\.#{digit}+)?#{exp}[+-]?[0-9_]+|\\.#{digit}+)"


module.exports = {list, listMaybe, concat, balanced, floatPattern}
