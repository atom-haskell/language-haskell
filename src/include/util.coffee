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

module.exports = {list, listMaybe, concat}
