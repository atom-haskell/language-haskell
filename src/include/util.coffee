rxToStr = (rx) ->
  if typeof rx is 'object'
    rx.source
  else
    rx

module.exports =
  list: (item, s, sep) ->
    "(?<#{item}>(?:#{rxToStr s})(?:\\s*(?:#{rxToStr sep})\\s*\\g<#{item}>)?)"

  listMaybe: (item, s, sep) ->
    #recursive regexp, caution advised
    "(?<#{item}>(?:#{rxToStr s})(?:\\s*(?:#{rxToStr sep})\\s*\\g<#{item}>)?)?"

  concat: (list...) ->
    r = ''.concat (list.map (i) -> "(?:#{rxToStr i})")...
    "(?:#{r})"
