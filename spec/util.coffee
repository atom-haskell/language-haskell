_ = require 'underscore-plus'

zip = ->
  lengthArray = (arr.length for arr in arguments)
  length = Math.max(lengthArray...)
  for i in [0...length]
    arr[i] for arr in arguments

module.exports =
  grammarExpect: (grammar, str) ->
    tkzd = grammar.tokenizeLines(str)
    expect(tkzd)

  customMatchers:
    toHaveTokens: (expected) ->
      for [a, e] in zip(@actual, expected)
        ts = a.map ({value}) -> value
        unless (_.isEqual(ts, e))
          # console.log @message
          @message = -> "Expected #{JSON.stringify(ts)} to equal #{JSON.stringify(e)}"
          return false
      return true
    toHaveTokenScopes: (expected) ->
      for [a, e] in zip(@actual, expected)
        e = [].concat (e.map (val) ->
          if typeof(val) is 'string'
            return [[val]]
          else if val?.length?
            return [val]
          else
            for k, v of val
              [k, v]
          )...
        ts = a.map ({value}) -> value
        es = e.map ([tok, scopes]) -> tok
        for [ta, te] in zip(ts, es) when ta isnt te
          @message = -> "Expected '#{ta}' to equal '#{te}' : #{JSON.stringify(ts)} ; #{JSON.stringify(es)}"
          return false
        for [{value, scopes}, [evalue, escopes]] in zip(a, e)
          unless value is evalue
            @message = -> "Expected \"#{value}\" to equal \"#{evalue}\""
            return false
          if escopes?
            for escope in escopes
              unless _.contains(scopes, escope)
                @message = -> "Expected \"#{JSON.stringify(scopes)}\" to contain \"#{escope}\""
                return false
      return true
    toHaveScopes: (expected) ->
      zip(@actual, expected).every ([a, e]) ->
        a.every ({scopes}) ->
          e.every (s) -> _.contains(scopes, s)
    tokenToHaveScopes: (expected) ->
      for [a, e] in zip(@actual, expected)
        for [i, s] in e
          for sc in s
            unless _.contains(a[i].scopes, sc)
              @message = -> "Expected #{JSON.stringify(a[i])} to have scope #{sc} from #{JSON.stringify(s)}"
              return false
      return true
