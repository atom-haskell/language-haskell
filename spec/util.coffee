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
    tokensToHaveScopes: (expected) ->
      for line in @actual
        for tok in line
          if tok.value of expected
            for sc in expected[tok.value]
              if not _.contains(tok.scopes, sc)
                @message = -> """
                Expected #{JSON.stringify(tok)} to
                have scope #{sc} from #{JSON.stringify(expected[tok.value])}
                """
                return false
      return true
    tokensNotToHaveScopes: (expected) ->
      for line in @actual
        for tok in line
          if tok.value of expected
            for sc in expected[tok.value]
              if _.contains(tok.scopes, sc)
                @message = -> """
                Expected #{JSON.stringify(tok)} to not
                have scope #{sc} from #{JSON.stringify(expected[tok.value])}
                """
                return false
      return true
    toHaveScopes: (expected) ->
      zip(@actual, expected).every ([a, e]) ->
        a.every ({scopes}) ->
          e.every (s) -> _.contains(scopes, s)
    notToHaveScopes: (expected) ->
      not zip(@actual, expected).every ([a, e]) ->
        a.every ({scopes}) ->
          e.every (s) -> _.contains(scopes, s)
    tokenToHaveScopes: (expected) ->
      for [a, e] in zip(@actual, expected)
        for i, s of e
          if not Array.isArray(s) or s.length is 0
            @message = -> "Zero-length assertion in #{i} of #{JSON.stringify(e)}"
            return false
          for sc in s
            unless _.contains(a[i].scopes, sc)
              @message = -> "Expected token #{i} #{JSON.stringify(a[i])} to have scope #{sc} from #{JSON.stringify(s)}"
              return false
      return true
    tokenNotToHaveScopes: (expected) ->
      for [a, e] in zip(@actual, expected)
        for i, s of e
          if not Array.isArray(s) or s.length is 0
            @message = -> "Zero-length assertion in #{i} of #{JSON.stringify(e)}"
            return false
          for sc in s
            if _.contains(a[parseInt(i, 10)].scopes, sc)
              @message = -> """
              Expected token #{i} #{JSON.stringify(a[i])} not to have scope #{sc} from #{JSON.stringify(s)}
              """
              return false
      return true
