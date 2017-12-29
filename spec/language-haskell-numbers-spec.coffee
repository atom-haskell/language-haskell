{grammarExpect, customMatchers} = require './util'

describe "Language-Haskell Numbers", ->
  grammar = null

  beforeEach ->
    @addMatchers(customMatchers)
    waitsForPromise ->
      atom.packages.activatePackage("language-haskell")

    runs ->
      grammar = atom.grammars.grammarForScopeName("source.haskell")

  describe "numbers", ->
    testNumbers = (scope, s) ->
      g = grammarExpect grammar, s
      tokens = s.split("\n").map((s) -> [s])
      g.toHaveTokens tokens
      g.toHaveScopes (
        [scope] for i in tokens
      )
    testNumbersNeg = (s, scope) ->
      g = grammarExpect grammar, s
      tokens = s.split("\n").map((s) -> [s])
      g.notToHaveScopes (
        [scope] for i in tokens
      )

    it "parses decimal integers", ->
      testNumbers 'constant.numeric.decimal.haskell', '''
        12345
        123_456
        123_45__6
      '''

    it "parses binary integers", ->
      testNumbers 'constant.numeric.binary.haskell', '''
        0b10010101
        0b1001_0101
        0b100__1_0101
      '''

    it "parses hexadecimal integers", ->
      testNumbers 'constant.numeric.hexadecimal.haskell', '''
        0xfade145
        0xfad_e145
        0xf__ad_e14_5
      '''

    it "parses octal integers", ->
      testNumbers 'constant.numeric.octal.haskell', '''
        0o1234567
        0o1_2_3___4_5_6_7
      '''

    it "does not parse invalid decimal integers", ->
      testNumbersNeg 'constant.numeric.decimal.haskell', '''
        12345a
        123_456a
      '''

    it "does not parse invalid binary integers", ->
      testNumbersNeg 'constant.numeric.binary.haskell', '''
        0b1001010123
        0b100101_0123
      '''

    it "does not parse invalid hexadecimal integers", ->
      testNumbersNeg 'constant.numeric.hexadecimal.haskell', '''
        0xfade145z
        0xfade14_5z
      '''

    it "does not parse invalid octal integers", ->
      testNumbersNeg 'constant.numeric.octal.haskell', '''
        0o12345678
        0o123_45678
      '''

    it "parses floating point numbers", ->
      testNumbers 'constant.numeric.float.haskell', '''
        1.234
        1e23
        1.23e4
        1E23
        1.23E4
        1.2_34
        1e2_3
        1.23e4
        1_2E2_3
        1.2_3E4
      '''

    it "parses hexfloat numbers", ->
      testNumbers 'constant.numeric.hexfloat.haskell', '''
        0x1.234
        0x1p23
        0x1.23p4
        0x1P23
        0x1.23P4
        0xa.Efa
        0xFap23
        0xf.23p4
        0x1P23
        0XaP23
        0X1.23P4
        0xa.E_fa
        0xF_ap2_3
        0xf.2_3p4
        0x1P2_3
        0XaP2_3
        0X1.2_3P4
      '''
