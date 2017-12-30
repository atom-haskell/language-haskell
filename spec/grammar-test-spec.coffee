grammarTest = require 'atom-grammar-test'
{sep} = require 'path'

describe 'Fixture-based grammar tests', ->

  beforeEach ->
    waitsForPromise ->
      atom.packages.activatePackage 'language-haskell'

  test = (name, file, desc = describe) ->
    desc name, -> grammarTest("#{__dirname}#{sep}fixture#{sep}#{file}")
  ftest = (name, file) -> test(name, file, fdescribe)

  test 'Liquid Haskell', 'general.hs'
  test 'Liquid Haskell', 'liquidhaskell.hs'
