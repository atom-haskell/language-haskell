grammarTest = require 'atom-grammar-test'
{sep} = require 'path'

fdescribe 'Fixture-based grammar tests', ->

  beforeEach ->
    waitsForPromise ->
      atom.packages.activatePackage 'language-haskell'

  grammarTest("#{__dirname}#{sep}fixture#{sep}liquidhaskell.hs")
