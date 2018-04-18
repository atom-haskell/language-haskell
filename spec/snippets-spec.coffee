CSON = require 'season'

defs = CSON.readFileSync("#{__dirname}/../snippets/language-haskell.cson")

open = (what) ->
  atom.workspace.open("#{__dirname}/fixture/#{what}")

describe "Snippets", ->
  [editorElement, editor, Snippets] = []

  expandSnippet = (editor) ->
    atom.commands.dispatch(atom.views.getView(editor), 'snippets:expand')

  sanitize = (body) ->
    parser = Snippets.getBodyParser()
    flatten = (obj) ->
      if typeof(obj) is "string"
        return obj
      else
        return obj.content.map(flatten).join('')
    parsed =
      parser.parse(body)
      .map(flatten)
      .join('')
      .replace /\t/g, ' '.repeat(editor.getTabLength())
    return parsed

  universalTests = ->
    it 'triggers snippets', ->
      expect((for name, {prefix, body} of defs['.source .haskell']
        editor.setText("")
        editor.insertText(prefix)
        expandSnippet(editor)
        expect(editor.getText().trim()).toBe sanitize(body).trim()
      ).length).toBeGreaterThan 0
    it 'triggers non-comment snippets', ->
      expect((for name, {prefix, body} of defs['.source .haskell:not(.comment)']
        editor.setText("")
        editor.insertText(prefix)
        expandSnippet(editor)
        expect(editor.getText().trim()).toBe sanitize(body).trim()
      ).length).toBeGreaterThan 0
    it 'triggers comment snippets', ->
      expect((for name, {prefix, body} of defs['.source .haskell.comment']
        editor.setText("")
        editor.insertText("-- #{prefix}")
        expandSnippet(editor)
        expect(editor.getText().trim()).toBe "-- #{sanitize(body).trim()}"
      ).length).toBeGreaterThan 0
    it 'triggers empty-list snippets', ->
      expect((for name, {prefix, body} of defs['.source .haskell.constant.language.empty-list']
        editor.setText("")
        editor.insertText("#{prefix}]")
        editor.getLastCursor().moveLeft()
        expandSnippet(editor)
        expect(editor.getText().trim()).toBe "#{sanitize(body).trim()}]"
      ).length).toBeGreaterThan 0
    it 'triggers type snippets', ->
      expect((for name, {prefix, body} of defs['.source .haskell.meta.type']
        editor.setText("")
        editor.insertText("data Data = Constr #{prefix}")
        expandSnippet(editor)
        expect(editor.getText().trim()).toBe "data Data = Constr #{sanitize(body).trim()}"
      ).length).toBeGreaterThan 0

  beforeEach ->
    waitsForPromise ->
      atom.packages.activatePackage("language-haskell")
    waitsForPromise ->
      snippets = atom.packages.getLoadedPackage('snippets') ? \
        atom.packages.loadPackage("#{process.env.HOME}/.atom/packages/snippets")
      snippets.activate()
      .then ->
        Snippets = snippets.mainModule
        new Promise (resolve) ->
          Snippets.onDidLoadSnippets -> resolve()

  describe 'haskell', ->
    beforeEach ->
      waitsForPromise ->
        open('sample.hs')
      runs ->
        editor = atom.workspace.getActiveTextEditor()
        editorElement = atom.views.getView(editor)
    universalTests()
  describe 'c2hs', ->
    beforeEach ->
      waitsForPromise ->
        open('sample.chs')
      runs ->
        editor = atom.workspace.getActiveTextEditor()
        editorElement = atom.views.getView(editor)
    universalTests()
  describe 'hsc2hs', ->
    beforeEach ->
      waitsForPromise ->
        open('sample.hsc')
      runs ->
        editor = atom.workspace.getActiveTextEditor()
        editorElement = atom.views.getView(editor)
    universalTests()

  describe 'cabal', ->
    beforeEach ->
      waitsForPromise ->
        open('sample.cabal')
      runs ->
        editor = atom.workspace.getActiveTextEditor()
        editorElement = atom.views.getView(editor)
    it 'triggers snippets', ->
      expect((for name, {prefix, body} of defs['.source.cabal']
        editor.setText("")
        editor.insertText(prefix)
        expandSnippet(editor)
        expect(editor.getText().trim()).toBe sanitize(body).trim()
      ).length).toBeGreaterThan 0
