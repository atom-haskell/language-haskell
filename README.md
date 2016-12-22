# Haskell support in Atom

Adds syntax highlighting and snippets to Haskell files in Atom.

Grammars:

* Haskell (\*.hs)
* Literate Haskell (\*.lhs)
* Cabal (\*.cabal)

![image](https://cloud.githubusercontent.com/assets/7275622/8120540/f16d7ee6-10a8-11e5-9b9d-223ff05a54c6.png)

Based on [Haskell TextMate bundle](https://github.com/textmate/haskell.tmbundle).

# Auto-indent

If you don't like current auto-indentation settings, you can define your own regexp in `config.cson` (Edit -> Open Your Config), or disable it altogether, e.g.

To disable auto-indent:

```cson
".haskell.source":
  editor:
    increaseIndentPattern: ''
```

Note that regexp expression is using oniguruma for parsing, and it needs to be a string, not a javascript regexp. You'll also have to escape `\`.

By default, `increaseIndentPattern` has the following value:

```cson
".haskell.source":
  editor:
    increaseIndentPattern: '(((=|\\bdo|\\bwhere|\\bthen|\\belse|\\bof)\\s*)|(\\bif(?!.*\\bthen\\b.*\\belse\\b.*).*))$'
```

# Configuring highlighting

Note, you may need to reopen currently opened files (or restart Atom) for your new stylesheet to be applied.

## Module names

`language-haskell` uses `support.other.module.haskell` scope for module names, both in `import` statements and when using qualified identifiers (like `Prelude.foldl`). Your syntax theme might not support this scope. If you want to highlight module names in this case, you can add the following to your stylesheet (Edit → Stylesheet...):

```less
atom-text-editor::shadow, ide-haskell-panel {
  .support.other.module.haskell {
    color: #C0A077; //or whatever color you like
  }
}
```

## Operators and infix function application

`language-haskell` uses `keyword.operator.haskell` scope for operators and `keyword.operator.infix.haskell` for infix function application, e.g.

```haskell
negate `map` [1..10]
```

Not all syntax themes support these scopes (almost none support `keyword.operator.infix` particularly)

If you want to higlight operators and infix function applications you can add the following to your stylesheet (Edit → Stylesheet...):

```less
atom-text-editor::shadow, ide-haskell-panel {
    .keyword.operator.haskell {
      color: #CF8C00; // or whatever color you like
    }
    .keyword.operator.infix.haskell {
      color: #CC77AC; // if you want to highlight infix application differently
    }
}
```

## Special `Prelude` treatment

For historical and other reasons (see #85 for discussion), `Prelude` identifiers (functions, types, etc) are treated slightly differently and, depending on your highlighting theme, can be highlighted differently.

Scopes that are used:

* `support.function.prelude.haskell` for functions and values
* `support.class.prelude.haskell` for types
* `entity.other.inherited-class.prelude.haskell` for typeclasses
* `support.tag.prelude.haskell` for type constructors

If you want `Prelude` identifiers highlighted differently from all the rest, you can define different colors for all or some of those, f.ex. by adding something like this to your stylesheet (Edit → Stylesheet...):

```less
atom-text-editor::shadow, ide-haskell-panel {
    .support.function.prelude.haskell {
      color: #56b6c2; // or whatever color you like
    }
    .support.tag.prelude.haskell {
      color: #e9969d;
    }
}
```

If you don't want `Prelude` identifiers highlighted differently, you can override it by adding something like this to your stylesheet (Edit → Stylesheet...):

```less
atom-text-editor::shadow, ide-haskell-panel {
  .prelude.haskell {
    color: inherit;
  }
}
```

Note, you may need to reopen currently opened files (or restart Atom) for your new stylesheet to be applied.

# Contributing

See [CONTRIBUTING.md](https://github.com/atom-haskell/language-haskell/blob/master/CONTRIBUTING.md)

# License

Copyright © 2015 Atom-Haskell

Contributors (by number of commits):
* Nikolay Yakimov
* Jared Roesch
* Matthew Griffith
* samuela
* Ross Ogilvie
* Rob Rix
* Ranjit Jhala
* Michael Rawson
* mdgriffith
* Jesse Cooke
* Ian D. Bollinger

See the [LICENSE.md][LICENSE] for details.

[LICENSE]: https://github.com/atom-haskell/language-haskell/blob/master/LICENSE.md
