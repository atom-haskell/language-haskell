# *** DO NOT EDIT GRAMMAR CSON FILES DIRECTLY ***

Some grammar files are generated based on common template. In particular,

- `grammars/haskell.cson`
- `grammars/haskell autocompletion hint.cson`
- `grammars/haskell type hint.cson`
- `grammars/haskell message hint.cson`
- `grammars/literate haskell.cson`

are generated based on instructions in `src/haskell.coffee`.

If you want to change those grammars, edit files in `src/include/` and then run `make` in the root of repository.
