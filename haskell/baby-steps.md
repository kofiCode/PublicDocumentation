Folder structure:

```
haskell-test1/
`-- test1.hs
```

in `test1.hs` put:

```haskell
f a = a + 1
```

ref: http://sritchie.github.com/2011/09/25/haskell-in-emacs.html

type `C-c C-z` to get to the Haskell REPL, supplied by inf-haskell mode

Then type `C-c C-l` in that buffer to load the file's contents into the
REPL. `C-c C-z` over to the repl again and try it out:
