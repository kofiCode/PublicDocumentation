Folder structure:

```
haskell-test1/
`-- First.hs
```

in `First.hs` put:

```haskell
f a = a + 1
```

ref: http://sritchie.github.com/2011/09/25/haskell-in-emacs.html

type `C-c C-z` to get to the Haskell REPL, supplied by inf-haskell mode

Then type `C-c C-l` in the buffer containing the `First.hs` file to
load the file's contents into the REPL. `C-c C-z` over to the repl
again and try it out:

```haskell
*Main> f 2
3
```

Lets structure our file properly.

Refer to: "Real World Haskell.  Section: Anatomy of a Module."

```haskell
module First (
              f,
              g
             ) where
f a = a + 1
g a = a * 2
```

So this is a proper library.  Lets create a file that uses it.  Create
the file: `Main.hs` with the following contents:

```haskell
module Main () where
import First
main =  do c <- getChar
putChar c
```



