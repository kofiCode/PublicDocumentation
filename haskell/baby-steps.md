Learn to use some basic system calls.

```haskell
ghci> :module System.Cmd
ghci> rawSystem "ls" ["-l", "/usr"]
total 212
drwxr-xr-x   3 root root 40960 Oct 27 08:08 bin
drwxr-xr-x 321 root root 36864 Oct 26 12:35 include
drwxr-xr-x 145 root root 90112 Oct 26 12:35 lib
...
```

So lets code this:

```haskell
import System.Process
main :: IO ()
main = do rawSystem "ls" ["-l", "/usr"]
```

Compile it:

```bash
$ ghc Main.hs
    Couldn't match type `GHC.IO.Exception.ExitCode' with `()'
    Expected type: IO ()
      Actual type: IO GHC.IO.Exception.ExitCode
```

We see from the
[documentation](http://hackage.haskell.org/packages/archive/process/1.0.1.1/doc/html/System-Process.html)
that the signature to rawSystem is: 

```haskell
rawSystem :: String -> [String] -> IO ExitCode
```

but we know main must return `IO ()`.  Well putting in: `return ()`

```haskell
main = do 
  rawSystem "ls" ["-l", "/usr"]
  return ()
```

lets it compile:

```bash
$ ghc Main.hs
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
$ Main
total 212
drwxr-xr-x   3 root root 40960 Oct 27 08:08 bin
drwxr-xr-x 321 root root 36864 Oct 26 12:35 include
drwxr-xr-x 145 root root 90112 Oct 26 12:35 lib
```

however this is quite useless, we'd like to read a directory, put it
into some type of data structure, do some analysis, then print out a
result right?  Why don't we sum the file sizes, so mimic the `du -sh`
command? 



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



