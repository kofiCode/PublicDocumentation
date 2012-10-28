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

At the documentation page, I found a command called: 

```haskell
readProcess :: FilePath -> [String] -> String -> IO String
```

so lets give this one a try:

```haskell
main = do 
  dirList <- readProcess "ls" ["-l", "/usr"] []
  print dirList
  return ()
```

compiling...

```bash
$ ghc Main
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
$ Main
"total 212\ndrwxr-xr-x   3 root root 40960 Oct 27 08:08 bin\ndrwxr-xr ...
```

Something very tricky just transpired!  The `<-` operator converted an
`IO String` to a `String`, something that a lot of documentation says
you CAN'T do.  The reason you can do it, is that we are already in dirList
function that will return an `IO ()`.

It's very important for you to see how we just moved from an IMPURE,
step, to a pure step.  Of course ultimately we must go back to
Monads/impure, etc..., because main must return an `IO ()`.

Moving on.  Lets convert the big string in lines with the `lines`
function!  Well lets find the definition of `lines` first!!!  Here are
a few ways to find information about functions:

* [Hoogle](http://www.haskell.org/hoogle)

* From GHCI: `:info lines` or `:i lines` for short.

```haskell
  dirList <- readProcess "ls" ["-l", "/usr"] []
  myLines = lines dirList
  return ()
```

But this doesn't work!

```bash
 $ ghc Main.hs
Main.hs:7:11: parse error on input `='
```

Damn!  The reason for this is that the `do` notation is syntactic
sugar, that must be used properly.  Lets look at an example:

```haskell
do
   putStrLn "What is your name?"
   name <- getLine
   putStrLn ("Welcome, " ++ name ++ "!")
```

is the syntactic sugar for:

```haskell
putStrLn "What is your name?"
>>= (\_ -> getLine)
>>= (\name -> putStrLn ("Welcome, " ++ name ++ "!"))
```

What the `>>=` bind infix operator does, is take the value on the left
and apply it to the function on the right.  Here is the signature for
`putStrLn`: 

```haskell
putStrLn :: String -> IO ()
```

So it results in an `IO ()`.  Lets quickly review how anonymous
function work.

```haskell
ghci> (\x -> x + 1) 4
5 :: Integer
```

The previous example is run in the GHCI environment.  Basically: `(\x
-> x + 1)` is the anonymous function part.  `4` is the argument to the
anonymous function.  We start anonymous functions with a `\` because
it looks a bit like a lambda sign, which is a synonym for anonymous
function from math.  The next part between `\` and `->` are the
arguments, in this case there is just one: `x`.  Finally the part to
the right of `->` is the function.  In this case we increment our
argument by one, thus the `5 :: Integer` result.  Okay back to IO!

So we are currently looking at:

```haskell
 putStrLn "What is your name?" >>= (\_ -> getLine)
```

`putStrln`... turns into a: `IO ()` as we can see by it's signature,
and this is applied to: `(\_ -> getLine)`.  The underscore, `_`,
basically means we dont care what our argument is, basically just
throw it away.  So lets look at `getLine`:

```haskell
> :i getLine
getLine :: IO String
```

So the value of `getLine` is simply an `IO String`.  Finally we are
left with:

```haskell
(\_ -> getLine) >>= (\name -> putStrLn ("Welcome, " ++ name ++ "!"))
```

We know the left hand side is of type: `IO String`, and the value of
putStrLn again is:

```haskell
putStrLn :: String -> IO ()
```

Well this takes a `String` not a `IO String`!  The bind operator also
unpackages the monad that it is working with, so `IO String` turns
into `String`.  This finally results in an `IO ()`, just the type that
main must evaluate to!  Recall the type of `main`:

```haskell
main :: IO ()
main = do
...
```

So back to our problem:

```haskell
  dirList <- readProcess "ls" ["-l", "/usr"] []
  myLines = lines dirList
  return ()
```

again the error message:

```bash
 $ ghc Main.hs
Main.hs:7:11: parse error on input `='
```

Well whats the type of: `lines a`?

```haskell
> :i lines
lines :: String -> [String]
```

There is no `IO` to be seen anywhere, so it breaks down.  We need to
replace it with a function that returns a type of: `IO` I guess.  Lets
try. 

Our original goal was to sum up the file sizes, so lets continue down
that path.  However, lets go really slow and build up to it!  First,
lets see the return type of the main function here: `readProcess`:

```haskell
> :i readProcess
FilePath -> [String] -> String -> IO String
```

Remember the bind `>>=` operator removes the contents of the type `IO
xyz` to just be `xyz`.  So we need to make a function that takes a
`String` in, and returns an `IO String`.  Lets call it: `dirSpaceUsed`,
so:

```haskell
dirSpaceUsed :: String -> IO String
```

Thats our end goal.  But again, lets go slow!

Lets get some helper functions.  `lines` breaks a long string on line
separators: `'\n'` for example.  So we'll end up with a list of
`String`'s, i.e.: `[String]`.

`splitOn`, in the `Data.List.Split` module, splits a string on a given
character:

```haskell
> splitOn "," "my,comma,separated,list"
["my","comma","separated","list"]
```

However, lines returns a list of `String`s, while splitOn takes only a
single `String`!  We can use a handy function called `map` which takes a
function and a list, and applies the function to each element in the
list, and returns the new list.  So we should be able to do something
like: 

```haskell
map (splitOn ",") (lines dirListing)
```

But I don't think we can write it like this.  Lets use a `where`
clause, so we can make a bunch of assignments that aid in legibility.
Here is the function I whipped up:

```haskell
makeArrayFromDirListing :: String -> [[String]]
makeArrayFromDirListing dirListing = 
    map splitOnComma dirListingLines
    where
      dirListingLines = lines dirListing
      splitOnComma = splitOn ","
```



`split` looks like a candidate to break up a line on a delimeter:

```haskell
  myLines = lines a
  splitter = split (==' ')
  splitLines = map splitter myLines
```
