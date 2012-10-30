# Understanding the IO Monad

After this tutorial you should understand what you need to know to be
proficient at using the IO monad.  I assume some basic programming
background, and also that you've read some information about *pure*
functions, and their lack of side-effects.

Lets jump right in.  Examine the following basic haskell code:

```haskell
main :: IO ()
main = do
   putStrLn "What is your name?"
   name <- getLine
   putStrLn ("Welcome, " ++ name ++ "!")
```

The first part: 

```haskell
main :: IO ()
```

is the type signature of the `main` function.  The format of
signatures are: _function name_ :: _type signature_.

So we can see that the main function, or expression, evaluates to an
`IO ()`.  The `()` part is basically equivalent to a null.  The `IO`
part means the `()` is wrapped in `IO` monad.

Next lets examine the `do` block.  `do` is syntactic sugar.  Examine
the two eqivalent blocks:

```haskell
do
  a "hello"
  b
  c
```

and

```haskell
do
  a "hello" >>= b >>= c
```

`>>=` is the *bind* operator.  It is an *infix* operator, so it's
arguments are on the left and right hand sides of it, just like the
`+` operator in: `a + b`.

What the bind operator does, is it takes the value of the expression
on the left and applies it to the function on the right.  Lets say
that `a`,`b`, and `c` have the following function signatures:

```haskell
a :: String -> IO ()
b :: IO String
c :: String -> IO ()
```

What this means is `a` takes a `String` and returns an `IO ()`.  `b`
on the other hand doesn't take any arguments, but just results in an
`IO String`.  `c` is the same as `a`.

Bind also does one more thing, when it takes the output of the left
hand side (LHS), say for the case of `a` this would be `IO ()`, it
unpackages the value from the monad, in our case the IO monad.  So:

```haskell
IO ()
```

becomes

```haskell
()
```

or just null, not `IO` null.  After bind does this unpackaging, it
then supplies this to function on the right hand side (RHS).  So lets
go back to our example and process it step by step

```haskell
do
  a "hello" >>= b >>= c
```

So first we evaluate

```haskell
a "hello"
```

who's signature is: 

```haskell
a :: String -> IO ()
```

so it's result is of type:

```haskell
IO ()
```

now the bind operator, unpackages this, removing the IO monad,
leaving:

```haskell
()
```

and we apply this to the function on the right, `b`, who has the
following signature:

```haskell
b :: IO String
```

so this is a function that doesn't have any arguments, and just
evaluates to an `IO String`.  Well kind of appropriate since we were
only going to pass it a `()`, null, anyway!  So continuing with our
bind operator, we must now take the output of the `b` function and
strip the `IO` monad from it, leaving just `String`.

So the final part looks like this:

```haskell
b >>= c
```

with signatures:

```haskell
b :: IO String
c :: String -> IO ()
```

so the bind operator removes the `IO` from `b` resultant type, leaving
just `String`, which is exactly what `c` is looking for, so we are in
good shape.  Conveniently the `c` evaluates to `IO ()`, which is
exactly the type of the function `main`:

```haskell
main :: IO ()
main = do
  ...
  c :: String -> IO ()
```

so we are in good shape.  Now lets apply this to the real code that we
have at the top:

```haskell
main :: IO ()
main = do
   putStrLn "What is your name?"
   name <- getLine
   putStrLn ("Welcome, " ++ name ++ "!")
```

Lets get the method signatures for those functions:

```haskell
putStrLn :: String -> IO ()
getLine :: IO String
putStrLn :: String -> IO ()
```

Now you can step through this code, just as was done above to see how
the inputs and outputs proceed.  Just remember the bind operator takes
the output of the previous line, strips the `IO` part, applies whats
left as the argument to the next function.














