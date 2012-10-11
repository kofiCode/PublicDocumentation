Starting out my journey with learning Haskell, and pretty quickly
realize I need to understand what these things called Monads are.  I
love simple, straight forward explanations, so I hope you'll find this
one to be that way.

I'm coming from Java land, so I'm assuming the audience isn't familiar
with functional concepts, so I'll cover the few that motivate the need
for Monads.

In haskell, the order of execution of two lines of code is NOT
necessarily done from top to bottom.  In this way it is very much like
regular math.

```
f(a) = 3 + a
g(b) = 2 + b
f(g(1)) = f( 2 + 1 ) = 3 + 2 + 1
```

now the computer can choose to do either the 3+2 first or the 2+1
first, like so: 

    ( 3 + 2 ) + 1 = 5 + 1 = 6

or 

    3 + ( 2 + 1 ) = 3 + 3 = 6

however in an imperative language like C or Java, we force the
computer to execute instruction in the order that they are written.
So:

```
print "Hello "
print "World!" 
```

must not print out:

   "World!Hello "
   
