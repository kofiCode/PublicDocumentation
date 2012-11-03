# Fixing Cabal dependency hell.


## Overview 

We would like to strive towards a dependency management solution, for
haskell, that doesn't fail to compile.  Cabal, can fail when you
request it to install a library.

Before fixing a problem, the problem should be well understood.  The
next section will articulate the problem.


## The Context

### GHC-PKG

Understanding how GHC handles packages.

### Hackage



### darcs


### package deps

[Package Deps](http://packdeps.haskellers.com/)

Enter a search string in the box above. It will find all packages
containing that string in the package name, maintainer or author
fields, and create an Atom feed for restrictive bounds. Simply add
that URL to a news reader, and you're good to go!

All of the code is available on Github . Additionally, there is a
package on Hackage with the code powering this site both as a library
and as an executable, so you can test code which is not uploaded to
the public Hackage server.
 



## Problem

* [The dreaded diamond dependency](http://www.well-typed.com/blog/9)


## Solutions

### Sandboxing

cabal-dev, uses sandboxing as a technique


