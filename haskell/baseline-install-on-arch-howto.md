Getting a baseline Haskell system up and running on Arch

```bash
$ sudo pacman -Syu
$ sudo pacman -S ghc cabal-install
```

Ensure `~/.cabal/bin` is at the front of your path

```bash
$ env | grep PATH
PATH=~/.cabal/bin:/usr/local/sbin:/usr/local/bin
```

update cabal and install a new cabal from hackage

```bash
$ cabal update
$ cabal install cabal-install
```

trying to install `darcs` gives the following errors:

```bash
$ cabal install darcs
Resolving dependencies...
cabal: Could not resolve dependencies:
trying: darcs-2.8.3
trying: darcs-2.8.3:+terminfo
trying: darcs-2.8.3:-force-char8-encoding
trying: terminfo-0.3.2.5
rejecting: base-3.0.3.2, 3.0.3.1 (global constraint requires installed
instance)
rejecting: base-4.6.0.0/installed-910... (conflict:
darcs-2.8.3:force-char8-encoding => base>=4 && <4.4)
rejecting: base-4.6.0.0, 4.5.1.0, 4.5.0.0, 4.4.1.0, 4.4.0.0, 4.3.1.0, 4.3.0.0,
4.2.0.2, 4.2.0.1, 4.2.0.0, 4.1.0.0, 4.0.0.0 (global constraint requires
installed instance)
```
