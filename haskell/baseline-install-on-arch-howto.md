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

get `darcs` for version control, put the following into
`/etc/pacman.conf` 

```
[archlinuxfr]
Server = http://repo.archlinux.fr/$arch
```

then run:

```bash
$ sudo pacman -S yaourt
$ cabal install haskeline
$ sudo yaourt -S darcs
```
