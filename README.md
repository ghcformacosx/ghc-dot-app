This will build a relocatable ghc-7.8.2.app in ./dist/build using
the binaries of GHC and cabal-install from haskell.org.

USAGE:

```bash
$ runhaskell Main.hs
```

To clean up:

```bash
$ rm -rf ./dist/
```

Add this GHC to your PATH (assuming it has moved to /Applications):

```bash
export PATH=/Applications/ghc-7.8.2.app/Contents/bin:$PATH
```

TODO:

* Add a sensible Info.plist and related metadata (icon, etc.)
* Create an executable that will help the user add this GHC to their PATH
* Pre-install some packages?
* Make placeholders for GHC that work like xcodeselect?
