# The easy way to install Haskell on Mac!

Want to try Haskell on your Mac without having to use homebrew or
even install anything globally? Download a self-contained GHC release here!

* [ghc-7.8.2-r3.zip](https://github.com/etrepum/ghc-dot-app/releases/download/v7.8.2-r3/ghc-7.8.2-r3.zip)

# What it includes

* GHC 7.8.2
* cabal-install 1.20.0.1 (just the binary)
* alex 3.1.3
* happy 1.9.13
* HTML documentation
* man pages

Included as dependencies for alex and happy:

* QuickCheck 2.7.3
* mtl 2.2.0.1
* transformers 0.4.1.0
* primitive 0.5.3.0
* random 1.0.1.1
* tf-random 0.5

# Building

## Minimal (no GUI helper)

This will build a relocatable ghc-7.8.2.app in ./dist/build using
the binaries of GHC and cabal-install from haskell.org. This does not
include the GUI helper.

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

# Full (GUI helper with code signing)

USAGE:

```bash
$ (cd GHC; xcodebuild)
```

Will produce a signed release build at `./GHC/build/Release/GHC.app`.

# TODO

* Create an executable that will help the user add this GHC to their
  PATH
* Watch app bundle to see if it moves
* Help diagnose PATH issues?
* Help people get out of cabal hell?
* Pre-install some packages?
* Make placeholders for GHC that work like xcodeselect?
