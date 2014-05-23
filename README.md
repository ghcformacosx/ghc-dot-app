# The easy way to install Haskell on Mac!

Want to try Haskell on your Mac without having to use homebrew or
even install anything globally? Download a self-contained GHC release here!

<ul class="release-downloads"><li><a
href="https://github.com/etrepum/ghc-dot-app/releases/download/v7.8.2-r0/ghc-7.8.2-r0.zip"
rel="nofollow" class="button primary"></li></ul>

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
