# The easy way to install Haskell on Mac!

Want to try Haskell on your Mac without having to use homebrew or
even install anything globally? Download a self-contained GHC release here!

* [ghc-7.10.2-r0.zip](https://github.com/etrepum/ghc-dot-app/releases/download/v7.10.2-r0/ghc-7.10.2-r0.zip)

# What it includes

* GHC 7.10.2
* cabal-install 1.22.6.0 (just the binary)
* stack 0.1.2.0 (just the binary)
* HTML documentation
* man pages

# Building

To build from this repository you will need an existing installation of
GHC such as the self-contained release above.

The `Makefile` will take care of satisfying any dependencies you might
need except for Xcode. It leverages `stack` to do everything.

## Minimal (no GUI helper)

This will build a relocatable ghc-7.10.2.app in ./dist/build using
the binaries of GHC and cabal-install from haskell.org and stack from
GitHub. This does not include the GUI helper.

USAGE:

```bash
$ make
```

To clean up:

```bash
$ rm -rf ./dist/
```

Add this GHC to your PATH (assuming it has moved to /Applications):

```bash
export PATH=$HOME/.local/bin:$HOME/.cabal/bin:/Applications/ghc-7.10.2.app/Contents/bin:$PATH
```

# Full (GUI helper with code signing)

USAGE:

```bash
$ (cd GHC; xcodebuild)
```

Will produce a signed release build at `./GHC/build/Release/GHC.app`,
if you have setup a signing identity with a Developer ID certificate.

If for some reason there is no icon, first a few obsceneties about Xcode
under your breath, and then do a clean build like this:

```bash
$ (cd GHC; xcodebuild clean build)
```

# TODO

* Watch app bundle to see if it moves
* Help people get out of cabal hell?
* Pre-install some packages?
* Make placeholders for GHC that work like xcodeselect?
* Add shell autocompletion for stack https://github.com/commercialhaskell/stack/wiki/Shell-autocompletion
