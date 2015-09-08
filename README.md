# sdl2-gfx

[![Build Status](https://travis-ci.org/sbidin/sdl2-gfx.svg?branch=master)](https://travis-ci.org/sbidin/sdl2-gfx)

#### Haskell bindings to SDL2_gfx

Both the raw and the higher level bindings should allow you to use any aspect
of the original SDL2_gfx library. Please report an issue if you encounter a bug
or feel that something is missing.

##### Install

This library depends on the new API version of
[haskell-game/sdl2](https://github.com/haskell-game/sdl2), available on
Hackage as
[sdl2 version 2.0.0 or greater](http://hackage.haskell.org/package/sdl2). With
that in mind, you can install sdl2-gfx from source like this:

```bash
git clone git@github.com:sbidin/sdl2-gfx.git
cd sdl2-gfx
cabal install
```

Note that you might get compile errors if you're not using the latest GHC. Only
7.10 is currently tested.

##### Documentation

You can find the documentation [here](https://bidin.eu/docs/sdl2-gfx).

The
[original SDL2_gfx documentation](http://www.ferzkopp.net/Software/SDL2_gfx/Docs/html/index.html)
can also help, as the bindings are close to a direct mapping.

##### Example

A small example executable is included with the library. It uses many parts of
the library to draw a chaotic jumbled mess on your screen. You can find it in
the `example` directory.

```bash
cd sdl2-gfx
cabal run
```
