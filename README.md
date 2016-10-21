# sdl2-gfx

[![Hackage](https://img.shields.io/hackage/v/lens.svg)](https://hackage.haskell.org/package/sdl2-mixer)
[![Build Status](https://travis-ci.org/sbidin/sdl2-gfx.svg?branch=master)](https://travis-ci.org/sbidin/sdl2-gfx)

#### Haskell bindings to SDL2_gfx

Both the raw and the higher level bindings should allow you to use any aspect
of the original SDL2_gfx library. Please report an issue if you encounter a bug
or feel that something is missing.

##### Install

```bash
cabal install sdl2-gfx
```

##### Documentation

For documentation, [visit Hackage](https://hackage.haskell.org/package/sdl2-gfx).

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
