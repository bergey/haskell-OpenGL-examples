## Comparative examples of Modern OpenGL in Haskell

These are the examples I wish I had when I started learning OpenGL.
There are more examples in the wild of the deprecated pre-3.0 API than
of modern OpenGL, and of course nearly all are in C(++).  The usual
assortment of GLUT and other windowing frameworks doesn't help.

These examples (will eventually) cover several different choices of
Haskell wrapper over OpenGL:

* modern buffers & VAOs using the [OpenGL][] package
* [GLUtil][] wrappers to reduce boilerplate & imperative style
* [vinyl-gl][] (extensible Haskell records, GHC type hackery) to remove more boilerplate

[OpenGL]: http://hackage.haskell.org/package/OpenGL
[GLUtil]: http://hackage.haskell.org/package/GLUtil
[vinyl-gl]: http://hackage.haskell.org/package/vinyl-gl

All examples use [GLFW-b][] for windowing.  There are a few helper
functions in `src/Util` which are used in all the examples.

If you are using these examples and find something missing or
difficult to understand, please open an issue.  Pull requests are also
welcome.

The example programs:
* `wikibook` ports a few examples from the excellent [OpenGL Wikibook](https://en.wikibooks.org/wiki/OpenGL_Programming)
* `glfw` (2D rotating triangle, based on the [example][] that ships with [GLFW][])

Running `cabal install` in this directory will build all of the
tutorial examples.  Take a look at
[the .cabal file](/haskell-OpenGL-examples.cabal) to see how the
executables are named.

### Resources

Vladimir Lopatin has a [good set of tutorials](https://github.com/madjestic/Haskell-OpenGL-Tutorial/) on modern OpenGL in Haskell.  They have more detail on interactions with [GLFW-b][] and do not use [GLUtil][] or [vinyl-gl][].

[GLFW]: http://www.glfw.org/
[example]: http://www.glfw.org/docs/3.0/quick.html#quick_example
[GLFW-b]: http://hackage.haskell.org/package/GLFW-b
