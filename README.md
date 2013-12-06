haskell-OpenGL-examples
=======================

## Comparative examples of OpenGL in Haskell

These are the examples I wish I had when I started learning OpenGL.
There are more examples in the wild of the deprecated pre-3.0 API than
of modern OpenGL, and of course nearly all are in C(++).  The usual
assortment of GLUT and other windowing frameworks doesn't help.

These examples (will eventually) cover several different choices of
Haskell wrapper over OpenGL:

* old-school direct-mode using the [OpenGL][] package
* modern buffers & VAOs using the [OpenGL][] package
* [GLUtil][] wrappers to reduce boilerplate & imperative style
* [vinyl-gl][] (extensible Haskell records, GHC type hackery) to remove more boilerplate

[OpenGL]: http://hackage.haskell.org/package/OpenGL
[GLUtil]: http://hackage.haskell.org/package/GLUtil
[vinyl-gl]: http://hackage.haskell.org/package/vinyl-gl

All examples use [GLFW-b][] for windowing.  There are a few helper
functions in `src/Util` which are used in all the examples.

I've otherwise tried to avoid such functions, to make it easier to
pick any example and see what is going on.  Most functions I'd want
are provided by `GLUtil` anyway, and the point of the "-modern"
examples is to show the use of the OpenGL bindings without those
wrappers.

To some extent it's also possible to mix and match these approaches.
One can use custom shaders with the `gl_Modelview` matrix, or
`drawArrays` with the fixed-function pipeline.  Pull requests welcome.

The example programs:
* `wikibook` ports a few examples from the excellent [OpenGL Wikibook](https://en.wikibooks.org/wiki/OpenGL_Programming)
* `glfw` (2D rotating triangle, based on the [example][] that ships with [GLFW][])

[GLFW]: http://www.glfw.org/
[example]: http://www.glfw.org/docs/3.0/quick.html#quick_example
[GLFW-b]: http://hackage.haskell.org/package/GLFW-b
