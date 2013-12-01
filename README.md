haskell-OpenGL-examples
=======================

Comparative examples of OpenGL in Haskell

These are the examples I wish I had when I started learning OpenGL.
There are more examples in the wild of the deprecated pre-3.0 API than
of modern OpenGL, and of course all examples are in C.  The usual
assortment of GLUT and other windowing frameworks doesn't help.  

These examples (will eventually) cover several different choices of
Haskell wrapper over OpenGL:

* old-school direct-mode using the OpenGL package
* modern buffers & VAOs using the OpenGL package
* GLUtil (wrappers to reduce boilerplate & imperative style)
* custom GPU shaders using vinyl-gl (extensible Haskell records, GHC type hackery)

All examples use GLFW-b for windowing.

The example programs, in order of increasing complexity:
* triangle (2D rotating triangle)
* gears (3D gears, pan, zoom, orbit)
