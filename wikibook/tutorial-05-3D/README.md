## Three Dimensions

This code accompanies the
[wikibook tutorial](https://en.wikibooks.org/wiki/OpenGL_Programming/Modern_OpenGL_Tutorial_05)
It draws a spinning cube spanning all (fully saturated) RGB colors on a white background.

GLUtil.hs uses the same techniques and libraries as the previous tutorial, except for the new 3D constructs.

Vinyl.hs uses [vinyl-gl][] to marshal the shader inputs and provide better typing.

TH.hs uses Template Haskell and [file-embed][] to inline the shaders
at compile time.  This avoids hardcoded file paths in the executable,
and the requirement to be in a particular directory when running the
program.

[GLUtil]: http://hackage.haskell.org/package/GLUtil
[vinyl-gl]: http://hackage.haskell.org/package/vinyl-gl
[file-embed]: http://hackage.haskell.org/package/file-embed
