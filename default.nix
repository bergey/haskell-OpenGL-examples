{ mkDerivation, base, bytestring, distributive, file-embed
, filepath, GLFW-b, GLUtil, linear, OpenGL, OpenGLRaw, singletons
, stdenv, vector, vinyl, vinyl-gl
}:
mkDerivation {
  pname = "haskell-OpenGL-examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base bytestring distributive file-embed filepath GLFW-b GLUtil
    linear OpenGL OpenGLRaw singletons vector vinyl vinyl-gl
  ];
  homepage = "http://github.com/bergey/haskell-OpenGL-examples";
  description = "Comparative examples of OpenGL in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
