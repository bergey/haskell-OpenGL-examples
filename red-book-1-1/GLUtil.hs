module Main where

import qualified Data.Vector.Storable as V
import           Foreign.Ptr
import           Foreign.Storable

import           Graphics.Rendering.OpenGL
import Graphics.GLUtil

import           Util.GLFW (mainWith)

fragmentShader, vertexShader :: String
fragmentShader = "#version 400 core\
\\
\out  vec4 fColor;\
\\
\void\
\main()\
\{\
\    fColor = vec4( 1.0, 0.0, 1.0, 1.0 );\
\}"

vertexShader = "#version 400 core\
\\
\in  vec4  vPosition;\
\\
\void\
\main()\
\{\
\    gl_Position = vPosition;\
\}"

numVAOs :: Int
numVAOs = 1

numBuffers :: Int
numBuffers = 1

vPosition :: AttribLocation
vPosition = AttribLocation 0

numVertices :: Integral a => a
numVertices = 6

vertices :: V.Vector Double
vertices = V.fromList [
             -0.90, -0.90
           ,  0.85, -0.90
           , -0.90,  0.85
           ,  0.90, -0.85 -- Triangle 2
           ,  0.90,  0.90
           , -0.85,  0.90 ]

mainInit :: IO VertexArrayObject
mainInit = do
  [vaoTriangles] <- genObjectNames 1
  bindVertexArrayObject $= Just vaoTriangles

  clearColor $= Color4 0 1 0 1

  fromVector ArrayBuffer vertices

  vSh <- loadShader VertexShader "triangles.vert"
  fSh <- loadShader FragmentShader "triangles.frag"

  -- vSh <- createShader VertexShader
  -- fSh <- createShader FragmentShader

  -- shaderSource vSh $= [vertexShader]
  -- shaderSource fSh $= [fragmentShader]
  -- compileShader vSh
  -- compileShader fSh

  -- shProg <- createProgram
  -- attachedShaders shProg $= [vSh, fSh]
  -- linkProgram shProg
  shProg <- linkShaderProgram [vSh, fSh]

  let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
  vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 2 Float stride offset0)
  vertexAttribArray   vPosition $= Enabled
  bindB

  return vaoTriangles

display :: VertexArrayObject -> IO ()
display vaoTriangles = do
    clear [ColorBuffer]
    bindVertexArrayObject $= Just vaoTriangles
    drawArrays Triangles 0 numVertices
    flush

main :: IO ()
main = do
    mainWith mainInit $ const display
