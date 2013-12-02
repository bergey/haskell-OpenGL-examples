module Main where

import qualified Data.Vector.Storable as V
import           Foreign.Ptr
import Foreign.C.Types (CInt)
import           Foreign.Storable
import           System.IO

import           Graphics.Rendering.OpenGL

import           Util.GLFW (mainWith)

numVAOs :: Int
numVAOs = 1

numBuffers :: Int
numBuffers = 1

vPosition :: AttribLocation
vPosition = AttribLocation 0

numVertices :: CInt
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

  [arrayBuffer]  <- genObjectNames 1
  bindBuffer ArrayBuffer $= Just arrayBuffer

  let len = fromIntegral $ V.length vertices  * sizeOf (V.head vertices)
  V.unsafeWith vertices $ \ptr ->
    bufferData ArrayBuffer $= (len, ptr, StaticDraw)

  [vSh] <-  genObjectNames 1
  [fSh] <- genObjectNames 1

  withFile "triangles.vert" ReadMode $ \vHandle -> do
      withFile "triangles.frag" ReadMode $ \fHandle -> do
          vText <- hGetContents vHandle
          fText <- hGetContents fHandle
          shaderSource vSh $= [vText]
          shaderSource fSh $= [fText]
          compileShader vSh
          compileShader fSh

  [shProg] <- genObjectNames 1
  attachedShaders shProg $= ([vSh], [fSh])
  linkProgram shProg

  vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)
  vertexAttribArray   vPosition $= Enabled

  return vaoTriangles

display :: VertexArrayObject -> IO ()
display vaoTriangles = do
    clear [ColorBuffer]
    bindVertexArrayObject $= Just vaoTriangles
    drawArrays Triangles 0 numVertices
    flush

main :: IO ()
main = do
    vao <- mainInit
    mainWith $ const (display vao)
