{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import System.Exit
import Data.ByteString (ByteString)
import qualified Data.Vector.Storable as V
import           Foreign.Ptr
import           Foreign.Storable

import           Graphics.Rendering.OpenGL

import           Util.GLFW (mainWith)

fragmentShader, vertexShader :: ByteString
fragmentShader = "#version 400 core\n\
\\n\
\out  vec4 fColor;\n\
\\n\
\void\n\
\main()\n\
\{\n\
\    fColor = vec4( 1.0, 0.0, 1.0, 1.0 );\n\
\}"

vertexShader = "#version 400 core\n\
\\n\
\in  vec4  vPosition;\n\
\\n\
\void\n\
\main()\n\
\{\n\
\    gl_Position = vPosition;\n\
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

  [arrayBuffer]  <- genObjectNames 1
  bindBuffer ArrayBuffer $= Just arrayBuffer

  let numBytes = fromIntegral $ V.length vertices  * sizeOf (V.head vertices)
--      stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
  V.unsafeWith vertices $ \ptr ->
    bufferData ArrayBuffer $= (numBytes, ptr, StaticDraw)

  vSh <- createShader VertexShader
  fSh <- createShader FragmentShader


  shaderSourceBS vSh $= vertexShader
  compileShader vSh
  v'Ok <- get $ compileStatus vSh
  unless v'Ok $ do
      vlog <- get $ shaderInfoLog vSh
      putStrLn $ "Log:" ++ vlog
      exitFailure

  shaderSourceBS fSh $= fragmentShader
  compileShader fSh
  f'Ok <- get $ compileStatus fSh
  unless f'Ok $ do
      flog <- get $ shaderInfoLog fSh
      putStrLn $ "Log:" ++ flog
      exitFailure

  shProg <- createProgram
  attachedShaders shProg $= [vSh, fSh]
  linkProgram shProg

  vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 2 Float 0 offset0)
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
    mainWith mainInit $ const display

-- |A zero-offset 'Ptr'.
offset0 :: Ptr a
offset0 = wordPtrToPtr . fromIntegral $ (0 :: Int)
