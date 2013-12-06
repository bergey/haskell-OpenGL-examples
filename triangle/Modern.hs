{-# LANGUAGE OverloadedStrings #-}

module Main where

-- This is a Haskell translation of the official GLFW quick example
-- found at <http://www.glfw.org/docs/3.0/quick.html#quick_example>
-- using the GLFW-b library, version 1.x

-- I tried hard to keep the same structure so that it is simple
-- enough to go back and forth between the C version and the 
-- Haskell one, while preserving some usual haskell tricks

-- If you have any comment, bug report, or any other kind of
-- feedback, feel free to shoot me an email at
-- alpmestan at gmail dot com

import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core31
import qualified Graphics.UI.GLFW as GLFW

import qualified Util.GLFW as U
import qualified Util.Shaders as U

main :: IO ()
main = do
    win <- U.initialize "multicolor triangle"
    p <- initResources
    U.mainLoop (draw p win) win
    U.cleanup win

colors :: V.Vector Float
colors = V.fromList [ 1, 0, 0
                    , 0, 1, 0
                    , 0, 0, 1
                    ]

vertices :: V.Vector Float
vertices = V.fromList [ -0.6, -0.4, 0
                      ,  0.6, -0.4, 0
                      ,    0,  0.6, 0
                      ]

initResources :: IO Program
initResources = do
    v <- U.makeShader VertexShader vsSource
    f <- U.makeShader FragmentShader fsSource

    p <- U.makeProgram [v, f] [ ("coord", AttribLocation 0)
                              , ("color", AttribLocation 1)
                              ]
    currentProgram $= Just p

    U.printError
    return p

draw :: Program -> GLFW.Window -> IO ()
draw p w = do
    (width, height) <- GLFW.getFramebufferSize w
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))

    -- let ratio = fromIntegral width / fromIntegral height

    clear [ColorBuffer]

    currentProgram $= Just p
    UniformLocation tLoc <- get $ uniformLocation p "transform"
    vertexAttribArray (AttribLocation 0) $= Enabled
    V.unsafeWith vertices $ \ptr -> do
        vertexAttribPointer (AttribLocation 0) $=
            (ToFloat, VertexArrayDescriptor 3 Float 0 ptr)
    vertexAttribArray (AttribLocation 1) $= Enabled
    V.unsafeWith colors $ \ptr -> do
        vertexAttribPointer (AttribLocation 1) $=
            (ToFloat, VertexArrayDescriptor 3 Float 0 ptr)
    t <- GLFW.getTime
    V.unsafeWith (rotateM t) $ \ptr -> do
        glUniformMatrix3fv tLoc 1 1 ptr
    drawArrays Triangles 0 3
    vertexAttribArray (AttribLocation 0) $= Disabled
    vertexAttribArray (AttribLocation 1) $= Disabled


rotateM :: Maybe Double -> V.Vector GLfloat
rotateM Nothing  = V.fromList [ 1, 0, 0
                             , 0, 1, 0
                             , 0, 0, 1
                             ]
rotateM (Just t') = V.fromList [  cos t,  sin t, 0
                             , -sin t, -cos t, 0
                             ,      0,      0, 1
                             ] where
  t = realToFrac t'

vsSource, fsSource :: BS.ByteString
vsSource = BS.intercalate "\n"
           [ "attribute vec3 coord;"
           , "attribute vec3 color;"
           , "varying vec3 f_color;"
           , "uniform mat3 transform;"
           , ""
           , "void main(void) { "
           , " gl_Position = vec4(transform * coord, 1.0); "
           , " f_color = color;"
           , "}"
           ]

fsSource = BS.intercalate "\n"
           [ "varying vec3 f_color;"
           , ""
           , "void main(void) { "
           , " gl_FragColor = vec4(f_color, 1);"
           , "}"
           ]
