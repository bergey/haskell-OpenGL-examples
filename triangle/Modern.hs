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

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW

import qualified Util.GLFW as U


main :: IO ()
main = do
    win <- U.initialize "multicolor triangle"
    clientState VertexArray $= Enabled
    U.mainLoop (draw win) win
    U.cleanup win

colors :: V.Vector (Double)
colors = V.fromList [ 1, 0, 0
                    , 0, 1, 0
                    , 0, 0, 1
                    ]

vertices :: V.Vector (Double)
vertices = V.fromList [ -0.6,  0.4, 0
                      ,  0.6, -0.4, 0
                      ,  0.0,  0.6, 0
                      ]

draw :: GLFW.Window -> IO ()
draw w = do
    (width, height) <- GLFW.getFramebufferSize w
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))

    let ratio = fromIntegral width / fromIntegral height

    clear [ColorBuffer]

    matrixMode $= Projection
    loadIdentity
    ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
    matrixMode $= Modelview 0

    loadIdentity
    -- this is bad, but keeps the logic of the original example I guess
    Just t <- GLFW.getTime
    rotate (realToFrac t * 50) (Vector3 0 0 1 :: Vector3 GLdouble)

    color $ Color3 1 0 (0 :: GLdouble)
    V.unsafeWith vertices $ \ptr -> do
        arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 ptr
    drawArrays Triangles 0 3
