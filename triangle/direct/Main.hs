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

import Control.Monad (unless, when)
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import System.Exit
import System.IO

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ = when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
  GLFW.setWindowShouldClose window True

main :: IO ()
main = do
  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init
  -- if init failed, we exit the program
  if not successfulInit then exitFailure else do
      mw <- GLFW.createWindow 640 480 "Simple example, haskell style" Nothing Nothing
      case mw of
          Nothing -> GLFW.terminate >> exitFailure
          Just window -> do
              GLFW.makeContextCurrent mw
              GLFW.setKeyCallback window (Just keyCallback)
              mainLoop window
              GLFW.destroyWindow window
              GLFW.terminate
              exitSuccess

mainLoop :: GLFW.Window -> IO ()
mainLoop w = do
    close <- GLFW.windowShouldClose w
    unless close $ do
        (width, height) <- GLFW.getFramebufferSize w
        let ratio = fromIntegral width / fromIntegral height

        viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
        clear [ColorBuffer]

        matrixMode $= Projection
        loadIdentity
        ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
        matrixMode $= Modelview 0

        loadIdentity
        -- this is bad, but keeps the logic of the original example I guess
        Just t <- GLFW.getTime
        rotate (realToFrac t * 50) (Vector3 0 0 1 :: Vector3 GLdouble)

        renderPrimitive Triangles $ do
            color  (Color3 1 0 0 :: Color3 GLdouble)
            vertex (Vertex3 (negate 0.6) (negate 0.4) 0 :: Vertex3 GLdouble)
            color  (Color3 0 1 0 :: Color3 GLdouble)
            vertex (Vertex3 0.6 (negate 0.4) 0 :: Vertex3 GLdouble)
            color  (Color3 0 0 1 :: Color3 GLdouble)
            vertex (Vertex3 0 0.6 0 :: Vertex3 GLdouble)

    GLFW.swapBuffers w
    GLFW.pollEvents
    mainLoop w
