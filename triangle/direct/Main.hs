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
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO

-- tiny utility functions, in the same spirit as 'maybe' or 'either'
-- makes the code a wee bit cleaner
bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x
    
-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback err description = hPutStrLn stderr description

keyCallback :: G.KeyCallback
keyCallback window key scancode action mods = when (key == G.Key'Escape && action == G.KeyState'Pressed) $
  G.setWindowShouldClose window True

main :: IO ()
main = do
  G.setErrorCallback (Just errorCallback)
  successfulInit <- G.init
  -- if init failed, we exit the program
  bool successfulInit exitFailure $ do
      mw <- G.createWindow 640 480 "Simple example, haskell style" Nothing Nothing
      maybe' mw (G.terminate >> exitFailure) $ \window -> do
          G.makeContextCurrent mw
          G.setKeyCallback window (Just keyCallback)
          mainLoop window
          G.destroyWindow window
          G.terminate
          exitSuccess
          
mainLoop :: G.Window -> IO ()
mainLoop w = unless' (G.windowShouldClose w) $ do
    (width, height) <- G.getFramebufferSize w
    let ratio = fromIntegral width / fromIntegral height
    
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    clear [ColorBuffer]
    
    matrixMode $= Projection
    loadIdentity
    ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
    matrixMode $= Modelview 0
    
    loadIdentity
    -- this is bad, but keeps the logic of the original example I guess
    Just t <- G.getTime
    rotate ((realToFrac t) * 50) $ (Vector3 0 0 1 :: Vector3 GLdouble)
    
    renderPrimitive Triangles $ do
        color  (Color3 1 0 0 :: Color3 GLdouble)
        vertex (Vertex3 (negate 0.6) (negate 0.4) 0 :: Vertex3 GLdouble)
        color  (Color3 0 1 0 :: Color3 GLdouble)
        vertex (Vertex3 0.6 (negate 0.4) 0 :: Vertex3 GLdouble)
        color  (Color3 0 0 1 :: Color3 GLdouble)
        vertex (Vertex3 0 0.6 0 :: Vertex3 GLdouble)
        
    G.swapBuffers w
    G.pollEvents
    mainLoop w