module Util.GLFW where

import Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import System.Exit
import System.IO

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ = when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
  GLFW.setWindowShouldClose window True

initialize :: String -> IO GLFW.Window
initialize title = do
  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init
  -- if init failed, we exit the program
  if not successfulInit then exitFailure else do
      GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
      GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
      mw <- GLFW.createWindow 640 480 title Nothing Nothing
      case mw of
          Nothing -> GLFW.terminate >> exitFailure
          Just window -> do
              GLFW.makeContextCurrent mw
              GLFW.setKeyCallback window (Just keyCallback)
              return window

cleanup :: GLFW.Window -> IO ()
cleanup win = do
    GLFW.destroyWindow win
    GLFW.terminate
    exitSuccess

mainLoop :: IO () -> GLFW.Window -> IO ()
mainLoop draw w = do
    close <- GLFW.windowShouldClose w
    unless close $ do
                    draw
                    GLFW.swapBuffers w
                    GLFW.pollEvents
                    mainLoop draw w
