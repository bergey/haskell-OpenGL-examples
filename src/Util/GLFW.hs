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

mainWith :: ( GLFW.Window -> IO () ) -> IO ()
mainWith draw = do
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
              mainLoop draw window
              GLFW.destroyWindow window
              GLFW.terminate
              exitSuccess

mainLoop :: ( GLFW.Window -> IO () ) -> GLFW.Window -> IO ()
mainLoop draw w = do
    close <- GLFW.windowShouldClose w
    unless close $ draw w
    GLFW.swapBuffers w
    GLFW.pollEvents
    mainLoop draw w
