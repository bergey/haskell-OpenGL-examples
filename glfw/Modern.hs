{-# LANGUAGE OverloadedStrings #-}

module Main where

-- General Haskell modules
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS
import           Control.Monad (unless)
import           System.Exit (exitFailure)

-- Import all OpenGL libraries qualified, for pedagogical reasons
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))
import           Graphics.Rendering.OpenGL.Raw.Core31 (glUniformMatrix3fv, glUniformMatrix4fv)
import qualified Graphics.UI.GLFW as GLFW

-- Local modules
import qualified Util.GLFW as U
import qualified Util.Shaders as U

main :: IO ()
main = do
    win <- U.initialize "multicolor triangle"
    p <- initResources
    U.mainLoop (draw p win) win
    U.cleanup win

initResources :: IO GL.Program
initResources = do
    -- compile shaders
    vSh <- GL.createShader GL.VertexShader
    GL.shaderSourceBS vSh $= vsSource
    GL.compileShader vSh
    vs'Ok <- GL.get $ GL.compileStatus vSh
    unless vs'Ok $ do
        slog <- GL.get $ GL.shaderInfoLog vSh
        putStrLn $ "Log:" ++ slog
        exitFailure

    -- Do it again for the fragment shader
    fSh <- GL.createShader GL.FragmentShader
    GL.shaderSourceBS fSh $= fsSource
    GL.compileShader fSh
    fs'Ok <- GL.get $ GL.compileStatus fSh
    unless fs'Ok $ do
        slog <- GL.get $ GL.shaderInfoLog fSh
        putStrLn $ "Log:" ++ slog
        exitFailure

    -- link shaders into a program
    program <- GL.createProgram
    GL.attachShader program vSh
    GL.attachShader program fSh
    GL.attribLocation program "coord" $= GL.AttribLocation 0
    GL.attribLocation program "color" $= GL.AttribLocation 1
    GL.linkProgram program
    p'Ok <- GL.get $ GL.linkStatus program
    GL.validateProgram program
    status <- GL.get $ GL.validateStatus program
    unless (p'Ok && status) $ do
        plog <- GL.get $ GL.programInfoLog program
        putStrLn plog
        exitFailure

    GL.currentProgram $= Just program

    U.printError
    return program

draw :: GL.Program -> GLFW.Window -> IO ()
draw p w = do
    (width, height) <- GLFW.getFramebufferSize w
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    let ratio = fromIntegral width / fromIntegral height

    GL.clear [GL.ColorBuffer]

    GL.currentProgram $= Just p
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    V.unsafeWith vertices $ \ptr -> do
        GL.vertexAttribPointer (GL.AttribLocation 0) $=
            (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    V.unsafeWith colors $ \ptr -> do
        GL.vertexAttribPointer (GL.AttribLocation 1) $=
            (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)
    t <- GLFW.getTime
    GL.UniformLocation tLoc <- GL.get $ GL.uniformLocation p "transform"
    V.unsafeWith (rotateM t) $ \ptr -> do
        glUniformMatrix3fv tLoc 1 1 ptr
    GL.UniformLocation pLoc <- GL.get $ GL.uniformLocation p "projection"
    V.unsafeWith (aspectM ratio) $ \ptr ->
        glUniformMatrix4fv pLoc 1 1 ptr
    GL.drawArrays GL.Triangles 0 3
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Disabled


rotateM :: Maybe Double -> V.Vector GL.GLfloat
rotateM Nothing  = V.fromList [ 1, 0, 0
                             , 0, 1, 0
                             , 0, 0, 1
                             ]
rotateM (Just t') = V.fromList [  cos t, -sin t, 0
                               ,  sin t,  cos t, 0
                               ,      0,      0, 1
                               ] where
  t = realToFrac t'

aspectM :: GL.GLfloat -> V.Vector GL.GLfloat
aspectM r = V.fromList [ 1/r, 0, 0, 0
                       ,   0, 1, 0, 0
                       ,   0, 0, 1, 0
                       ,   0, 0, 0, 1
                       ]

vsSource, fsSource :: BS.ByteString
vsSource = BS.intercalate "\n"
           [ "attribute vec3 coord;"
           , "attribute vec3 color;"
           , "varying vec3 f_color;"
           , "uniform mat3 transform;"
           , "uniform mat4 projection;"
           , ""
           , "void main(void) { "
           , " gl_Position = projection * vec4(transform * coord, 1.0); "
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
