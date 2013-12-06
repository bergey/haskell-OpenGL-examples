{-# LANGUAGE OverloadedStrings #-}

module Main where

-- General Haskell modules
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS

-- Import all OpenGL libraries qualified, for pedagogical reasons
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.Raw.Core31 (glUniformMatrix3fv)
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
    v <- U.makeShader GL.VertexShader vsSource
    f <- U.makeShader GL.FragmentShader fsSource

    p <- U.makeProgram [v, f] [ ("coord", GL.AttribLocation 0)
                              , ("color", GL.AttribLocation 1)
                              ]
    GL.currentProgram $= Just p

    U.printError
    return p

draw :: GL.Program -> GLFW.Window -> IO ()
draw p w = do
    (width, height) <- GLFW.getFramebufferSize w
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    let ratio = fromIntegral width / fromIntegral height

    GL.clear [GL.ColorBuffer]

    GL.currentProgram $= Just p
    GL.UniformLocation tLoc <- GL.get $ GL.uniformLocation p "transform"
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    V.unsafeWith vertices $ \ptr -> do
        GL.vertexAttribPointer (GL.AttribLocation 0) $=
            (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    V.unsafeWith colors $ \ptr -> do
        GL.vertexAttribPointer (GL.AttribLocation 1) $=
            (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)
    t <- GLFW.getTime
    V.unsafeWith (rotateM t) $ \ptr -> do
        glUniformMatrix3fv tLoc 1 1 ptr
    GL.drawArrays GL.Triangles 0 3
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Disabled


rotateM :: Maybe Double -> V.Vector GL.GLfloat
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
