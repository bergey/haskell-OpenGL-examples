-- | Linear provides us with matrices and suitable operators, similar
-- to what the C++ version of the tutorial does with libGLM.  GLUtil
-- marshals those types to OpenGL.

{-# LANGUAGE OverloadedStrings #-}

module Main where

-- General Haskell modules
import           Control.Applicative
import           System.FilePath ((</>))

-- Import all OpenGL libraries qualified, for pedagogical reasons
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Linear as L

-- Local modules
import qualified Util.GLFW as W

main :: IO ()
main = do
    -- GLFW code will be the same in all variants
    win <- W.initialize "My First Cube"
    prog <- initResources
    W.mainLoop (draw prog win) win
    W.cleanup win

initResources :: IO Resources
initResources = do
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    -- As our shaders take more inputs, collecting the attributes gets
    -- annoying.  GLUtil helps out with the ShaderProgram type, which
    -- keeps track of the 'AttribLocations' and 'UniformLocation's by
    -- name.
    let v = shaderPath </> "cube.v.glsl"
        f = shaderPath </> "cube.f.glsl"
    print vertices
    Resources <$> U.simpleShaderProgram v f
              <*> U.fromSource GL.ArrayBuffer vertices
              <*> U.fromSource GL.ArrayBuffer colors
              <*> U.fromSource GL.ElementArrayBuffer elements

draw :: Resources -> GLFW.Window -> IO ()
draw r win = do
    GL.clearColor $= GL.Color4 1 1 1 1
    GL.depthFunc $= Just GL.Less
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    -- In C++ example GLUT handles resizing the viewport?
    (width, height) <- GLFW.getFramebufferSize win
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    t <- maybe 0 id <$> GLFW.getTime -- time in seconds since program launch
    GL.currentProgram $= (Just . U.program . shaderProgram $ r)
    U.enableAttrib (shaderProgram r) "coord3d"
    U.enableAttrib (shaderProgram r) "v_color"
    GL.bindBuffer GL.ArrayBuffer $= Just (vertBuffer r)
    U.setAttrib (shaderProgram r) "coord3d"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
    GL.bindBuffer GL.ArrayBuffer $= Just (colorBuffer r)
    U.setAttrib (shaderProgram r) "v_color"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
    U.asUniform (transformM width height t) $ U.getUniform (shaderProgram r) "mvp"
    GL.bindBuffer GL.ElementArrayBuffer $= Just (elementBuffer r)
    U.drawIndexedTris (fromIntegral $ length elements)
    -- GL.drawArrays GL.Triangles 0 3 -- 3 is the number of vertices
    -- GLUtil does not yet provide a function to disable attributes
    GL.vertexAttribArray (U.getAttrib (shaderProgram r) "coord3d") $= GL.Disabled
    GL.vertexAttribArray (U.getAttrib (shaderProgram r) "v_color") $= GL.Disabled

-- | Represents the shader program and its input buffers
data Resources = Resources { shaderProgram :: U.ShaderProgram
                           , vertBuffer :: GL.BufferObject
                           , colorBuffer :: GL.BufferObject
                           , elementBuffer :: GL.BufferObject
                           }

transformM :: Int -> Int -> Double -> L.M44 GL.GLfloat
transformM width height t = projection L.!*! view L.!*! model L.!*! anim where
  angle      = realToFrac t * pi/4
  anim       = L.mkTransformation (L.axisAngle (L.V3 0 1 0) angle) L.zero
  model      = L.mkTransformationMat L.eye3 $ L.V3 0 0 (-4)
  view       = U.camMatrix cam
  cam        = U.tilt (-30) . U.dolly (L.V3 0 2 0) $ U.fpsCamera
  projection = U.projectionMatrix (pi/4) aspect 0.1 10
  aspect     = fromIntegral width / fromIntegral height

shaderPath :: FilePath
shaderPath = "wikibook" </> "tutorial-05-3D"

-- This does not result in the same face order as the C++ example.
-- The first 4 vertices correspond to the right (positive X) face.
vertices :: [L.V3 Float]
vertices = L.V3 <$> [1, -1] <*> [1, -1] <*> [1, -1]

colors :: [L.V3 Float]
colors = vertices -- color space visualization

-- Vertices for each triangle in CCW order
elements :: [L.V3 GL.GLuint]
elements = [ L.V3 2 1 0 -- right
           , L.V3 1 2 3
           , L.V3 0 1 4 -- top
           , L.V3 4 1 5
           , L.V3 4 5 6 -- left
           , L.V3 7 6 5
           , L.V3 2 6 3 -- bottom
           , L.V3 6 3 7
           , L.V3 0 4 2 -- front
           , L.V3 2 4 6
           , L.V3 5 1 7 -- back
           , L.V3 7 1 3
           ]
