-- | This variant of Tutorial #3 keeps geometry and colors in the,
-- same list, and sets the VertexArrayDescriptor to separate them.
-- This probably isn't a great style, since it makes it hard to add
-- type safety later.
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- General Haskell modules
import           Control.Applicative
import           Foreign.Storable as F
import           System.FilePath ((</>))

-- Import all OpenGL libraries qualified, for pedagogical reasons
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U

-- Local modules
import qualified Util.GLFW as W

main :: IO ()
main = do
    -- GLFW code will be the same in all variants
    win <- W.initialize "My First Triangle"
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
    let v = shaderPath </> "triangle.v.glsl"
        f = shaderPath </> "triangle.f.glsl"
    Resources <$> U.simpleShaderProgram v f
              <*> U.makeBuffer GL.ArrayBuffer interleaved

draw :: Resources -> GLFW.Window -> IO ()
draw r win = do
    GL.clearColor $= GL.Color4 1 1 1 1
    GL.clear [GL.ColorBuffer]
    -- In C++ example GLUT handles resizing the viewport?
    (width, height) <- GLFW.getFramebufferSize win
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    t <- maybe 0 id <$> GLFW.getTime -- time in seconds since program launch
    let fade :: GL.Index1 GL.GLfloat
        fade = GL.Index1 . realToFrac $ sin (t * 2 * pi / 5) / 2 + 0.5

    GL.currentProgram $= (Just . U.program . triProgram $ r)
    -- More helpers from GLUtil, equivalent to:
    -- GL.vertexAttribArray coord2d $= GL.Enabled
    -- GL.vertexAttribArray v_color $= GL.Enabled
    U.enableAttrib (triProgram r) "coord2d"
    U.enableAttrib (triProgram r) "v_color"
    GL.bindBuffer GL.ArrayBuffer $= Just (attrBuffer r)
    U.setAttrib (triProgram r) "coord2d"
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float (5 * floatSize) U.offset0
    U.setAttrib (triProgram r) "v_color"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float (5 * floatSize)
          (U.offsetPtr $ 2 * floatSize)
    U.setUniform (triProgram r) "fade" fade
    GL.drawArrays GL.Triangles 0 3 -- 3 is the number of vertices
    -- GLUtil does not yet provide a function to disable attributes
    GL.vertexAttribArray (U.getAttrib (triProgram r) "coord2d") $= GL.Disabled
    GL.vertexAttribArray (U.getAttrib (triProgram r) "v_color") $= GL.Disabled

-- | Represents the shader program and its input buffers
data Resources = Resources { triProgram :: U.ShaderProgram
                           , attrBuffer :: GL.BufferObject
                           }

floatSize :: Num a => a
floatSize = fromIntegral $ F.sizeOf (undefined :: GL.GLfloat)

shaderPath :: FilePath
shaderPath = "wikibook" </> "tutorial-03-attributes"

interleaved :: [Float]
interleaved = [  0.0,  0.8, 1, 1, 0
              , -0.8, -0.8, 0, 0, 1
              ,  0.8, -0.8, 1, 0, 0
              ]
