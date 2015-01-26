-- | Linear provides us with matrices and suitable operators, similar
-- to what the C++ version of the tutorial does with libGLM.  GLUtil
-- marshals those types to OpenGL.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

-- General Haskell modules
import           Control.Applicative
import qualified Data.ByteString as BS
import           Data.FileEmbed (embedFile)

-- Import all OpenGL libraries qualified, for pedagogical reasons
import qualified Data.Vinyl as V
import           Data.Vinyl ((=:), (<+>))
import Data.Vinyl.Derived (SField(..))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Graphics.VinylGL as VG
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

vSrc, fSrc :: BS.ByteString
-- TH arguments need to be literal strings, or defined in another module
-- In real code, the latter would be better to avoid platform-dependent '/'
vSrc = $(embedFile $ "wikibook/tutorial-05-3D/cube.v.glsl")
fSrc = $(embedFile $ "wikibook/tutorial-05-3D/cube.f.glsl")

initResources :: IO Resources
initResources = do
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    Resources <$> U.simpleShaderProgramBS vSrc fSrc
              <*> VG.bufferVertices (zipWith (<+>) vertices colors)
              <*> U.fromSource GL.ElementArrayBuffer elements

draw :: Resources -> GLFW.Window -> IO ()
draw (Resources s vb e) win = do
    -- could probably move this to init function
    GL.clearColor $= GL.Color4 1 1 1 1
    GL.depthFunc $= Just GL.Less
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    -- get some state for the transforms
    (width, height) <- GLFW.getFramebufferSize win
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    t <- maybe 0 id <$> GLFW.getTime -- time in seconds since program launch
    -- actually set up OpenGL
    GL.currentProgram $= (Just . U.program $ s)
    VG.enableVertices' s vb
    VG.bindVertices vb
    VG.setAllUniforms s $ transformM width height t
    GL.bindBuffer GL.ElementArrayBuffer $= Just e
    U.drawIndexedTris (fromIntegral $ length elements)

-- | Represents the shader program and its input buffers
data Resources = Resources { shaderProgram :: U.ShaderProgram
                           , buffer :: VG.BufferedVertices [Pos, Color]
                           , elementBuffer :: GL.BufferObject
                           }

transformM :: Int -> Int -> Double -> V.FieldRec '[MVP]
transformM width height t =
              mvp =: (projection L.!*! view L.!*! model L.!*! anim)
  where
    angle      = realToFrac t * pi/4
    anim       = L.mkTransformation (L.axisAngle (L.V3 0 1 0) angle) L.zero
    model      = L.mkTransformationMat L.eye3 $ L.V3 0 0 (-4)
    view       = U.camMatrix cam
    cam        = U.tilt (-30) . U.dolly (L.V3 0 2 0) $ U.fpsCamera
    projection = U.projectionMatrix (pi/4) aspect 0.1 10
    aspect     = fromIntegral width / fromIntegral height

-- This does not result in the same face order as the C++ example.
-- The first 4 vertices correspond to the right (positive X) face.
vertices :: [V.FieldRec '[Pos]]
vertices = map (coord3d =:) $ L.V3 <$> [1, -1] <*> [1, -1] <*> [1, -1]

colors :: [V.FieldRec '[Color]]
colors = map (v_color =:) $ L.V3 <$> [1, -1] <*> [1, -1] <*> [1, -1]  -- color space visualization

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


type Pos = '("coord3d", L.V3 GL.GLfloat)
type Color = '("v_color", L.V3 GL.GLfloat)
type MVP = '("mvp", L.M44 GL.GLfloat)

coord3d  :: SField Pos
coord3d = SField
v_color :: SField Color
v_color = SField
mvp :: SField MVP
mvp = SField
