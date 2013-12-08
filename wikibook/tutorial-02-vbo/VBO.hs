-- | GLUtil provides functions to load shaders, so we do not need to
-- write our own, as the C++ tutorial does.
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

-- Local modules
import qualified Util.GLFW as W

main :: IO ()
main = do
    -- GLFW code will be the same in all variants
    win <- W.initialize "My First Triangle"
    prog <- initResources
    W.mainLoop (draw prog win) win
    W.cleanup win

initResources :: IO Program
initResources = do
    -- compile vertex shader
    vs <- U.loadShader GL.VertexShader $ shaderPath </> "triangle.v.glsl"
    fs <- U.loadShader GL.FragmentShader $ shaderPath </> "triangle.f.glsl"
    p <- U.linkShaderProgram [vs, fs]
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    Program p <$> GL.get (GL.attribLocation p "coord2d")
              <*> U.makeBuffer GL.ArrayBuffer vertices

draw :: Program -> GLFW.Window -> IO ()
draw (Program program attrib buf) win = do
    GL.clearColor $= GL.Color4 1 1 1 1
    GL.clear [GL.ColorBuffer]
    -- In C++ example GLUT handles this?
    (width, height) <- GLFW.getFramebufferSize win
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    GL.currentProgram $= Just program
    GL.vertexAttribArray attrib $= GL.Enabled
    GL.bindBuffer GL.ArrayBuffer $= Just buf
    GL.vertexAttribPointer attrib $=
        (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0)
    GL.drawArrays GL.Triangles 0 3 -- 3 is the number of vertices
    GL.vertexAttribArray attrib $= GL.Disabled

-- | Represents the shader program and its input parameter
data Program = Program GL.Program GL.AttribLocation GL.BufferObject

shaderPath :: FilePath
shaderPath = "wikibook" </> "tutorial-02-vbo"

vertices :: [Float]
vertices = [  0.0,  0.8
           , -0.8, -0.8
           ,  0.8, -0.8
           ]
