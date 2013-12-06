-- from schell/blocks

module Util.Shaders where

import           Graphics.Rendering.OpenGL
import           Control.Monad
import           System.IO (hPutStrLn, stderr)
import           System.Exit (exitFailure)
import qualified Data.ByteString as B

printError :: IO ()
printError = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)

makeShader :: ShaderType -> B.ByteString -> IO Shader
makeShader ty src = do
    s <- createShader ty
    shaderSourceBS s $= src
    compileShader s
    s'Ok <- get $ compileStatus s
    unless s'Ok $ do
        slog <- get $ shaderInfoLog s
        putStrLn $ "Log:" ++ slog
        exitFailure
    printError
    return s

makeProgram :: [Shader] -> [(String, AttribLocation)] -> IO Program
makeProgram shaders attributes = do
    p <- createProgram
    mapM_ (attachShader p) shaders
    mapM_ (\(name, loc) -> attribLocation p name $= loc) attributes
    linkProgram p
    p'Ok <- get $ linkStatus p
    validateProgram p
    status <- get $ validateStatus p
    unless (p'Ok && status) $ do
        plog <- get $ programInfoLog p
        putStrLn plog
        printError
        exitFailure
    return p

bindVBO :: BufferObject -> VertexArrayDescriptor a -> AttribLocation -> IO ()
bindVBO vbo dsc loc = do
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer loc $= (ToFloat, dsc)
    vertexAttribArray loc $= Enabled
