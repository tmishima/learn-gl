import Control.Monad (unless, when)
--import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
  withWindow 640 480 "triangle" $ \win -> do
    putStrLn "have a window"
    GLFW.swapInterval 1
    GLFW.setKeyCallback win $ Just keyCb
    GLFW.setWindowCloseCallback win $ Just winCloseCb

    -- init gl
    vao <- GL.get GL.bindVertexArrayObject
    vtxBuf <- GL.get $ GL.bindBuffer GL.ArrayBuffer
    bufData <- GL.get $ GL.bufferData GL.ArrayBuffer
    -- bufData $= ()

    run win $ render win $ fromJust vtxBuf

  putStrLn "exiting"

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow w h t f = do
  GLFW.setErrorCallback $ Just errorCb
  r <- GLFW.init
  when r $ do
    GLFW.windowHint $ GLFW.WindowHint'Resizable False
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    m <- GLFW.createWindow w h t Nothing Nothing
    case m of
      (Just win) -> do
        GLFW.makeContextCurrent m
        f win
        GLFW.destroyWindow win
      Nothing -> return ()
    GLFW.terminate
  where errorCb e s = putStrLn $ unwords [show e, show s]

keyCb :: GLFW.KeyCallback
keyCb win key scan st mods = do
  case (key, st) of
    (GLFW.Key'Escape, GLFW.KeyState'Pressed) ->
      GLFW.setWindowShouldClose win True
    _ -> return ()

winCloseCb :: GLFW.WindowCloseCallback
winCloseCb win = GLFW.setWindowShouldClose win True

run :: GLFW.Window -> IO () -> IO ()
run win render = do
  GLFW.swapBuffers win
  GL.flush
  GLFW.pollEvents

  render

  q <- GLFW.windowShouldClose win
  unless q $ run win render

render :: GLFW.Window -> GL.BufferObject -> IO ()
render win vtxBuf = do
  return ()
