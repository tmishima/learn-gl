import Control.Monad (unless, when)
--import Control.Monad.IO.Class (liftIO)
import Data.Array.Storable
import Data.Maybe (fromJust)
import Foreign.Ptr (nullPtr)
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
    GL.clearColor GL.$= GL.Color4 0.5 0.5 0.5 1
    vao <- GL.get GL.bindVertexArrayObject
    tri <- listToVbo triangle

    run win $ render win tri

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
--keyCb win key scan st mods = do
keyCb win key _ st _ = do
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
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.clientState GL.VertexArray $= GL.Enabled
  GL.bindBuffer GL.ArrayBuffer $= Just vtxBuf
  GL.arrayPointer GL.VertexArray $= (GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)
  GL.drawArrays GL.Triangles 0 $ fromIntegral 3
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  GL.clientState GL.VertexArray $= GL.Disabled
  return ()

listToVbo :: [GL.GLfloat] -> IO GL.BufferObject
listToVbo xs = do
  let len = length xs
      ptrsize = toEnum $ len * 4
  [array] <- GL.genObjectNames 1
  GL.bindBuffer GL.ArrayBuffer $= Just array
  arr <- newListArray (0, len - 1) xs
  withStorableArray arr $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (ptrsize, ptr, GL.StaticDraw)
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  return array

triangle :: [GL.GLfloat]
triangle = [
  -1.0, -1.0,  0.0,
   1.0, -1.0,  0.0,
   0.0,  1.0,  0.0]
