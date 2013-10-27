import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (unless, when)

main :: IO ()
main = do
  withWindow 640 480 "test" $ \win -> do
    putStrLn "have a window"
    GLFW.swapInterval 1
    GLFW.setKeyCallback win $ Just keyCb

    run win

  putStrLn "complete"

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow w h t f = do
  GLFW.setErrorCallback $ Just errorCb
  r <- GLFW.init
  when r $ do
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
  case key of
    GLFW.Key'Escape -> do
      putStrLn "exiting"
      GLFW.setWindowShouldClose win True
    _ -> return ()

run :: GLFW.Window -> IO ()
run win = do
  GLFW.swapBuffers win
  GL.flush
  GLFW.pollEvents

  q <- GLFW.windowShouldClose win
  unless q $ run win
