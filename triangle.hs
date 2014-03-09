import Control.Monad (unless, when)
--import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr (nullPtr)
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil

main :: IO ()
main = do
  withWindow 640 480 "triangle" $ \win -> do
    putStrLn "have a window"
    GLFW.swapInterval 1
    GLFW.setKeyCallback win $ Just keyCb
    GLFW.setWindowCloseCallback win $ Just winCloseCb

    -- init gl
    GL.clearColor $= GL.Color4 0.5 0.5 0.5 1

    --GL.position (GL.Light 0) GL.$= GL.Vertex4 5 5 10 0
    --GL.light    (GL.Light 0) GL.$= GL.Enabled
    --GL.lighting   GL.$= GL.Enabled
    --GL.cullFace   GL.$= Just GL.Back
    --GL.depthFunc  GL.$= Just GL.Less
    ----GL.clearColor GL.$= GL.Color4 0.05 0.05 0.05 1
    --GL.normalize  GL.$= GL.Enabled

    -- init shaders
    sp <- simpleShaderProgramWith "simple.vert" "simple.frag" $ \ p -> do
      attribLocation p "VertexPosition" $= AttribLocation 0
      bindFragDataLocation p "FragColor" $= 0

    GL.currentProgram GL.$= Just (program sp)
    pLog <- GL.get $ GL.programInfoLog (program sp)
    putStrLn pLog

    -- load objects
    tvao <- makeVAO $ do
      makeBuffer ArrayBuffer triangle
      enableAttrib sp "VertexPosition"
      setAttrib sp "VertexPosition" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)
      makeBuffer ArrayBuffer triColor 
      enableAttrib sp "VertexColor"
      setAttrib sp "VertexColor" ToFloat (VertexArrayDescriptor 4 Float 0 offset0)
      return ()

    qvao <- makeVAO $ do
      makeBuffer ArrayBuffer quads
      enableAttrib sp "VertexPosition"
      setAttrib sp "VertexPosition" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)
      makeBuffer ArrayBuffer qColor 
      enableAttrib sp "VertexColor"
      setAttrib sp "VertexColor" ToFloat (VertexArrayDescriptor 4 Float 0 offset0)
      return ()
    GL.currentProgram GL.$= Nothing

    run win $ render win sp tvao qvao

  putStrLn "exiting"

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow w h t f = do
  GLFW.setErrorCallback $ Just errorCb
  r <- GLFW.init
  when r $ do
    GLFW.windowHint $ GLFW.WindowHint'Resizable False
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    --GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
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
run win draw = do
  GLFW.swapBuffers win
  GL.flush
  GLFW.pollEvents

  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  draw

  q <- GLFW.windowShouldClose win
  unless q $ run win draw

render :: GLFW.Window -> ShaderProgram -> GL.VertexArrayObject -> GL.VertexArrayObject -> IO ()
render _ shprg tvao qvao = do

  GL.currentProgram GL.$= Just (program shprg)
  GL.clientState GL.VertexArray $= GL.Enabled

  let vUnifLoc = getUniform shprg "ViewMat"
  uniformMat vUnifLoc $= [[0.5,0,0,0],[0,0.5,0,0] ,[0,0,0.5,0],[0,0,0,1]]
  --asUniform vUnifLoc -- camMatrix camera2D

  setUniform shprg "TransMat" $ Vertex3 1 0 (0 :: GLfloat)
  withVAO tvao $ do
    GL.drawArrays GL.Triangles 0 $ fromIntegral (3::Int)
  
  setUniform shprg "TransMat" $ Vertex3 (-1) 0 (0 :: GLfloat)
  withVAO qvao $ do
    GL.drawArrays GL.Quads 0 $ fromIntegral (4::Int)

  GL.currentProgram GL.$= Nothing
  --return ()

triangle, triColor, quads, qColor :: [GL.GLfloat]
triColor = [
  1.0, 1.0, 0.0, 1.0,
  1.0, 0.0, 1.0, 1.0,
  0.0, 1.0, 1.0, 1.0]
triangle =
  [ -1.0, -1.0,  0.0
  ,  1.0, -1.0,  0.0
  ,  0.0,  1.0,  0.0]
qColor =
  [ 1.0, 0.0, 1.0, 1.0
  , 0.0, 1.0, 0.0, 1.0
  , 1.0, 0.0, 1.0, 1.0
  , 0.0, 1.0, 1.0, 1.0]
quads =
  [ -1.0, -1.0,  0.0
  ,  1.0, -1.0,  0.0
  ,  1.0,  1.0,  0.0
  , -1.0,  1.0,  0.0
  ]

