import Control.Monad (unless, when)
--import Control.Monad.IO.Class (liftIO)
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
--import Graphics.GLUtil.Linear
import Linear.V3

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
    polygonSmooth $= Enabled
    cullFace $= Just Back  --Just Front
    depthFunc $= Just Lequal

    -- init shaders
    sp <- simpleShaderProgramWith "cube.vert" "simple.frag" $ \ p -> do
      attribLocation p "VertexPosition" $= AttribLocation 0
      bindFragDataLocation p "FragColor" $= 0

    GL.currentProgram GL.$= Just (program sp)
    pLog <- GL.get $ GL.programInfoLog (program sp)
    putStrLn pLog

    -- load objects
    qvao <- makeVAO $ do
      print "make Vao"
      makeBuffer ArrayBuffer cubeVert
      enableAttrib sp "VertexPosition"
      setAttrib sp "VertexPosition" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

      bufferIndices cubeElem

      makeBuffer ArrayBuffer qColor 
      enableAttrib sp "VertexColor"
      setAttrib sp "VertexColor" ToFloat (VertexArrayDescriptor 4 Float 0 offset0)

      return ()

    GL.currentProgram GL.$= Nothing

    run win (render win sp qvao) 0

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

run :: GLFW.Window -> (Int -> IO ()) -> Int -> IO ()
run win draw deg = do
  GLFW.swapBuffers win
  GL.flush
  GLFW.pollEvents

  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  draw deg

  q <- GLFW.windowShouldClose win
  unless q $ run win draw (deg + 1)

render :: GLFW.Window -> ShaderProgram -> GL.VertexArrayObject -> Int -> IO ()
render _ shprg qvao deg = do

  GL.currentProgram GL.$= Just (program shprg)
  GL.clientState GL.VertexArray $= GL.Enabled

  let prjMat = projectionMatrix (deg2rad 60) 1.0 0.1 (10::GLfloat)
      cam = camMatrix $ dolly (V3 0 0 (4::GLfloat)) fpsCamera
      --rot r = V3 (V3 (cos r) 0 (sin r)) (V3 0 1 0) (V3 (-sin r) 0 (cos r)) -- rotY
      rot r = V3 (V3 1 0 0) (V3 0 (cos r) (-sin r)) (V3 0 (sin r) (cos r)) -- rotX
      vUnifLoc = getUniform shprg "ViewMat"
      pUnifLoc = getUniform shprg "ProjMat"
      rUnifLoc = getUniform shprg "RotMat"
  asUniform cam vUnifLoc 
  asUniform prjMat pUnifLoc 
  asUniform (rot (deg2rad (fromIntegral deg) ::GLfloat)) rUnifLoc
 
  withVAO qvao $ 
    GL.drawElements GL.Quads (fromIntegral $ length cubeElem) UnsignedInt offset0

  GL.currentProgram GL.$= Nothing
  --return ()

cubeVert, qColor :: [GL.GLfloat]
qColor =
  [ 1.0, 0.0, 1.0, 1.0
  , 0.0, 1.0, 0.0, 1.0
  , 0.0, 1.0, 1.0, 1.0
  , 0.0, 1.0, 0.0, 1.0
  , 1.0, 0.0, 1.0, 1.0
  , 0.0, 1.0, 1.0, 1.0
  , 1.0, 0.0, 1.0, 1.0
  , 0.0, 1.0, 1.0, 1.0
  ]
cubeVert =
  [ -0.5, -0.5, -0.5
  ,  0.5, -0.5, -0.5
  ,  0.5, -0.5,  0.5
  , -0.5, -0.5,  0.5
  ,  0.5,  0.5,  0.5
  , -0.5,  0.5,  0.5
  , -0.5,  0.5, -0.5
  ,  0.5,  0.5, -0.5
  ]
cubeElem :: [Word32]
cubeElem =
  [ 0, 1, 2, 3
  , 4, 7, 6, 5
  , 4, 5, 3, 2
  , 6, 7, 1, 0
  , 5, 6, 0, 3
  , 7, 4, 2, 1
  ]

