import Control.Monad (unless, when)
--import Control.Monad.IO.Class (liftIO)
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Linear
import Linear.Matrix
import qualified Numeric.Matrix as BFM

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
    ----GL.clearColor GL.$= GL.Color4 0.05 0.05 0.05 1
    --GL.normalize  GL.$= GL.Enabled
    polygonSmooth $= Enabled
    cullFace $= Just Back  --Just Front
    depthFunc $= Just Lequal
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    
    -- init shaders
    sp <- simpleShaderProgramWith "cubeTexWithLight.vert" "cubeTexWithLight.frag" $ \ p -> do
      attribLocation p "VertexPosition" $= AttribLocation 0
      attribLocation p "VertexColor" $= AttribLocation 1
      attribLocation p "VertexTexture" $= AttribLocation 2
      bindFragDataLocation p "FragColor" $= 0

    GL.currentProgram GL.$= Just (program sp)
    pLog <- GL.get $ GL.programInfoLog (program sp)
    putStrLn pLog

    texObj <- readTexture "sample_tex.png"
    tobj <- case texObj of
      Right to -> do textureBinding Texture2D $= Just to
                     return to 
      Left err -> error err
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
             >> texture2DWrap $= (Repeated, ClampToEdge)

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
      
      makeBuffer ArrayBuffer qTexCoord
      enableAttrib sp "VertexTexture"
      setAttrib sp "VertexTexture" ToFloat (VertexArrayDescriptor 2 Float 0 offset0)

      makeBuffer ArrayBuffer vnorm
      enableAttrib sp "VertexNormal"
      setAttrib sp "VertexNormal" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

      return ()

    GL.currentProgram GL.$= Nothing

    run win (render win sp qvao tobj) 0

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

render :: GLFW.Window -> ShaderProgram -> GL.VertexArrayObject -> TextureObject -> Int -> IO ()
render _ shprg qvao tex deg = do

  GL.currentProgram GL.$= Just (program shprg)
  --GL.clientState GL.VertexArray $= GL.Enabled

  let prjMat = projectionMatrix (deg2rad 60) 1.0 0.1 (10::GLfloat)
      cam = camMatrix $ dolly (V3 0 0 (4::GLfloat)) fpsCamera
      pvMat = prjMat !*! cam
      invPvMat = invM44 pvMat
      --rot r = V3 (V3 (cos r) 0 (sin r)) (V3 0 1 0) (V3 (-sin r) 0 (cos r)) -- rotY
      rot r = V3 (V3 1 0 0) (V3 0 (cos r) (-sin r)) (V3 0 (sin r) (cos r)) -- rotX
      pvUnifLoc = getUniform shprg "ProjViewMat"
      ipvUnifLoc = getUniform shprg "invProjViewMat"
      rUnifLoc = getUniform shprg "RotMat"
  asUniform pvMat pvUnifLoc 
  asUniform invPvMat ipvUnifLoc
  asUniform (rot (deg2rad (fromIntegral deg) ::GLfloat)) rUnifLoc
  setUniform shprg "Boxtex" (TextureUnit 0)
 
  withVAO qvao . withTextures2D [tex] $ 
    GL.drawElements GL.Quads (fromIntegral $ length cubeElem) UnsignedInt offset0

  GL.currentProgram GL.$= Nothing
  --return ()
  where

invM44 (V4 va vb vc vd) = case im of
    Just [la, lb, lc, ld] -> (V4 (l2v la) (l2v lb) (l2v lc) (l2v ld)) :: M44 GLfloat
    Nothing -> eye4 
  where
    im = fmap BFM.toList $ BFM.inv $ BFM.fromList ([ v2l va, v2l vb, v2l vc, v2l vd] :: [[Float]])
    v2l (V4 a b c d) = [realToFrac a,realToFrac b,realToFrac c,realToFrac d]
    l2v [a,b,c,d] = V4 (realToFrac a) (realToFrac b) (realToFrac c) (realToFrac d)

qColor :: [GL.GLfloat]
qColor = concat $ replicate 24 [ 1.0, 1.0, 1.0, 1.0 ]

p0,p1,p2,p3,p4,p5,p6,p7 :: [GLfloat]
p0 = [ -0.5, -0.5, -0.5]
p1 = [  0.5, -0.5, -0.5]
p2 = [  0.5, -0.5,  0.5]
p3 = [ -0.5, -0.5,  0.5]
p4 = [  0.5,  0.5,  0.5]
p5 = [ -0.5,  0.5,  0.5]
p6 = [ -0.5,  0.5, -0.5]
p7 = [  0.5,  0.5, -0.5]

cubeVert :: [GLfloat]
cubeVert = concat 
  [ p0, p1, p2 ,p3 -- Bottom
  , p7, p6, p5, p4 -- Top
  , p2, p4, p5, p3 -- Back
  , p0, p6, p7, p1 -- Front
  , p3, p5, p6, p0 -- Left
  , p1, p7, p4, p2 -- Right
  ]

vnorm :: [GLfloat]
vnorm = (f [  0, -1,  0 ] :: [GLfloat])
     ++ (f [  0,  1,  0 ] :: [GLfloat])
     ++ (f [  0,  0,  1 ] :: [GLfloat])
     ++ (f [  0,  0, -1 ] :: [GLfloat])
     ++ (f [ -1,  0,  0 ] :: [GLfloat])
     ++ (f [  1,  0,  0 ] :: [GLfloat])
  where
    f l = concat $ replicate 4 l

cubeElem :: [Word32]
cubeElem = [0 .. 23]

qTexCoord :: [GLfloat]
qTexCoord =
  [ 0.0, 0.0 , 0.0, 1.0 , 1.0, 1.0 , 1.0, 0.0
  , 0.0, 0.0 , 0.0, 1.0 , 1.0, 1.0 , 1.0, 0.0
  , 1.0, 1.0 , 1.0, 0.0 , 0.0, 0.0 , 0.0, 1.0 
  , 1.0, 1.0 , 1.0, 0.0 , 0.0, 0.0 , 0.0, 1.0 
  , 1.0, 1.0 , 1.0, 0.0 , 0.0, 0.0 , 0.0, 1.0 
  , 1.0, 1.0 , 1.0, 0.0 , 0.0, 0.0 , 0.0, 1.0 
  ]

