import Control.Monad (unless, when)
--import Control.Monad.IO.Class (liftIO)
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
--import Graphics.GLUtil.Linear
import Linear.V3
import Linear.V4
import Linear.Matrix

main :: IO ()
main = do
  withWindow 640 480 "triangle" $ \win -> do
    putStrLn "have a window"
    GLFW.swapInterval 1
    GLFW.setKeyCallback win $ Just keyCb
    GLFW.setWindowCloseCallback win $ Just winCloseCb

    -- init gl
    GL.clearColor $= GL.Color4 0.0 0.0 0.0 1

    --GL.position (GL.Light 0) GL.$= GL.Vertex4 5 5 10 0
    --GL.light    (GL.Light 0) GL.$= GL.Enabled
    --GL.lighting   GL.$= GL.Enabled
    --GL.cullFace   GL.$= Just GL.Back
    --GL.depthFunc  GL.$= Just GL.Less
    --depthFunc $= Just Lequal
    ----GL.clearColor GL.$= GL.Color4 0.05 0.05 0.05 1
    --GL.normalize  GL.$= GL.Enabled
    polygonSmooth $= Enabled
    cullFace $= Just Back  --Just Front Just FrontAndBack -- 
    depthFunc $= Just Less

    -- init shaders
    sp <- simpleShaderProgramWith "shadow.vert" "shadow.frag" $ \ p -> do
      attribLocation p "VertexPosition" $= AttribLocation 0
      attribLocation p "VertexNormal" $= AttribLocation 1
      bindFragDataLocation p "FragColor" $= 0

    GL.currentProgram GL.$= Just (program sp)
    pLog <- GL.get $ GL.programInfoLog (program sp)
    putStrLn pLog

    -- load objects
    qvao <- makeVAO $ do
      print "make cube Vao"
      makeBuffer ArrayBuffer cubeVert
      enableAttrib sp "VertexPosition"
      setAttrib sp "VertexPosition" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

      makeBuffer ArrayBuffer cubeNormal
      enableAttrib sp "VertexNormal"
      setAttrib sp "VertexNormal" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

      return ()

    pvao <- makeVAO $ do
      print "make plate Vao"
      makeBuffer ArrayBuffer plateVert
      enableAttrib sp "VertexPosition"
      setAttrib sp "VertexPosition" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

      makeBuffer ArrayBuffer plateNorm
      enableAttrib sp "VertexNormal"
      setAttrib sp "VertexNormal" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

    GL.currentProgram GL.$= Nothing

    (tbo,fbo) <- makeShadowBuff

    run win (render win sp qvao pvao (tbo,fbo)) 0

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

  --GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  draw deg

  q <- GLFW.windowShouldClose win
  unless q $ run win draw (deg + 1)

render :: GLFW.Window -> ShaderProgram -> GL.VertexArrayObject
       -> GL.VertexArrayObject
       -> (TextureObject,FramebufferObject) -> Int -> IO ()
render _ shprg qvao pvao (tbo,fbo) deg = do

  GL.currentProgram GL.$= Just (program shprg)
  GL.clientState GL.VertexArray $= GL.Enabled


  let prjMat = projectionMatrix (deg2rad 60) 1.0 0.1 (50::GLfloat)
      cam = camMatrix $ dolly (V3 0 0 (10::GLfloat)) fpsCamera
      --rot r = V3 (V3 (cos r) 0 (sin r)) (V3 0 1 0) (V3 (-sin r) 0 (cos r)) -- rotY
      --rot r = V3 (V3 1 0 0) (V3 0 (cos r) (-sin r)) (V3 0 (sin r) (cos r)) -- rotX
      vUnifLoc = getUniform shprg "ViewMat"
      pUnifLoc = getUniform shprg "ProjMat"
      rUnifLoc = getUniform shprg "RotMat"
      -- for Shadow
      --mvUnifLoc = getUniform shprg "ModelViewMatrix"
      nrUnifLoc = getUniform shprg "NormalMatrix"
      sdUnifLoc = getUniform shprg "ShadowMatrix"
      swUnifLoc = getUniform shprg "shadowSW"
      --

  --asUniform (rot (deg2rad (fromIntegral deg) ::GLfloat)) rUnifLoc

  let sprjMat = projectionMatrix (deg2rad 60) 1.0 0.1 (20::GLfloat)
      sun = camMatrix $ tilt (-90) $ dolly (V3 0 (10::GLfloat) 0) fpsCamera
      --sun = camMatrix $ dolly (V3 0 10 (0::GLfloat)) fpsCamera

  --asUniform sun mvUnifLoc 
  asUniform (sprjMat !*! sun)  sdUnifLoc
  asUniform (V3 (V3 0 1 0) (V3 0 1 0) (V3 0 1 (0::GLfloat))) nrUnifLoc

  -- shadow
  viewport $= (Position 0 0, Size 512 512)
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  uniformScalar swUnifLoc $= (0::GLint)
  bindFramebuffer Framebuffer $= fbo

  withVAO qvao $ 
    drawArrays Quads 0 $ fromIntegral $ length cubeVert

  withVAO pvao $ 
    drawArrays Quads 0 $ fromIntegral $ length plateVert

  -- View
  viewport $= (Position 0 0, Size 640 480)
  asUniform cam vUnifLoc 
  --asUniform sun vUnifLoc 
  asUniform prjMat pUnifLoc 
  --asUniform sprjMat pUnifLoc 
  asUniform (V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 (1::GLfloat))) rUnifLoc

  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  uniformScalar swUnifLoc $= (1::GLint)
  bindFramebuffer Framebuffer $= defaultFramebufferObject

  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just tbo 

  withVAO qvao $ 
    drawArrays Quads 0 $ fromIntegral $ length cubeVert

  withVAO pvao $ 
    drawArrays Quads 0 $ fromIntegral $ length plateVert
  
  GL.currentProgram GL.$= Nothing
  --return ()

shadowMapSize :: TextureSize2D
shadowMapSize = TextureSize2D 512 512

makeShadowBuff :: IO (TextureObject,FramebufferObject)
makeShadowBuff = do
  b <- genObjectName :: IO TextureObject
  textureBinding Texture2D $= Just b
  texImage2D Texture2D NoProxy 0 DepthComponent'
             shadowMapSize 0
             (PixelData DepthComponent UnsignedByte offset0)  -- nullPtr)
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Repeated, ClampToBorder)
  textureWrapMode Texture2D T $= (Repeated, ClampToBorder)
  textureBorderColor Texture2D $= Color4 1.0 0.0 0.0 (0.0::GLfloat)
  textureCompareMode Texture2D $= Just Less

  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just b

  f <- genObjectName :: IO FramebufferObject
  bindFramebuffer Framebuffer $= f
  framebufferTexture2D Framebuffer DepthAttachment Texture2D b 0
  drawBuffers $= [NoBuffers]

  bindFramebuffer Framebuffer $= defaultFramebufferObject

  return (b,f)

cubeVert, {-qColor,-} cubeNormal :: [GL.GLfloat]
{- qColor =
  [ 1.0, 0.0, 1.0, 1.0
  , 0.0, 1.0, 0.0, 1.0
  , 0.0, 1.0, 1.0, 1.0
  , 0.0, 1.0, 0.0, 1.0
  , 1.0, 0.0, 1.0, 1.0
  , 0.0, 1.0, 1.0, 1.0
  , 1.0, 0.0, 1.0, 1.0
  , 0.0, 1.0, 1.0, 1.0
  ] -}
cubeNormal = concat
  -- Top
  [ yp, yp, yp, yp  
  -- Bottom
  , yn, yn, yn, yn
  -- Front
  , zn, zn, zn, zn 
  -- Back
  , zp, zp, zp, zp 
  -- Right
  , xp, xp, xp, xp 
  -- Left
  , xn, xn, xn, xn  
  ]
  where
    xp = [ 1.0,  0.0,  0.0]
    xn = [-1.0,  0.0,  0.0]
    yp = [ 0.0,  1.0,  0.0]
    yn = [ 0.0, -1.0,  0.0]
    zp = [ 0.0,  0.0,  1.0]
    zn = [ 0.0,  0.0, -1.0]
cubeVert = concatMap (\ (x,y,z) -> [x,y,z])
  -- Top
  [ p7,p6,p5,p4
  -- Bottom
  , p0,p1,p2,p3 
  -- Front
  , p6,p7,p1,p0
  -- Back
  , p4,p5,p3,p2
  -- Right
  , p7,p4,p2,p1
  -- Left
  , p5,p6,p0,p3
  ]
  where
    (p0:p1:p2:p3:p4:p5:p6:p7:_) = blockNodeVertex

type VrtxPos3D = (GLfloat,GLfloat,GLfloat)
blockNodeVertex :: [VrtxPos3D]
blockNodeVertex = 
  [ ( -0.5, -0.5, -0.5) -- P0
  , (  0.5, -0.5, -0.5) -- P1
  , (  0.5, -0.5,  0.5) -- P2
  , ( -0.5, -0.5,  0.5) -- P3
  , (  0.5,  0.5,  0.5) -- P4
  , ( -0.5,  0.5,  0.5) -- P5
  , ( -0.5,  0.5, -0.5) -- P6
  , (  0.5,  0.5, -0.5) -- P7
  , (  0.5,  0.0,  0.5) -- P8
  , ( -0.5,  0.0,  0.5) -- P9
  , ( -0.5,  0.0, -0.5) -- P10
  , (  0.5,  0.0, -0.5) -- P11
  ]

plateVert, plateNorm :: [GL.GLfloat]
{-pColor =pColor, 
  [ 0.3, 0.3, 0.3, 1.0
  , 0.3, 0.3, 0.3, 1.0
  , 0.3, 0.3, 0.3, 1.0
  , 0.3, 0.3, 0.3, 1.0
  ]
plateVert =
  [ -5.5, -2.5, -5.5
  ,  5.5, -2.5, -5.5
  ,  5.5, -2.5,  5.5
  , -5.5, -2.5,  5.5
  ] -}
plateVert =
  [ -5.5, -1.5,  5.5
  ,  5.5, -1.5,  5.5
  ,  5.5, -1.5, -5.5
  , -5.5, -1.5, -5.5
  ]
plateNorm =
  [  0.0,  1.0,  0.0
  ,  0.0,  1.0,  0.0
  ,  0.0,  1.0,  0.0
  ,  0.0,  1.0,  0.0
  ]

