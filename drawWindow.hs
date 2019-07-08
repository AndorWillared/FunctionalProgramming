import qualified Graphics.UI.Gtk as G
import Graphics.Rendering.Cairo
import Data.IORef
import Control.Monad.State
import Data.Matrix as M
import Codec.Picture.Png
import Codec.Picture.Types

drawCoordianteSpot :: Double -> Int -> Int -> Render ()
drawCoordianteSpot rgb i j =
  do
    rectangle (fromIntegral (12*i + 1)) (fromIntegral(12 * j + 1)) 11.0 11.0
    setSourceRGB rgb rgb rgb
    fill

drawCoordinates :: M.Matrix Int -> Render ()
drawCoordinates m =
  forM_ [0..27] $ \y -> do 
    forM_ [0..27] $ \x -> do
      drawCoordianteSpot (fromIntegral (M.getElem (x+1) (y+1) m)) x y

destroyEventHandler :: IO ()
destroyEventHandler =
  do G.mainQuit

updateCanvas :: G.DrawingArea -> IO Bool
updateCanvas canvas = do
  G.widgetQueueDraw canvas
  liftIO (putStrLn ("draw"))
  return False
  
convert' :: Int -> Int
convert' value = 1 + (value `quot` 12)

getPixel :: Int -> Int -> M.Matrix Int -> Pixel8
getPixel x y matrix = (fromIntegral(M.getElem (x+1) (y+1) matrix))

imageCreator :: String -> M.Matrix Int -> IO()
imageCreator path matrix = writePng path $ generateImage pixelRenderer 28 28
      where pixelRenderer x y = PixelRGBA8 (getPixel x y matrix) (getPixel x y matrix) (getPixel x y matrix) 255

main ::  IO ()
main =
  do
    mouseClickState <- newIORef False
    drawSpots' <- newIORef (M.zero 28 28)
    G.initGUI
    window <- G.windowNew
    vBox <- G.vBoxNew False 3
    paintArea <- G.drawingAreaNew
    saveBtn <- G.buttonNew
    predictBtn <- G.buttonNew
    resetBtn <- G.buttonNew

    
    G.set saveBtn [ G.buttonLabel G.:= "save" ]
    G.on saveBtn G.buttonActivated $ do
      localDrawSpots' <- liftIO $ (readIORef drawSpots')
      imageCreator "./wow.png" localDrawSpots'
      return ()
    
    G.set predictBtn [ G.buttonLabel G.:= "predict" ]
    G.on predictBtn G.buttonActivated $ do
      liftIO (putStrLn ("is called 2"))
      return ()

    G.set resetBtn [ G.buttonLabel G.:= "resetBtn" ]
    G.on resetBtn G.buttonActivated $ do
      liftIO $ writeIORef drawSpots' (M.zero 28 28)              
      liftIO (updateCanvas paintArea)
      liftIO (putStrLn ("reset called"))
      return ()

    displayField <- G.labelNew (Just " ")
    G.set window [G.windowDefaultWidth G.:= 337,
                G.windowDefaultHeight G.:= 477,
                G.windowWindowPosition G.:= G.WinPosCenter,
                G.containerChild G.:= vBox,
                G.windowTitle G.:= "Mouse Movement Demo"]
    G.widgetAddEvents paintArea [G.PointerMotionMask, G.EnterNotifyMask, G.LeaveNotifyMask, G.ButtonPressMask, G.ButtonReleaseMask]
    G.on paintArea G.enterNotifyEvent $ do
        liftIO $ G.labelSetText displayField "drawingArea entered"
        return False
    G.on paintArea G.leaveNotifyEvent $ do
        liftIO $ G.labelSetText displayField "drawingArea left"
        return False
    G.on paintArea G.buttonPressEvent $ do
        bttnId <- G.eventButton
        liftIO $ writeIORef mouseClickState True
        return False
    G.on paintArea G.buttonReleaseEvent $ do
        liftIO $ writeIORef mouseClickState False
        return False
    G.on paintArea G.motionNotifyEvent $ do
        localMouseClicked <- liftIO $ (readIORef mouseClickState)
        if localMouseClicked
            then do
              (x,y) <- liftIO ( G.widgetGetPointer paintArea)
              localDrawSpots' <- liftIO $ (readIORef drawSpots')
              liftIO $ writeIORef drawSpots' (M.setElem 255 (convert' x, convert' y) localDrawSpots')
              liftIO (updateCanvas paintArea)
            else
              return False
        return False
    G.on paintArea G.draw $ do
        localDrawSpots' <- liftIO $ (readIORef drawSpots')
        drawCoordinates localDrawSpots'
        return ()
    G.on window G.configureEvent $ do
           (w, h) <- G.eventSize
           liftIO (putStrLn (show w ++ "x" ++ show h))
           return False
    G.on window G.objectDestroy destroyEventHandler
    G.boxPackStart vBox displayField G.PackNatural 0
    G.boxPackStart vBox saveBtn G.PackNatural 0
    G.boxPackStart vBox predictBtn G.PackNatural 0
    G.boxPackStart vBox resetBtn G.PackNatural 0
    G.boxPackStart vBox paintArea G.PackGrow 1
    G.widgetShow displayField
    G.widgetShow saveBtn
    G.widgetShow predictBtn
    G.widgetShow resetBtn
    G.widgetShow paintArea
    G.widgetShow vBox
    G.widgetShow window
    G.mainGUI