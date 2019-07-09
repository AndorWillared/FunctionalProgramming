import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as C
import Data.IORef
import Control.Monad.State
import Data.Matrix as M
import Codec.Picture.Png
import Codec.Picture.Types
import Data.Text

-- constants_begin
matrix_width = 28
matrix_height = 28
tile_size = 24
tile_size_sub_1 = 23.0
least_opacity = 255
window_width = matrix_width * tile_size + 200
window_height = matrix_width * tile_size
placeholder = "aasdf"
-- constants_end 

drawCoordianteSpot :: Double -> Int -> Int -> C.Render ()
drawCoordianteSpot rgb i j =
  do
    C.rectangle (fromIntegral (tile_size*i + 1)) (fromIntegral(tile_size * j + 1)) tile_size_sub_1 tile_size_sub_1
    C.setSourceRGB rgb rgb rgb
    C.fill

drawCoordinates :: M.Matrix Int -> C.Render ()
drawCoordinates m =
  forM_ [0..(matrix_height-1)] $ \y -> do 
    forM_ [0..(matrix_width-1)] $ \x -> do
      drawCoordianteSpot (fromIntegral (M.getElem (x+1) (y+1) m)) x y

destroyEventHandler :: IO ()
destroyEventHandler =
  do mainQuit

updateCanvas :: DrawingArea -> IO Bool
updateCanvas canvas = do
  widgetQueueDraw canvas
  return False
  
convert' :: Int -> Int
convert' value = 1 + (value `quot` tile_size)

getPixel :: Int -> Int -> M.Matrix Int -> Pixel8
getPixel x y matrix = (fromIntegral(M.getElem (x+1) (y+1) matrix))

imageCreator :: String -> M.Matrix Int -> IO()
imageCreator path matrix = writePng path $ generateImage pixelRenderer matrix_width matrix_height
      where pixelRenderer x y = PixelRGBA8 (getPixel x y matrix) (getPixel x y matrix) (getPixel x y matrix) least_opacity

toInt :: Double -> Int
toInt = round

main :: IO ()
main =
  do
    mouseClickState <- newIORef False
    drawSpots' <- newIORef (M.zero matrix_width matrix_height)
    initGUI
    vBox <- vBoxNew False 1
    hBox <- hBoxNew False 2
    hBoxBottom <- hBoxNew False 2
    vBoxBottom <- vBoxNew False 1

    paintArea <- drawingAreaNew
    displayField <- labelNew (Just " ")

    spinBtn <- spinButtonNewWithRange 0 999999 1
    --set spinBtn [ buttonLabel := "iteration" ]

    entryField <- entryNew
    set entryField [ entryPlaceholderText := Just("class") ]

    saveBtn <- buttonNew
    set saveBtn [ buttonLabel := "save" ]
    on saveBtn buttonActivated $ do
      classVal <- entryGetText entryField
      iterationVal <- spinButtonGetValue spinBtn
      localDrawSpots' <- liftIO $ (readIORef drawSpots')
      imageCreator (classVal ++"_" ++ (show $ toInt iterationVal) ++ ".png") localDrawSpots'
      spinButtonSetValue spinBtn (iterationVal+1)
      return ()
    
    predictBtn <- buttonNew
    set predictBtn [ buttonLabel := "predict" ]
    on predictBtn buttonActivated $ do
      liftIO $ labelSetText displayField "predict called"
      return ()

    resetBtn <- buttonNew
    set resetBtn [ buttonLabel := "resetBtn" ]
    on resetBtn buttonActivated $ do
      liftIO $ writeIORef drawSpots' (M.zero matrix_width matrix_height)              
      liftIO (updateCanvas paintArea)
      liftIO (putStrLn ("reset called"))
      return ()

    
    window <- windowNew
    set window [windowDefaultWidth := window_height,
                windowDefaultHeight := window_width,
                windowWindowPosition := WinPosCenter,
                containerChild := vBox,
                windowTitle := "Neural Network Demo"]
    
    widgetAddEvents paintArea [PointerMotionMask, ButtonPressMask, ButtonReleaseMask]
    on paintArea buttonPressEvent $ do
        bttnId <- eventButton
        liftIO $ writeIORef mouseClickState True
        return False

    on paintArea buttonReleaseEvent $ do
        liftIO $ writeIORef mouseClickState False
        return False
    
    on paintArea motionNotifyEvent $ do
        localMouseClicked <- liftIO $ (readIORef mouseClickState)
        if localMouseClicked
            then do
              (x,y) <- liftIO ( widgetGetPointer paintArea)
              localDrawSpots' <- liftIO $ (readIORef drawSpots')
              if x > 0 && x < window_height && y > 0 && y < window_height 
                then do
                  liftIO $ writeIORef drawSpots' (M.setElem 255 (convert' x, convert' y) localDrawSpots')
                  liftIO (updateCanvas paintArea)
                else
                  return False
            else
              return False
        return False
    
    on paintArea draw $ do
        localDrawSpots' <- liftIO $ (readIORef drawSpots')
        drawCoordinates localDrawSpots'
        return ()
    
    
    on window configureEvent $ do
           (w, h) <- eventSize
           return False
    
    on window objectDestroy destroyEventHandler
    boxPackStart vBox displayField PackNatural 0

    boxPackStart hBox resetBtn PackGrow 0
    boxPackStart hBox predictBtn PackGrow 0

    boxPackStart vBoxBottom hBoxBottom PackNatural 1
    boxPackStart vBoxBottom entryField PackNatural 1
    boxPackStart vBoxBottom spinBtn PackNatural 1
    boxPackStart vBoxBottom saveBtn PackNatural 1

    boxPackStart vBox hBox PackNatural 1
    boxPackStart vBox paintArea PackGrow 1
    boxPackStart vBox vBoxBottom PackNatural 1


    widgetShow displayField
    widgetShow resetBtn
    widgetShow predictBtn
    widgetShow entryField
    widgetShow spinBtn
    widgetShow saveBtn
    widgetShow paintArea
    widgetShow hBox
    widgetShow vBox
    widgetShow hBoxBottom
    widgetShow vBoxBottom
    widgetShow window
    mainGUI