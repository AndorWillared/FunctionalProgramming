import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as C
import Data.IORef
import Control.Monad.State
import Data.Matrix as M
import Codec.Picture.Png
import Codec.Picture.Types
import Data.Text
import NeuralNetwork
import Util
import MNIST

-- constants_begin
matrix_width = 28
matrix_height = 28
tile_size = 24
tile_sizeD = 23.0
least_opacity = 255
window_width = matrix_width * tile_size 
window_height = matrix_width * tile_size + 200
-- constants_end 

-- check for valid indices in matrix
checkAdjacent :: Int -> Int -> Bool
checkAdjacent x y =
  if x >= 1 && x <= matrix_width && y >= 1 && y <= matrix_height
    then
      True
    else 
      False

-- get nearby on Y-Axis (used for pencil)
getAdjacentY :: Int -> Int -> [(Int,Int)]
getAdjacentY x y = [ (x,ys) | ys <- [(y-1),(y+1)], checkAdjacent x ys]

-- get nearby on X-Axis (used for pencil)
getAdjacentX :: Int -> Int -> [(Int,Int)]
getAdjacentX x y = [ (xs,y) | xs <- [(x-1),(x+1)], checkAdjacent xs y]

-- draw one specific spot in canvas 
drawCoordinateSpot :: Double -> Int -> Int -> C.Render ()
drawCoordinateSpot rgb j i =
  do
    C.rectangle (fromIntegral (tile_size * i + 1)) (fromIntegral(tile_size * j +1)) tile_sizeD tile_sizeD
    C.setSourceRGB rgb rgb rgb
    C.fill

-- drawMethod for each Value in Matrix
drawCoordinates :: M.Matrix Int -> C.Render ()
drawCoordinates m = do
  forM_ [0..(matrix_height-1)] $ \y -> do 
    forM_ [0..(matrix_width-1)] $ \x -> do
      drawCoordinateSpot (fromIntegral ((M.getElem (x+1) (y+1) m))/255) y x

-- EventHandler if window is quit (Gtk)      
destroyEventHandler :: IO ()
destroyEventHandler =
  do mainQuit

-- updates DrawingArea calls "on paintArea draw $ " (caller for Gtk-Event draw)   
updateCanvas :: DrawingArea -> IO Bool
updateCanvas canvas = do
  widgetQueueDraw canvas
  return False

-- modify value to map on matrix   
convert' :: Int -> Int
convert' value = 1 + (value `quot` tile_size)

-- get Pixel of Matrix
getPixel :: Int -> Int -> M.Matrix Int -> Pixel8
getPixel x y matrix = (fromIntegral(M.getElem (x+1) (y+1) matrix))

-- writes an image to path of executeable/binary
imageCreator :: String -> M.Matrix Int -> IO()
imageCreator path matrix = writePng path $ generateImage pixelRenderer matrix_width matrix_height
      where pixelRenderer x y = PixelRGBA8 (getPixel x y matrix) (getPixel x y matrix) (getPixel x y matrix) least_opacity

-- converts double value to int      
toInt :: Double -> Int
toInt = round

-- Gtk GUIBuilder
main :: IO ()
main =
  do
    -- initalize Empty Neural Network
    net <- (initNN [] 101)
    -- create IORef
    gnn <- newIORef net

    mouseClickState <- newIORef False
    drawSpots' <- newIORef (M.zero matrix_width matrix_height)
    
    -- create GTK GUI
    initGUI
    
    vBox <- vBoxNew False 1
    hBox <- hBoxNew False 2
    
    hBoxBottom <- hBoxNew False 2
    vBoxBottom <- vBoxNew False 1

    paintArea <- drawingAreaNew
    displayField <- labelNew (Just " ")

    -- spinButton for naming of traingdata
    spinBtn <- spinButtonNewWithRange 0 999999 1

    -- entry for naming of traingdata (classes)
    entryField <- entryNew
    set entryField [ entryPlaceholderText := Just("class") ]

    -- entry for path of neural network
    loadField <- entryNew
    set loadField [ entryPlaceholderText := Just("networkpath") ]

    -- Button to load Neural Network
    loadBtn <- buttonNew
    set loadBtn [ buttonLabel := "Load NN" ]
    on loadBtn buttonActivated $ do
      nnPath <- entryGetText loadField
      loadedNet <- deserialize nnPath
      liftIO $ labelSetText displayField "Status: net loaded"
      writeIORef gnn loadedNet
      return ()    
    
    -- Button to save image
    saveBtn <- buttonNew
    set saveBtn [ buttonLabel := "Save" ]
    on saveBtn buttonActivated $ do
      classVal <- entryGetText entryField
      iterationVal <- spinButtonGetValue spinBtn
      localDrawSpots' <- liftIO $ (readIORef drawSpots')
      imageCreator (classVal ++"_" ++ (show $ toInt iterationVal) ++ ".png") localDrawSpots'
      spinButtonSetValue spinBtn (iterationVal+1)
      return ()
    
    -- Button to predict matrix in view
    predictBtn <- buttonNew
    set predictBtn [ buttonLabel := "Predict" ]
    on predictBtn buttonActivated $ do
      localNN <- liftIO $ (readIORef gnn)
      matrix <- liftIO $ (readIORef drawSpots')
      let reshapedMatrix = fmap (/255.0) (fmap fromIntegral (fromList 784 1 (toList (M.transpose matrix))))
      let prediction = predict localNN reshapedMatrix -- reshapedMatrix
      let res = argmax prediction

      liftIO $ labelSetText displayField ("Wert: " ++ (show res))
      return ()

    -- Button to reset matrix in view
    resetBtn <- buttonNew
    set resetBtn [ buttonLabel := "Reset" ]
    on resetBtn buttonActivated $ do
      liftIO $ writeIORef drawSpots' (M.zero matrix_width matrix_height)              
      liftIO (updateCanvas paintArea)
      return ()

    -- Gtk window configuration
    window <- windowNew
    set window [windowDefaultWidth := window_width,
                windowDefaultHeight := window_height,
                windowWindowPosition := WinPosCenter,
                containerChild := vBox,
                windowTitle := "Neural Network Demo"]
    
    -- Add GtkEvents to EventListener
    widgetAddEvents paintArea [PointerMotionMask, ButtonPressMask, ButtonReleaseMask]
    -- handle mouseClickEvent
    on paintArea buttonPressEvent $ do
        bttnId <- eventButton
        liftIO $ writeIORef mouseClickState True
        return False
    -- handle mouseReleaseEvent
    on paintArea buttonReleaseEvent $ do
        liftIO $ writeIORef mouseClickState False
        return False
    -- handle motionNotifyEvent
    on paintArea motionNotifyEvent $ do
        localMouseClicked <- liftIO $ (readIORef mouseClickState)
        if localMouseClicked
            then do
              (x,y) <- liftIO ( widgetGetPointer paintArea)
              localDrawSpots' <- liftIO $ (readIORef drawSpots')
              if x > 0 && x < window_width && y > 0 && y < window_width 
                then do
                  liftIO $ writeIORef drawSpots' (M.setElem 255 (convert' x, convert' y) localDrawSpots')
                  -- PENCIL START
                  forM_ ((getAdjacentX (convert' x) (convert' y)) ++ (getAdjacentY (convert' x) (convert' y))) $ \adj -> do
                    localDrawSpots' <- liftIO $ (readIORef drawSpots')
                    if (M.getElem (fst adj) (snd adj) localDrawSpots' >= 255)
                      then do
                        return ()
                      else
                        liftIO $ writeIORef drawSpots' (M.setElem (255`div`2) (fst adj, snd adj) localDrawSpots')
                  -- PENCIL END
                  liftIO (updateCanvas paintArea)
                else
                  return False
            else
              return False
        return False
    -- drawCall (called by updateCanvas (internal Gtk EventHandling))
    on paintArea draw $ do
        localDrawSpots' <- liftIO $ (readIORef drawSpots')
        drawCoordinates localDrawSpots'
        return ()
  
    on window configureEvent $ do
           (w, h) <- eventSize
           return False
  
    -- add Gtk Attributes and Elements       
    on window objectDestroy destroyEventHandler
    boxPackStart vBox displayField PackNatural 0
    boxPackStart hBox loadField PackGrow 0
    boxPackStart hBox loadBtn PackGrow 0

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
    widgetShow loadBtn
    widgetShow loadField
    widgetShow paintArea
    widgetShow hBox
    widgetShow vBox
    widgetShow hBoxBottom
    widgetShow vBoxBottom
    widgetShow window
    mainGUI