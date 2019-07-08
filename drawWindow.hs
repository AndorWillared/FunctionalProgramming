import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as GE
import Graphics.Rendering.Cairo
import Control.Monad                -- for  ForM_
import Control.Monad.IO.Class
import Data.Colour.SRGB             -- for  channelRed, channelGreen, channelBlue
import Data.Colour.RGBSpace.HSV     -- for hsv
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad.State
import Data.Map
import Data.Matrix as M
import qualified Data.Vector as V 
import Codec.Picture.Png
import Codec.Picture.Types

backgroundColor :: RGB Double
backgroundColor = hsv 0.0 0.0 0.0

pencilColor :: RGB Double
pencilColor = hsv 0.0 0.0 100.0


drawColoredQuadrangle :: RGB Double -> Int -> Int -> Render ()
drawColoredQuadrangle rgb i j =
  do
    rectangle (fromIntegral (12*i + 1)) (fromIntegral(12 * j + 1)) 11.0 11.0
    setSourceRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb)
    fill

drawPencilQuadrangle :: RGB Double -> Int -> Int -> Render ()
drawPencilQuadrangle rgb i j =
  do
    rectangle (fromIntegral (12*i + 0)) (fromIntegral(12 * j + 0)) 12.0 12.0 
    setSourceRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb)
    fill
    --stroke

-- drawPencilQuadrangle' rgb i j =
--   do
--     rectangle (fromIntegral (12*i + 0)) (fromIntegral(12 * j + 0)) 12.0 12.0 
--     setSourceRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb)
--     fill
--     --stroke

drawQuadrangle :: Int -> Int -> Render ()
drawQuadrangle i j =
  drawColoredQuadrangle (backgroundColor) i j

drawPencilQuad :: (Int,Int) -> Render ()
drawPencilQuad (x,y) =
  drawPencilQuadrangle (pencilColor) x y

-- drawPencilQuad' :: (Int,Int) -> Render ()
-- drawPencilQuad' (x,y) =
--   drawPencilQuadrangle' (pencilColor) x y

drawAllPencilQuads :: [(Int,Int)] -> Render()
drawAllPencilQuads tuples =
  forM_ tuples (drawPencilQuad)

-- drawAllPencilQuads' :: M.Matrix Double -> Render()
-- drawAllPencilQuads' m =
--   forM_ m (drawPencilQuad)

drawRowOfQuadrangles :: Int -> Render ()
drawRowOfQuadrangles i =
    forM_ [0 .. 27] (drawQuadrangle i)


drawEventHandler :: G.DrawingArea -> Render()
drawEventHandler drawingArea  = do
    forM_ [0 .. 27] (drawRowOfQuadrangles)
    liftIO (putStrLn ("is called 2"))

destroyEventHandler :: IO ()
destroyEventHandler =
  do G.mainQuit

updateCanvas :: G.DrawingArea -> IO Bool
updateCanvas canvas = do
  G.widgetQueueDraw canvas
  liftIO (putStrLn ("draw"))
  return False
  
appendTuple:: [(Int,Int)] -> (Int, Int) -> [(Int,Int)]
appendTuple list value = value : list

convert :: Int -> Int
convert value = (value `quot` 12)

convert' :: Int -> Int
convert' value = 1 + (value `quot` 12)

generateMap width height = replicate height . replicate width

main' :: Bool -> IO ()
main' test =
  do
    mouseClickState <- newIORef False
    drawSpots <- newIORef ([])
    drawSpots' <- newIORef (M.zero 28 28)

    G.initGUI
    window <- G.windowNew
    vBox <- G.vBoxNew False 2
    paintArea <- G.drawingAreaNew
    displayField <- G.labelNew (Just " ")
    G.set window [G.windowDefaultWidth G.:= 337,
                G.windowDefaultHeight G.:= 357,
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
              localDrawSpots <- liftIO $ (readIORef drawSpots)
              localDrawSpots' <- liftIO $ (readIORef drawSpots')
              liftIO $ writeIORef drawSpots (appendTuple localDrawSpots (convert x, convert y))
              liftIO $ writeIORef drawSpots' (M.setElem 100.0 (convert' x, convert' y) localDrawSpots')
              liftIO $ putStrLn $ show $ ((convert' x), (convert' y))
            else
              return ()
        liftIO (updateCanvas paintArea)
    G.on paintArea G.draw $ do
        paintArea <- forM_ [0 .. 27] (drawRowOfQuadrangles)
        localDrawSpots <- liftIO $ (readIORef drawSpots)
        drawAllPencilQuads localDrawSpots
        localDrawSpots' <- liftIO $ (readIORef drawSpots')
        -- sdrawAllPencilQuads' localDrawSpots'
        -- liftIO $ putStrLn $ show $ localDrawSpots'
        return ()
    G.on window G.configureEvent $ do
           (w, h) <- G.eventSize
           liftIO (putStrLn (show w ++ "x" ++ show h))
           return False
    G.on window G.objectDestroy destroyEventHandler
    G.boxPackStart vBox displayField G.PackNatural 0
    G.boxPackStart vBox paintArea G.PackGrow 0
    G.widgetShow displayField
    G.widgetShow paintArea
    G.widgetShow vBox
    G.widgetShow window
    G.mainGUI


getPixel :: Int -> Int -> M.Matrix Int -> Pixel8
getPixel x y matrix = (fromIntegral(M.getElem x y matrix))

imageCreator :: String -> M.Matrix Int -> IO()
imageCreator path matrix = writePng path $ generateImage pixelRenderer 28 28
      where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y)  (getPixel x y matrix)

-- vectorToPng :: V.Vector (PixelBaseComponent PixelRGBA8) -> IO ()
vectorToPng pixels = do
  writePng "./wow.png" pixels


main :: IO ()
main = do
    main' False
