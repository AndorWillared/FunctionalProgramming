import Data.IORef
import Control.Arrow ((***))
import Control.Monad (when,void)
import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import qualified Control.Monad.Fail as Fail

type SR = IORef (Maybe Surface)


instance Fail.MonadFail SR

clearSurface :: Surface -> IO ()
clearSurface surface =
  renderWith surface $ do
    setSourceRGB 1 1 1
    paint

-- Create a new surface of the appropriate size to store our scribbles
configureEventCb :: SR -> EventM any Bool
configureEventCb surfaceRef = eventWindow >>= \window -> tryEvent $ liftIO $ do
  traverse surfaceFinish =<< readIORef surfaceRef
  surface <- renderWithDrawWindow window $
             withTargetSurface $ \s -> liftIO $ do
               w <- drawWindowGetWidth window
               h <- drawWindowGetHeight window
               createSimilarSurface s ContentColor w h
  writeIORef surfaceRef (Just surface)

  -- Initialize the surface to white
  clearSurface surface

-- Redraw the screen from the surface.  Note that the ::draw
-- signal receives a ready-to-be-used cairo_t that is already
-- clipped to only draw the exposed areas of the widget
drawCb :: SR -> Render ()
drawCb surfaceRef = do
  Just surface <- liftIO (readIORef surfaceRef)
  setSourceSurface surface 0 0
  paint

-- Draw a rectangle on the surface at the given position
drawBrush :: Surface -> DrawingArea -> Int -> Int -> IO ()
drawBrush surface widget x y = do
  -- Paint to the surface, where we store our state
  renderWith surface $ do
    rectangle (fromIntegral x-3) (fromIntegral y-3) 6 6
    fill

  -- Now invalidate the affected refion of the drawing area.
  widgetQueueDrawArea widget (x-3) (y-3) 6 6

-- Handle button press events by either drawing a rectangle
-- or clearing the surface, depending on which button was pressed.
-- The ::button-press signal handler receives a GdkEventButton
-- struct which contains this information.
buttonPressEventCb :: SR -> DrawingArea -> EventM EButton Bool
buttonPressEventCb surfaceRef widget = tryEvent $ do
  -- Paranoia check, in case we haven't gotten a configure event.
  -- In Haskell, it's automatically induced by tryEvent.
  Just surface <- liftIO (readIORef surfaceRef)
  button <- eventButton

  when (button == LeftButton) $ do
    (x,y) <- (truncate *** truncate) <$> eventCoordinates
    liftIO (drawBrush surface widget x y)
  when (button == RightButton) $ liftIO $ do
    clearSurface surface
    widgetQueueDraw widget

-- Handle motion events by continuing to draw if button 1 is
-- still held down.  The ::motion-notify signal handler receives
-- a GdkEventMotion struct which contains this information.
motionNotifyEventCb :: SR -> DrawingArea -> EventM EMotion Bool
motionNotifyEventCb surfaceRef widget = tryEvent $ do
  -- Paranoia check, in case we haven't gotten a configure event.
  -- In Haskell, it's automatically induced by tryEvent.
  Just surface <- liftIO (readIORef surfaceRef)
  modifiers <- eventModifierMouse
  when (Button1 `elem` modifiers) $ do
    (x,y) <- (round *** round) <$> eventCoordinates
    liftIO $ drawBrush surface widget x y

closeWindow :: SR -> EventM EAny ()
closeWindow surfaceRef = liftIO $
  void . traverse surfaceFinish =<< readIORef surfaceRef

main = do
  -- Surface to store current scribbles
  surfaceRef <- newIORef Nothing

  initGUI
  window <- windowNew
  set window [ windowTitle := "Drawing area" ]
  window `on` deleteEvent $ liftIO mainQuit >> return False
  window `on` destroyEvent $ closeWindow surfaceRef >> return False
  set window [ containerBorderWidth := 8 ]

  frame <- frameNew
  set frame [ frameShadowType := ShadowIn ]
  containerAdd window frame

  drawingArea <- drawingAreaNew
  -- set a minimum size
  widgetSetSizeRequest drawingArea 100 100
  containerAdd frame drawingArea

  drawingArea `on` draw           $ drawCb           surfaceRef
  drawingArea `on` configureEvent $ configureEventCb surfaceRef

  -- Event signals
  drawingArea `on` motionNotifyEvent $ motionNotifyEventCb surfaceRef drawingArea
  drawingArea `on` buttonPressEvent  $ buttonPressEventCb  surfaceRef drawingArea

  -- Ask to receive events the drawing area doesn't normally
  -- subscribe to.  In particular, we need to ask for the
  -- button press and motion notify events that we want to handle.
  widgetAddEvents drawingArea [ButtonPressMask,PointerMotionMask]

  widgetShowAll window
  mainGUI