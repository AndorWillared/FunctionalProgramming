import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

main = do
    void initGUI       
    window <- windowNew
    set window [ windowTitle        := "Neuronales Netz"
             , windowResizable      := False
             , windowDefaultWidth   := 800
             , windowDefaultHeight  := 600 ]
    window `on` deleteEvent $ do -- handler to run on window destruction
        liftIO mainQuit
        return False

    layout <- layoutNew Nothing Nothing
    vbox <- vBoxNew False 0

    welcomeText <- entryNew
    set welcomeText [ entryEditable := True
                    , entryXalign   := 0.5 -- makes contents center-aligned
                    , entryText     := "Wilkommen zu unserem FP-Projekt!" ]
    numberText <- mkEntry "3.141592"
    set numberText  [ entryEditable := False
                    , entryXalign   := 0.5 -- makes contents center-aligned
                    , entryText     := "3.141592" ]

    containerAdd vbox welcomeText >> containerAdd vbox numberText
    containerAdd layout vbox
    containerAdd window layout

    widgetShowAll window
    mainGUI


mkEntry :: String -> IO Entry
mkEntry text = do
    entry <- entryNew
    set entry   [ entryEditable := False
                , entryXalign   := 0.5 -- makes contents center-aligned
                , entryText     := text ]
    entry `on` entryCopyClipboard $
        set entry [ entryText := "pressed" ]
    return entry