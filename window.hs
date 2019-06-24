import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

-- glade file already contains much logic implementation when it comes to
-- restricting user input within the user interface

main = do
    void initGUI       
    
    builder <- builderNew
    builderAddFromFile builder "fp_projekt_window.glade"

    window <- builderGetObject builder castToWindow "mainWindow"
    window `on` deleteEvent $ do -- handler to run on window destruction
        liftIO mainQuit
        return False

    trainingButton <- builderGetObject builder castToButton "trainingButton"
    -- trainingButton `on` EVENT-HERE $ do


    predictionButton <- builderGetObject builder castToButton "predictionButton"
    -- predictionButton `on` EVENT-HERE $ do

    img <- builderGetObject builder castToImage "img"

    imgFileChooserButton <- builderGetObject builder castToFileChooserButton "imgFileChooserButton"

    fileFilter <- fileFilterNew
    fileFilterAddMimeType fileFilter "image/png"
    fileChooserAddFilter imgFileChooserButton fileFilter

    imgFileChooserButton `on` fileChooserButtonFileSet $ 
        do file <- fileChooserGetPreviewFilename imgFileChooserButton
           case file of
                Nothing -> putStrLn "Keine Datei ausgewählt"
                Just fpath -> do 
                    imageSetFromFile img fpath
                    pixbufOld <- imageGetPixbuf img
                    pixbufNew <- pixbufScaleSimple pixbufOld 350 350 InterpBilinear
                    imageSetFromPixbuf img pixbufNew

    widgetShowAll window
    mainGUI