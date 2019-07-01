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
    -- trainingButton `on` buttonActivated $ do


    predictionButton <- builderGetObject builder castToButton "predictionButton"
    -- predictionButton `on` buttonActivated $ do

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

    settingsModal <- builderGetObject builder castToWindow "settingsModal"
    windowSetPosition settingsModal WinPosCenterOnParent
    windowSetKeepAbove settingsModal True
    settingsModal `on` deleteEvent $ do -- emmited on delete event
        return $ widgetHide settingsModal
        return False
        
    modalSaveButton <- builderGetObject builder castToButton "modalSaveButton"
    modalSaveButton `on` buttonActivated $ do
        -- initialize network here --
        widgetHide settingsModal

    modalCancelButton <- builderGetObject builder castToButton "modalCancelButton"
    modalCancelButton `on` buttonActivated $ do
        widgetHide settingsModal

    networkSettingsButton <- builderGetObject builder castToButton "networkSettingsButton"
    networkSettingsButton `on` buttonActivated $ do
        widgetShow modalCancelButton
        widgetShow settingsModal

    hiddenNodeDetailBox <- builderGetObject builder castToBox "hiddenNodeDetailBox"

    hiddenNodeCountSelector <- builderGetObject builder castToSpinButton "hiddenNodeCountSelector"
    hiddenNodeCountSelector `afterValueSpinned` (do
            spinButtonValue <- spinButtonGetValue hiddenNodeCountSelector
            -- delete old buttons
            oldButtons <- containerGetChildren hiddenNodeDetailBox
            -- remove old buttons
            -- return $ fmap (\button -> containerRemove hiddenNodeDetailBox button) oldButtons 
            putStrLn $ show $ spinButtonValue
            -- putStrLn $ show $ length oldButtons
            -- widgetDestroy hiddenNodeDetailBox
            -- hiddenNodeDetailBox <- vBoxNew
            -- testText <- entryNew
            -- entrySetText testText "TestText"
            -- boxPackStart hiddenNodeDetailBox testText PackNatural 0
            sequence $ fmap (\button -> widgetDestroy button) oldButtons -- LAZY EVALUATION, WIRD NICHT AUSGEFÜHRT
            -- sequence makes spinButtonList from type "[IO SpinButton]" to type "IO [SpinButton]"
            -- spinButtonList is of type "[SpinButton]"
            spinButtonList <- sequence [ spinButtonNewWithRange 1 1000 1 | x <- [1..spinButtonValue] ]
            -- for each spin button, add it to the hiddenNodeDetailBox
            forM_ spinButtonList (\button -> boxPackStart hiddenNodeDetailBox button PackNatural 0) -- LAZY EVALUATION, WIRD NICHT AUSGEFÜHRT
            widgetShowAll hiddenNodeDetailBox 
            return ()
        )

    widgetShowAll window
    widgetShow settingsModal
    widgetHide modalCancelButton
    mainGUI