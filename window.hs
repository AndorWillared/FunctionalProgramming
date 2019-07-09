import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import NeuralNetwork
import MNIST

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

    alertWindow <- builderGetObject builder castToWindow "alertWindow"
    alertLabel <- builderGetObject builder castToLabel "alertLabel"
    alertButton <- builderGetObject builder castToButton "alertButton"
    alertButton `on` buttonActivated $ do
        labelSetLabel alertLabel ""
        widgetHide alertWindow

    gnn <- newIORef (createNeuralNetwork [])
    netConfigured <- newIORef False
    imagePath <- newIORef ""
    imageSet <- newIORef False

    statusLabel <- builderGetObject builder castToLabel "statusLabel"
    trainFactorSelector <- builderGetObject builder castToSpinButton "trainFactorSelector"

    predictionLabel <- builderGetObject builder castToLabel "predictionLabel"

    predictionButton <- builderGetObject builder castToButton "predictionButton"
    predictionButton `on` buttonActivated $ do
        localNN <- liftIO $ (readIORef gnn)
        localImgS <- liftIO $ (readIORef imageSet)
        temp <- readIORef imagePath
        if localImgS 
            then do 
                imageMatrix <- pngToVector temp
                let prediction = predict localNN imageMatrix
                putStrLn $ show prediction
                let res = argmax prediction
                labelSetLabel predictionLabel ("Wert: " ++ (show res))
            else do 
                labelSetLabel alertLabel "Bitte ein 28x28 Pixel Bild auswählen"
                widgetShow alertWindow

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
                    pixbuf <- imageGetPixbuf img
                    width <- pixbufGetWidth pixbuf
                    height <- pixbufGetHeight pixbuf
                    if width == 28 || height == 28
                        then do
                            pixbufNew <- pixbufScaleSimple pixbuf 400 400 InterpBilinear
                            imageSetFromPixbuf img pixbufNew
                            widgetShow img
                            liftIO $ writeIORef imagePath fpath
                            liftIO $ writeIORef imageSet True
                        else do
                            imageClear img
                            widgetHide img
                            liftIO $ writeIORef imageSet False
                            labelSetLabel alertLabel "Bild hat keine Größe von 28x28 Pixel!"
                            widgetShow alertWindow

    hiddenNodeLayout <- builderGetObject builder castToScrolledWindow "hiddenNodeLayout"
    hiddenNodeDetailBox <- vBoxNew False 5
    scrolledWindowAddWithViewport hiddenNodeLayout hiddenNodeDetailBox

    hiddenNodeCountSelector <- builderGetObject builder castToSpinButton "hiddenNodeCountSelector"
    hiddenNodeCountSelector `afterValueSpinned` (do
            -- get spinButton value
            spinButtonValue <- spinButtonGetValueAsInt hiddenNodeCountSelector
            -- get all widgets (all old buttons)
            oldButtons <- containerGetChildren hiddenNodeDetailBox
            -- remove old buttons
            sequence $ fmap (\button -> widgetDestroy button) oldButtons -- LAZY EVALUATION, WIRD NICHT AUSGEFÜHRT
            -- generate new buttons
            spinButtonList <- sequence [ spinButtonNewWithRange 1 1000 1 | x <- [1..spinButtonValue] ]
            -- for each spin button, add it to the hiddenNodeLayout
            forM_ spinButtonList (\button -> boxPackStart hiddenNodeDetailBox button PackNatural 0) -- LAZY EVALUATION, WIRD NICHT AUSGEFÜHRT
            -- show all widgets
            widgetShowAll hiddenNodeDetailBox
        )

    -- initialize some input fields for the hiddenNodeDetailBox
    do 
        spinButtonValue <- spinButtonGetValueAsInt hiddenNodeCountSelector
        spinButtonList <- sequence [ spinButtonNewWithRange 1 1000 1 | x <- [1..spinButtonValue] ]
        forM_ spinButtonList (\button -> boxPackStart hiddenNodeDetailBox button PackNatural 0)
        widgetShowAll hiddenNodeDetailBox

    settingsModal <- builderGetObject builder castToWindow "settingsModal"
    windowSetPosition settingsModal WinPosCenterOnParent
    windowSetKeepAbove settingsModal True
    settingsModal `on` deleteEvent $ do -- emmited on delete event
        return $ widgetHide settingsModal
        return False
        
    modalTrainInitializeButton <- builderGetObject builder castToButton "modalTrainInitializeButton"
    modalTrainInitializeButton `on` buttonActivated $ do
        widgetHide settingsModal
        labelSetLabel statusLabel "Status: Training läuft..."
        putStrLn "DEBUG1"
        putStrLn "DEBUG2"
        labelSetLabel statusLabel "Status: Training beendet."
        
        spinButtonsOfHiddenLayer <- containerGetChildren hiddenNodeDetailBox
        hiddenLayerNodes <- sequence [ spinButtonGetValueAsInt (castToSpinButton (spinButtonsOfHiddenLayer!!(x-1))) | x <- [1..(length spinButtonsOfHiddenLayer)] ]
        let net = createNeuralNetwork $ [784]++ hiddenLayerNodes ++[10]
        learningRate <- spinButtonGetValue trainFactorSelector
        trainingSamples <- getTrainingSamples
        trainedNet <- train net trainingSamples (realToFrac learningRate)
        writeIORef gnn trainedNet
        return ()

    modalTrainLoadButton <- builderGetObject builder castToButton "modalTrainLoadButton"
    modalTrainLoadButton `on` buttonActivated $ do
        widgetHide settingsModal
        -- TODO: implement logic

    modalExitButton1 <- builderGetObject builder castToButton "modalExitButton1"
    modalExitButton1 `on` buttonActivated $ do
        widgetHide settingsModal
        widgetHide window
        liftIO mainQuit 
    modalExitButton2 <- builderGetObject builder castToButton "modalExitButton2"
    modalExitButton2 `on` buttonActivated $ do
        widgetHide settingsModal
        widgetHide window
        liftIO mainQuit
        
    networkInitializeBox <- builderGetObject builder castToBox "networkInitializeBox"
    networkLoadBox <- builderGetObject builder castToBox "networkLoadBox"
    networkDisplayBox <- builderGetObject builder castToBox "networkDisplayBox"

    initBool <- newIORef (True)
    loadBool <- newIORef (False)

    networkInitializeButton <- builderGetObject builder castToButton "networkInitializeButton"
    networkInitializeButton `on` buttonActivated $ do
        localInitBool <- readIORef loadBool
        if localInitBool
            then do
                widgetShow hiddenNodeLayout
                containerRemove networkDisplayBox networkLoadBox
                containerAdd networkDisplayBox networkInitializeBox
                writeIORef initBool True
                writeIORef loadBool False
            else do
                return ()
    networkLoadButton <- builderGetObject builder castToButton "networkLoadButton"
    networkLoadButton `on` buttonActivated $ do
        localInitBool <- readIORef initBool
        if localInitBool
            then do
                widgetHide hiddenNodeLayout
                containerRemove networkDisplayBox networkInitializeBox
                boxPackEnd networkDisplayBox networkLoadBox PackNatural 0
                writeIORef initBool False
                writeIORef loadBool True
            else do
                return ()

   
    containerAdd networkDisplayBox networkInitializeBox

    widgetShowAll window
    widgetHide img
    widgetShow settingsModal

    mainGUI
    