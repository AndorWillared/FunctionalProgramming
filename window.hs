import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import NN
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

    gnn <- newIORef (createNeuralNetwork [])
    netConfigured <- newIORef False
    imagePath <- newIORef ""
    imageSet <- newIORef False

    statusLabel <- builderGetObject builder castToLabel "statusLabel"
    trainFactorSelector <- builderGetObject builder castToSpinButton "trainFactorSelector"

    trainingButton <- builderGetObject builder castToButton "trainingButton"
    trainingButton `on` buttonActivated $ do
        localNN <- liftIO $ (readIORef gnn)
        learningRate <- spinButtonGetValue trainFactorSelector
        trainingSamples <- getTrainingSamples
        trainedNN <- trainVerbose localNN trainingSamples (realToFrac learningRate)
        liftIO $ writeIORef gnn trainedNN
        liftIO $ labelSetLabel statusLabel "Status: trainiert"
        return ()

    predictionLabel <- builderGetObject builder castToLabel "predictionLabel"

    predictionButton <- builderGetObject builder castToButton "predictionButton"
    predictionButton `on` buttonActivated $ do
        localNN <- liftIO $ (readIORef gnn)
        localImgS <- liftIO $ (readIORef imageSet)
        temp <- readIORef imagePath
        if localImgS then do imageMatrix <- pngToVector temp
                             let prediction = predict localNN imageMatrix
                             let res = mapToResult prediction
                             liftIO $ labelSetLabel predictionLabel ("Sicherheit: " ++ show (fst res) ++ " Wert: " ++ show (snd res))
                     else putStrLn  "Bitte Bild wählen !"  --TODO: POPUP einfügen

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
                    -- TODO: check if image has correct size
                    liftIO $ writeIORef imagePath fpath    
                    liftIO $ writeIORef imageSet True 

    hiddenNodeLayout <- builderGetObject builder castToScrolledWindow "hiddenNodeLayout"
    hiddenNodeDetailBox <- vBoxNew False 5
    scrolledWindowAddWithViewport hiddenNodeLayout hiddenNodeDetailBox

    inputNodeCountSelector <- builderGetObject builder castToSpinButton "inputNodeCountSelector"
    hiddenNodeCountSelector <- builderGetObject builder castToSpinButton "hiddenNodeCountSelector"
    outputNodeCountSelector <- builderGetObject builder castToSpinButton "outputNodeCountSelector"
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
        
    modalSaveButton <- builderGetObject builder castToButton "modalSaveButton"
    modalSaveButton `on` buttonActivated $ do
        -- initialize network here --
        -- retrieve values from all spinButtons --
        spinButtonsOfHiddenLayer <- containerGetChildren hiddenNodeDetailBox
        inputNodes <- spinButtonGetValueAsInt inputNodeCountSelector
        hiddenLayerNodes <- sequence [ spinButtonGetValueAsInt (castToSpinButton (spinButtonsOfHiddenLayer!!(x-1))) | x <- [1..(length spinButtonsOfHiddenLayer)] ]
        outputNodes <- spinButtonGetValueAsInt outputNodeCountSelector
        let networkInitializationList = [inputNodes] ++ hiddenLayerNodes ++ [outputNodes]
        --putStrLn $ "networkInitializationList: " ++ (show networkInitializationList)
        liftIO $ writeIORef gnn (createNeuralNetwork networkInitializationList)
        liftIO $ writeIORef netConfigured True
        liftIO $ labelSetLabel statusLabel "Status: untrainiert"
        -- INITIALIZE NETWORK WITH networkInitializationList -- 
        widgetHide settingsModal

    modalCancelButton <- builderGetObject builder castToButton "modalCancelButton"
    modalCancelButton `on` buttonActivated $ do
        widgetHide settingsModal

    networkSettingsButton <- builderGetObject builder castToButton "networkSettingsButton"
    networkSettingsButton `on` buttonActivated $ do
        widgetShow modalCancelButton
        widgetShow settingsModal

    paintWindowButton <- builderGetObject builder castToButton "paintWindowButton"
    paintWindowButton `on` buttonActivated $ do
        -- OPEN PAINT WINDOW HERE --
        -- widgetShow paintWindow
        return () -- this line can be deleted later

    widgetShowAll window
    widgetShow settingsModal
    widgetHide modalCancelButton
    mainGUI