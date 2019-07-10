import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import NeuralNetwork
import MNIST
import Util

-- glade file already contains much logic implementation when it comes to
-- restricting user input within the user interface

main = do
    -- init gui thread
    void initGUI
    -- init builder
    builder <- builderNew
    -- supply builder with .glade file
    builderAddFromFile builder "fp_projekt_window.glade"

    -- VARIABLES --
    net <- (initNN [] 101)
    gnn <- newIORef net
    netConfigured <- newIORef False
    imagePath <- newIORef ""
    imageSet <- newIORef False
    initBool <- newIORef True
    loadBool <- newIORef False

    -- UI ELEMENTS --
    alertButton <- builderGetObject builder castToButton "alertButton"
    alertLabel <- builderGetObject builder castToLabel "alertLabel"
    alertWindow <- builderGetObject builder castToWindow "alertWindow"
    hiddenNodeCountSelector <- builderGetObject builder castToSpinButton "hiddenNodeCountSelector"
    hiddenNodeLayout <- builderGetObject builder castToScrolledWindow "hiddenNodeLayout"
    img <- builderGetObject builder castToImage "img"
    imgFileChooserButton <- builderGetObject builder castToFileChooserButton "imgFileChooserButton"
    modalExitButton1 <- builderGetObject builder castToButton "modalExitButton1"
    modalExitButton2 <- builderGetObject builder castToButton "modalExitButton2"
    modalTrainInitializeButton <- builderGetObject builder castToButton "modalTrainInitializeButton"
    modalTrainLoadButton <- builderGetObject builder castToButton "modalTrainLoadButton"
    networkDisplayBox <- builderGetObject builder castToBox "networkDisplayBox"
    networkFileChooserButton <- builderGetObject builder castToFileChooserButton "networkFileChooserButton"
    networkInitializeBox <- builderGetObject builder castToBox "networkInitializeBox"
    networkInitializeButton <- builderGetObject builder castToButton "networkInitializeButton"
    networkLoadBox <- builderGetObject builder castToBox "networkLoadBox"
    networkLoadButton <- builderGetObject builder castToButton "networkLoadButton"
    networkNameField <- builderGetObject builder castToEntry "networkNameField"
    networkSaveButton <- builderGetObject builder castToButton "networkSaveButton"
    predictionButton <- builderGetObject builder castToButton "predictionButton"
    predictionLabel <- builderGetObject builder castToLabel "predictionLabel"
    settingsModal <- builderGetObject builder castToWindow "settingsModal"
    statusLabel <- builderGetObject builder castToLabel "statusLabel"
    trainFactorSelector <- builderGetObject builder castToSpinButton "trainFactorSelector"
    window <- builderGetObject builder castToWindow "mainWindow"

    -- used for displaying the input fields for configuring the node count of each hidden layer
    hiddenNodeDetailBox <- vBoxNew False 5
    scrolledWindowAddWithViewport hiddenNodeLayout hiddenNodeDetailBox
    
    -- EVENT HANDLERS --
    -- closes the application with the (X) button of the main window
    window `on` deleteEvent $ do -- handler to run on window destruction
        liftIO mainQuit
        return False

    -- predicts a 28x28 picture with a trained neural net
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

    -- saves a trained neural net in the folder "trained_nets" with previously given name
    networkSaveButton `on` buttonActivated $ do
        localNN <- readIORef gnn
        networkName <- entryGetText networkNameField
        if (length networkName /= 0) 
            then do
                serialize localNN ("trained_nets/" ++ networkName)
            else do
                serialize localNN ("trained_nets/" ++ "defaultNetName")
        return ()

    -- renders a selected 28x28 pixel .png file, opens alert if invalid file is given
    imgFileChooserButton `on` fileChooserButtonFileSet $ 
        do file <- fileChooserGetPreviewFilename imgFileChooserButton
           case file of
                Nothing -> do
                    labelSetLabel alertLabel "Bitte ein 28x28 Pixel Bild auswählen"
                    widgetShow alertWindow
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

    -- dynamically creates necessary input fields for node counts of hidden layers
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
    
    -- hides the initial neural net settings window
    settingsModal `on` deleteEvent $ do -- emmited on delete event
        return $ widgetHide settingsModal
        return False
        
    -- initializes and trains a neural net with training data
    modalTrainInitializeButton `on` buttonActivated $ do
        widgetHide settingsModal
        labelSetLabel statusLabel "Status: Training läuft..."
        labelSetLabel statusLabel "Status: Training beendet."
        
        spinButtonsOfHiddenLayer <- containerGetChildren hiddenNodeDetailBox
        hiddenLayerNodes <- sequence [ spinButtonGetValueAsInt (castToSpinButton (spinButtonsOfHiddenLayer!!(x-1))) | x <- [1..(length spinButtonsOfHiddenLayer)] ]
        net <- initNN ([784]++ hiddenLayerNodes ++[10]) 42
        learningRate <- spinButtonGetValue trainFactorSelector
        trainingSamples <- getTrainingSamples "data/train-images" "data/train-labels"
        trainedNet <- train net trainingSamples (realToFrac learningRate)
        writeIORef gnn trainedNet
        return ()

    -- loads a pretrained neural net
    modalTrainLoadButton `on` buttonActivated $ do
        file <- fileChooserGetPreviewFilename networkFileChooserButton
        case file of
            Nothing -> do
                labelSetLabel alertLabel "Keine Datei ausgewählt"
                widgetShow alertWindow
            Just file -> do
                loadedNet <- deserialize file
                labelSetLabel statusLabel "Status: Netz geladen."
                writeIORef gnn loadedNet
                widgetHide settingsModal
        return ()

    -- exits the application
    modalExitButton1 `on` buttonActivated $ do
        widgetHide settingsModal
        widgetHide window
        liftIO mainQuit 
    
    -- exits the application
    modalExitButton2 `on` buttonActivated $ do
        widgetHide settingsModal
        widgetHide window
        liftIO mainQuit

    -- displays all settings needed for initializing and training a new neural net
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
    
    -- displays all settings needed for loading a pretrained neural net
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

    -- FINAL TASKS --
    -- initializes input fields for the hiddenNodeDetailBox
    do 
        spinButtonValue <- spinButtonGetValueAsInt hiddenNodeCountSelector
        spinButtonList <- sequence [ spinButtonNewWithRange 1 1000 1 | x <- [1..spinButtonValue] ]
        forM_ spinButtonList (\button -> boxPackStart hiddenNodeDetailBox button PackNatural 0)
        widgetShowAll hiddenNodeDetailBox

    -- only allows .png files to be loaded for prediction
    imgFileFilter <- fileFilterNew
    fileFilterAddMimeType imgFileFilter "image/png"
    fileChooserAddFilter imgFileChooserButton imgFileFilter

    -- initially displays the initialization settings in settings window
    containerAdd networkDisplayBox networkInitializeBox

    -- initially shows all widgets of main window
    widgetShowAll window
    -- hides img display of main window due to no image being initially selected
    widgetHide img
    -- initially open neural net settings window
    widgetShow settingsModal

    -- render the GUI
    mainGUI
    
