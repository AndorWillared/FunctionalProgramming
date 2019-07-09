import Data.IDX
import System.IO
import Data.Maybe
import Debug.Trace

main =  do
        fp <- getLine
        labels <- decodeIDXLabelsFile fp
        fp2 <- getLine
        imgs <- decodeIDXFile fp2
        let res = fromJust $ labeledDoubleData (fromJust labels) (fromJust imgs)
        putStrLn $ show $ length res