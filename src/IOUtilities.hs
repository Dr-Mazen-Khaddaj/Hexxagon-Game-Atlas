module IOUtilities  ( chooseIndex
                    , fetchFilesWithExtension
                    , askYesNo
                    , getLine'
                    , printTxID
                    , ToColor(..)
                    ) where

import System.Directory (listDirectory)
import System.FilePath (takeExtension)
import System.IO (hFlush, stdout)
import Data.Char (toLower)
import Text.Read (readMaybe)
import GeniusYield.Types (GYTxId, GYLogNamespace)
import GeniusYield.Imports (fromString, IsString)

chooseIndex :: Show a => String -> [a] -> IO Int
chooseIndex typeName list = do
    shortLine ; printList list ; shortLine
    putStr $ "Select " <> typeName <> " : "
    ($ 1) . (-) <$> getInt [1..(length list)]

fetchFilesWithExtension :: FilePath -> String -> IO [FilePath]
fetchFilesWithExtension dir ext = ((dir <>) <$>) . filter ((== ext) . takeExtension) <$> listDirectory dir

printList :: Show a => [a] -> IO ()
printList = mapM_ (\(n,a)-> putStrLn $ show @Int n <> " - " <> show a) . zip [1..]

getInt :: [Int] -> IO Int
getInt range = do
    input <- getLine'
    case readMaybe @Int input of
        Just n -> if n `elem` range then return n else putStrLn errorMsg2 >> getInt range
        Nothing -> putStrLn errorMsg1 >> getInt range
    where
        errorMsg1 = "Please enter a number!"
        errorMsg2 = "Number out of range!"

askYesNo :: IO Bool
askYesNo = getLine' >>= (\case  "yes" -> pure True
                                "no"  -> pure False
                                _     -> putStrLn "Invalid Response!" >> askYesNo
                        ) . map toLower

getLine' :: IO String
getLine' = hFlush stdout >> getLine
-- 
printTxID :: GYTxId -> IO ()
printTxID txID = do
    putStrLn "-----------------------------------"
    putStrLn $ "Transaction ID : " <> show txID
    putStrLn $ "Cardanoscan    : https://preview.cardanoscan.io/transaction/" <> show txID
    putStrLn "============================================================================================================================="

---------------------
-- Aesthetic Tools --

emptyLine :: IO ()
emptyLine = putStrLn ""
-- 
dashedLine :: IO ()
dashedLine = putStrLn "--------------------------------------------------------------------------------"
-- 
whiteLine :: IO ()
whiteLine = putStrLn "============================================================================================================"
-- 
shortLine :: IO ()
shortLine = putStrLn "---------------"
-- 
class ToColor a where
    toBlack, toRed, toGreen, toYellow, toBlue, toMagenta, toCyan, toWhite, toIBlack, toIRed, toIGreen, toIYellow, toIBlue, toIMagenta, toICyan, toIWhite
        :: IsString a => String -> a
    toBlack     str = fromString $ "\ESC[30m" <> str <> "\ESC[0m"
    toRed       str = fromString $ "\ESC[31m" <> str <> "\ESC[0m"
    toGreen     str = fromString $ "\ESC[32m" <> str <> "\ESC[0m"
    toYellow    str = fromString $ "\ESC[33m" <> str <> "\ESC[0m"
    toBlue      str = fromString $ "\ESC[34m" <> str <> "\ESC[0m"
    toMagenta   str = fromString $ "\ESC[35m" <> str <> "\ESC[0m"
    toCyan      str = fromString $ "\ESC[36m" <> str <> "\ESC[0m"
    toWhite     str = fromString $ "\ESC[37m" <> str <> "\ESC[0m"
    toIBlack    str = fromString $ "\ESC[90m" <> str <> "\ESC[0m"
    toIRed      str = fromString $ "\ESC[91m" <> str <> "\ESC[0m"
    toIGreen    str = fromString $ "\ESC[92m" <> str <> "\ESC[0m"
    toIYellow   str = fromString $ "\ESC[93m" <> str <> "\ESC[0m"
    toIBlue     str = fromString $ "\ESC[94m" <> str <> "\ESC[0m"
    toIMagenta  str = fromString $ "\ESC[95m" <> str <> "\ESC[0m"
    toICyan     str = fromString $ "\ESC[96m" <> str <> "\ESC[0m"
    toIWhite    str = fromString $ "\ESC[97m" <> str <> "\ESC[0m"
instance ToColor GYLogNamespace
instance ToColor String

--------------------------------------------------------------------------------------------------------------------------- |