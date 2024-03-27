module IOUtilities  ( chooseIndex
                    , fetchFilesWithExtension
                    , askYesNo
                    , getLine'
                    , printTxID
                    , printMsg
                    , ToColor(..)
                    ) where

import System.IO            ( hFlush, stdout )
import System.FilePath      ( takeExtension  )
import System.Directory     ( listDirectory  )
import Text.Read            ( readMaybe      )
import Data.Char            ( toLower        )
import GeniusYield.Types    ( GYTxId, GYLogNamespace )
import GeniusYield.Imports  ( fromString, IsString   )

chooseIndex :: Show a => String -> [a] -> IO Int
chooseIndex typeName list = do
    printMsg (width + 16) listStr
    putStr $ "Select " <> typeName <> " : "
    ($ 1) . (-) <$> getInt [1..(length list)]
    where
        width = maximum $ length . show <$> list
        listStr = [ show i <> " - " <> e <> replicate (width - length e) ' ' | (i,show -> e) <- zip @Int [1..] list]

fetchFilesWithExtension :: FilePath -> String -> IO [FilePath]
fetchFilesWithExtension dir ext = ((dir <>) <$>) . filter ((== ext) . takeExtension) <$> listDirectory dir

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

--------------------------------------------------- | Aesthetic Tools | ---------------------------------------------------

printTxID :: GYTxId -> IO ()
printTxID txID = do
    putStrLn $ toIGreen "Confirmed"
    putStrLn "-----------------------------------"
    putStrLn $ "Transaction ID : " <> toIYellow (show txID)
    putStrLn $ "Cardanoscan    : " <> toICyan ("https://preview.cardanoscan.io/transaction/" <> show txID)
    putStrLn "============================================================================================================================="
-- 
printMsg :: Int -> [String] -> IO ()
printMsg n msgs = do
    let line = toMagenta $ replicate n '='
        gap = replicate 5 ' '
    putStrLn line
    mapM_ (\ msg -> putStrLn $ toMagenta "=" <> gap <> msg <> gap <> toMagenta "=") msgs
    putStrLn line
-- 
-- emptyLine :: IO ()
-- emptyLine = putStrLn ""
-- -- 
-- dashedLine :: IO ()
-- dashedLine = putStrLn "--------------------------------------------------------------------------------"
-- -- 
-- whiteLine :: IO ()
-- whiteLine = putStrLn "============================================================================================================"
-- -- 
-- shortLine :: IO ()
-- shortLine = putStrLn "---------------"
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

---------------------------------------------------------------------------------------------------------------------------