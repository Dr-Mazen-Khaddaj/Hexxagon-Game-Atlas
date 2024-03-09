module IOUtilities  ( chooseIndex
                    , fetchFilesWithExtension
                    , askYesNo
                    , getLine'
                    ) where

import System.Directory (listDirectory)
import System.FilePath (takeExtension)
import System.IO (hFlush, stdout)
import Data.Char (toLower)
import Text.Read (readMaybe)

chooseIndex :: Show a => String -> [a] -> IO Int
chooseIndex typeName list = do
    printShortLine ; printList list ; printShortLine
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

printShortLine :: IO ()
printShortLine = putStrLn "---------------"

--------------------------------------------------------------------------------------------------------------------------- |