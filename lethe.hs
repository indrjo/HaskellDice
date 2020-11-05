
import Data.List           (isPrefixOf)
import Data.List.Extra     (trimEnd)
import Data.Char           (toUpper)
import Control.Monad       (liftM)
import Control.Conditional (ifM)
import System.Environment  (getArgs)
import System.Random       (randomRIO)
import System.Exit         (die)
import System.Directory    (doesFileExist)

-- the laconic main...
main :: IO ()
main = putStrLn =<< rollDice

-- The "dice": given a file meant to be a list of elements, extract
-- one among them at random. The file to be read by this program is
-- the one outputted by the getFileToUse function (see details below).
rollDice :: IO String
rollDice = liftM format (getRandItemFrom =<< getFileToUse)
  where
    -- Remove initial spaces or '*' and final spaces from a string.
    format :: String -> String
    format = (" >>> " ++) . map toUpper . purify
      where
        purify :: String -> String
        purify = trimEnd . dropWhile (`elem` [' ', '*'])
    -- The actual random extractor.
    getRandItemFrom :: FilePath -> IO String
    getRandItemFrom file = do
        xs <- getItemsFrom file
        liftM (xs !!) (getRandInt xs)
      where
        getItemsFrom :: FilePath -> IO [String]
        getItemsFrom = liftM (filter isItem . lines) . readFile
          where
            isItem :: String -> Bool
            isItem [] = False
            isItem str@(x:xs)
                | x == ' '  = isItem xs
                | otherwise = isPrefixOf "*" str
        getRandInt :: [a] -> IO Int
        getRandInt xs = randomRIO (0, length xs -1)
    -- The unique manner to tell the program what file to read is
    -- issuing the following instruction via commandline: 
    --  $ ./this-program --use FILE_TO_USE
    getFileToUse :: IO FilePath
    getFileToUse = do
        args <- getArgs
        case dropWhile (/="--use") args of
          (_:v:_) -> ifM (doesFileExist v) (return v)
                       (fatal $ v ++ " does not exist!")        
          _       -> fatal "provide a file to use!"
    -- XXX | Actually, this is the sole manoeuvre expected from the
    -- user: if no file is indicated, a fatal error is thrown out. 
    -- OK,that feature may be a real nuisance, but that's it!

-- Fatal messages: tell something and immediately die.
fatal :: String -> IO a
fatal = die . (" >>> FATAL: " ++)

