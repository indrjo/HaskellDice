
import Data.List                  (intercalate)
import Data.List.Extra            (splitOn, trim)
import Data.Maybe                 (listToMaybe)
import Control.Monad              (liftM)
import Control.Conditional        (ifM)
import System.Environment         (getArgs)
import System.Environment.FindBin (getProgPath)
import System.FilePath            ((</>), takeDirectory, takeFileName)
import System.Directory           (renameFile, doesFileExist)
import System.Random              (randomRIO)
import System.Exit                (die)

main :: IO ()
main = ifM (isOpt "--reset") zeroDice (upDice =<< rollDice)

-- Check whether a string is an option passed via command-line or not.
isOpt :: String -> IO Bool
isOpt str = liftM (elem str) getArgs

{- Change the name of the file you want to read: such file with a new 
   name is the one to be actually read; then its lines are  organized
   in a list of strings.                                            -}
getElemsFrom :: FilePath -> IO [String]
getElemsFrom file = do
    renameFile file (old file)
    slurp (old file)
  where
    -- make a file old
    old :: FilePath -> FilePath
    old f = takeDirectory f </> "old-" ++ takeFileName f
    -- the actual file reader
    slurp :: FilePath -> IO [String]
    slurp = liftM h . readFile
      where
        -- lines are trimmed and blank lines discarded
        h :: String -> [String]
        h = filter (/= "") . map trim . lines

-- Write in a file the elements of a given list one per line.
writeList :: FilePath -> [String] -> IO ()
writeList file = writeFile file . intercalate "\n"

{- The file to be used as list of elements, where counters are
   stored, updated or resetted. By default a file called "dice.list"
   is looked for into the directory where the main program lies. If
   you don't want that, you may add "--use" with a value among the
   options passed via command-line
   
    $ ./dice --use /path/to/this/file                               -}
getListFile :: IO FilePath
getListFile = do
    this <- liftM (next "--use") getArgs
    case this of
      Just a -> ifM (doesFileExist a)
                    (return a)
                    (die $ "no file \"" ++ a ++ "\"!")
      _      -> die $ "no file provided!"
  where
    next :: Eq a => a -> [a] -> Maybe a
    next e = listToMaybe . drop 1 . dropWhile (/= e)

{- Use the file provided by getListFile and output an IO couple: the
   first of its components is the list of lines, and the second one is
   a randomly chosen element of that list.                          -}
rollDice :: IO ([String], String)
rollDice = do
    xs <- getElemsFrom =<< getListFile
    x  <- getRandElemFrom xs
    return (xs, x)
  where
    getRandElemFrom :: [a] -> IO a
    getRandElemFrom as = liftM (as !!) (r as)
      where
        r :: [a] -> IO Int
        r ls = randomRIO (0, length ls -1)

{- Accept the couple thrown out by rollDice, updates the selected
   line int the list of lines and write the new list in the file
   indicated by getListFile (remember such file doesn't exist anymore
   since it is renamed by rollDice).                                -}
upDice :: ([String], String) -> IO ()
upDice (xs, x) = do
    putStrLn . up $ x
    here <- getListFile
    writeList here . map (\z -> if z == x then up z else z) $ xs
  where
    up :: String -> String
    up = glue . (\(a, b) -> (a, b+1)) . chop
      where
        chop :: String -> (String, Int)
        chop str = case splitOn "#" str of
                     s:t:_ -> (trim s, read . trim $ t)
                     s:[]  -> (s, 0)
                     _     -> error "blank lines cannot be chopped"
        glue :: (String, Int) -> String
        glue (str, n) = str ++ " # " ++ show n

{- Reset all counters of the file indicated by getListFile. Actually,
   no counter is replaced by zero: only the " # <counter>" parts are
   removed form each line instead.                                  -}
zeroDice :: IO ()
zeroDice = do
    here <- getListFile
    xs   <- getElemsFrom here
    writeList here . map (trim . takeWhile (/= '#')) $ xs
    putStrLn "counters resetted!"

