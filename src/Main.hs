{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Declarative
import Control.Monad.IO.Class (liftIO)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Data.List (isSuffixOf, isPrefixOf)
import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Console.ANSI
import Text.Regex.Posix(AllTextMatches, getAllTextMatches, (=~))


data Export = Export {
  file :: FilePath,
  exportName :: String,
  isDefault :: Bool
} deriving(Show)

flags :: Flag "p" '["path"] "STRING" "path" (String)
      -> Arg "ARGS" [String]
      -> Cmd "Command-Line Flags" ()

flags path  args = do
    liftIO . barrel $ get path

main :: IO ()
main = run_ flags

barrel :: String -> IO()
barrel path = do
    exports <- scanDir path
    return ()

scanDir :: String -> IO [Export]
scanDir path = do
    files <- getDirectoryContents path
    exports <- mapM processEntry files
    return $ concat exports
    where
      processEntry :: FilePath -> IO [Export]
      processEntry file | isSuffixOf "tsx" file = processFile $ path </> file
      processEntry file | file =="." || file == ".." = return []
      processEntry file = do
        isDirectory <- doesDirectoryExist $ path </> file
        if isDirectory
           then scanDir $ path </> file
           else return []

processFile :: FilePath -> IO [Export]
processFile file = do
  logProcessStart
  content <- readFile file
  let matches =  content =~ pat :: [[String]]
  let exports = map matchToExport matches
  mapM_ putStrLn $ map (show . exportName) exports
  return exports
  where
    pat :: String
    pat = "export ((class |interface |type )|(default (class |interface |type )?))(\\w+)"

    matchToExport :: [String] -> Export
    matchToExport [_, exportType, _, _, _, name] = Export {exportName = name, file = file, isDefault = (isPrefixOf "default" exportType)}

    logProcessStart :: IO()
    logProcessStart = do
      putStr $ "Processing: " ++ (takeDirectory file) ++ "/"
      setSGR [ SetConsoleIntensity BoldIntensity , SetColor Foreground Dull Blue]
      putStr $ takeFileName file
      setSGR [Reset]
      putStrLn ""


