{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, isSuffixOf)
import Options.Declarative
import System.Console.ANSI
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeDirectory, takeFileName)
import Text.Regex.Posix (AllTextMatches, (=~), getAllTextMatches)

data Export = Export
  { exportName :: String
  , isDefault :: Bool
  } deriving (Show)

data FileExports =
  FileExports FilePath
              [Export]
  deriving (Show)

flags :: Flag "p" '[ "path"] "STRING" "path" (String) -> Arg "ARGS" [String] -> Cmd "Command-Line Flags" ()
flags path args = do
  liftIO . barrel $ get path

main :: IO ()
main = run_ flags

barrel :: String -> IO ()
barrel path = do
  exports <- scanDir path
  writeExports path exports
  return ()

scanDir :: String -> IO [FileExports]
scanDir path = do
  files <- getDirectoryContents path
  exports <- mapM processEntry files
  return $ concat exports
  where
    processEntry :: FilePath -> IO [FileExports]
    processEntry file
      | isSuffixOf "tsx" file = do
        export <- processFile $ path </> file
        return [export]
    processEntry file
      | file == "." || file == ".." = return []
    processEntry file = do
      isDirectory <- doesDirectoryExist $ path </> file
      if isDirectory
        then scanDir $ path </> file
        else return []

processFile :: FilePath -> IO FileExports
processFile file = do
  logProcessStart
  content <- readFile file
  let matches = content =~ pat :: [[String]]
  let exports = map matchToExport matches
  return $ FileExports file exports
  where
    pat :: String
    pat = "export ((class |interface |type )|(default (class |interface |type )?))(\\w+)"
    matchToExport :: [String] -> Export
    matchToExport [_, exportType, _, _, _, name] = Export {exportName = name, isDefault = (isPrefixOf "default" exportType)}
    logProcessStart :: IO ()
    logProcessStart = do
      putStr $ "Processing: " ++ (takeDirectory file) ++ "/"
      setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Blue]
      putStr $ takeFileName file
      setSGR [Reset]
      putStrLn ""

writeExports :: FilePath -> [FileExports] -> IO ()
writeExports path fileExports = do
  putStrLn "Writing fbarrel.ts"
