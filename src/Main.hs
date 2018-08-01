{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Declarative
import Control.Monad.IO.Class (liftIO)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Data.List (isSuffixOf)
import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Console.ANSI
import Text.Regex.Posix(AllTextMatches, getAllTextMatches, (=~))


flags :: Flag "p" '["path"] "STRING" "path" (String)
      -> Arg "ARGS" [String]
      -> Cmd "Command-Line Flags" ()

flags path  args = do
    liftIO . barrel $ get path

main :: IO ()
main = run_ flags

barrel :: String -> IO()
barrel = scanDir

scanDir :: String -> IO()
scanDir path = do
    files <- getDirectoryContents path
    mapM_ processEntry files
    where
      processEntry :: FilePath -> IO()
      processEntry file | isSuffixOf "tsx" file = processFile $ path </> file
      processEntry file | file =="." || file == ".." = return ()
      processEntry file = do
        isDirectory <- doesDirectoryExist $ path </> file
        if isDirectory
           then scanDir $ path </> file
           else return ()

processFile :: FilePath -> IO()
processFile file = do
  logProcessStart
  content <- readFile file
  let matches =  content =~ pat :: [[String]]
  putStrLn $ show matches
  return ()
  where
    pat :: String
    pat = "export default (class|interface|type )?(\\w+)"
    logProcessStart :: IO()
    logProcessStart = do
      putStr $ "Processing: " ++ (takeDirectory file) ++ "/"
      setSGR [ SetConsoleIntensity BoldIntensity , SetColor Foreground Dull Blue]
      putStr $ takeFileName file
      setSGR [Reset]
      putStrLn ""


