{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Declarative
import Control.Monad.IO.Class (liftIO)
import System.Directory (getDirectoryContents)
import Data.List (isSuffixOf)
import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Console.ANSI


flags :: Flag "p" '["path"] "STRING" "path" (String)
      -> Arg "ARGS" [String]
      -> Cmd "Command-Line Flags" ()

flags path  args = do
    liftIO . barrel $ get path

main :: IO ()
main = run_ flags

barrel :: String -> IO()
barrel path = do
    putStrLn path
    all <- getDirectoryContents path
    let filtered = filter (isSuffixOf "tsx") all
    mapM_ (\f -> processFile $ path </> f) filtered

processFile :: FilePath -> IO()
processFile file = do
  putStr $ "Processing: " ++ (takeDirectory file) ++ "/"
  setSGR [ SetConsoleIntensity BoldIntensity , SetColor Foreground Dull Blue]
  putStr $ takeFileName file
  setSGR [Reset]
  putStrLn ""


