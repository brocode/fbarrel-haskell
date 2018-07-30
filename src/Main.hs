{-# LANGUAGE DataKinds #-}

module Main where

import Options.Declarative
import Control.Monad.IO.Class (liftIO)
import System.Directory (getDirectoryContents)
import Data.List (isSuffixOf)
import System.FilePath ((</>))

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
    mapM_ (processFile path) filtered

processFile :: String -> String -> IO()
processFile path file = do
  putStrLn $ path </> file


