{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List (intercalate)
import           Data.Maybe
import           Data.Yaml
import qualified System.FilePath.Find as Find
import           System.IO
import           Text.Printf

data Entry = Entry
  {
    functionName       :: String
  , functionPackage    :: String
  , functionModules    :: [String]
  , functionComplexity :: Maybe String
  , functionCode       :: String
  }

instance FromJSON Entry where
    parseJSON (Object v) = Entry
      <$> v .:  "name"
      <*> v .:  "package"
      <*> v .:  "modules"
      <*> v .:? "complexity"
      <*> v .:  "code"
    parseJSON _          = mzero

showEntry :: Entry -> String
showEntry e =
  printf "function: %s\n" (functionName e) ++
  printf "available from: %s (%s)\n"
    (functionPackage e)
    (intercalate ", " $ functionModules e) ++
  maybe "" (printf "complexity: %s\n") (functionComplexity e) ++
  "implementation:\n\n" ++
  (unlines . map ("    " ++) . lines $ functionCode e)

main :: IO ()
main = do
  let isRegularFile = (== Find.RegularFile) <$> Find.fileType
  files <- Find.find (return True) isRegularFile "db"
  entries <- fmap concat $ forM files $ \f -> do
    mbRes <- decodeFileEither f
    case mbRes of
      Left  err -> printf "%s: %s" f (show err) >> return []
      Right res -> return res
  repl entries

repl :: [Entry] -> IO ()
repl entries = do
  putStr "> "
  hFlush stdout
  query <- getLine
  unless (query == "/quit") $ do
    let matching = filter ((== query) . functionName) entries
    putStr . intercalate "\n" . map showEntry $ matching
    repl entries