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
    functionName            :: String
  , functionPackage         :: String
  , functionModules         :: [String]
  , functionImplementations :: [Implementation]
  }

data Implementation = Implementation
  {
    implementationName       :: String
  , implementationComplexity :: Maybe String
  , implementationCode       :: String
  }

instance FromJSON Entry where
  parseJSON (Object v) = Entry
    <$> v .: "name"
    <*> v .: "package"
    <*> v .: "modules"
    <*> v .: "implementations"
  parseJSON _          = mzero

instance FromJSON Implementation where
  parseJSON (Object v) = Implementation
    <$> v .:  "name"
    <*> v .:? "complexity"
    <*> v .:  "code"
  parseJSON _          = mzero

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

showEntry :: Entry -> String
showEntry e =
  printf "function: %s\n" (functionName e) ++
  printf "available from: %s (%s)\n"
    (functionPackage e)
    (intercalate ", " $ functionModules e) ++
  "implementations:\n\n" ++
  intercalate "\n" (map (indent 2 . showImplementation) 
                        (functionImplementations e))

showImplementation :: Implementation -> String
showImplementation e =
  printf "%s:\n" (implementationName e) ++
  maybe "" (printf "  complexity: %s\n") (implementationComplexity e) ++
  "  code:\n" ++
  indent 4 (implementationCode e)

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