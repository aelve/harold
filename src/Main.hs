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
  , functionComplexity :: Maybe String
  , functionCode       :: String
  }

instance FromJSON Entry where
    parseJSON (Object v) = Entry
      <$> v .:  "name"
      <*> v .:? "complexity"
      <*> v .:  "code"
    parseJSON _          = mzero

showEntry :: Entry -> String
showEntry e =
  printf "function name: %s\n" (functionName e) ++
  maybe "" (printf "complexity: %s\n") (functionComplexity e) ++
  "implementation:\n\n" ++
  (unlines . map ("    " ++) . lines $ functionCode e)

main :: IO ()
main = do
  let isRegularFile = (== Find.RegularFile) <$> Find.fileType
  files <- Find.find (return True) isRegularFile "db"
  entries <- fmap catMaybes $ forM files $ \f -> do
    res <- decodeFileEither f
    case res of
      Left  err   -> printf "%s: %s" f (show err) >> return Nothing
      Right entry -> return (Just entry)
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