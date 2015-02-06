{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List (intercalate, nub)
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
  , functionComplexity      :: Maybe String
  , functionImplementations :: [Implementation]
  }

data Implementation = Implementation
  {
    implementationName       :: String
  , implementationType       :: String
  , implementationComplexity :: Maybe String
  , implementationCode       :: String
  }

instance FromJSON Entry where
  parseJSON (Object v) = Entry
    <$> v .:  "name"
    <*> v .:  "package"
    <*> v .:  "modules"
    <*> v .:? "complexity"
    <*> v .:  "implementations"
  parseJSON _          = mzero

instance FromJSON Implementation where
  parseJSON (Object v) = Implementation
    <$> v .:  "name"
    <*> v .:  "type"
    <*> v .:? "complexity"
    <*> v .:  "code"
  parseJSON _          = mzero

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

showEntry :: Entry -> String
showEntry e =
  -- Name.
  functionName e ++ "\n" ++
  -- Signatures.
  concatMap (printf "  :: %s\n")
            (nub . map implementationType . functionImplementations $ e) ++
  "\n" ++
  -- Package, modules.
  printf "available from %s (%s)\n"
    (functionPackage e)
    (intercalate ", " $ functionModules e) ++
  "\n" ++
  -- Implementations.
  "implementations:\n" ++
  "\n" ++
  intercalate "\n" (map (indent 2 . showImplementation (functionName e))
                        (functionImplementations e))

showImplementation :: String -> Implementation -> String
showImplementation name e =
  printf "%s:\n" (implementationName e) ++
  indent 4 code
  where
    code = maybe "" (printf "-- complexity: %s\n")
                    (implementationComplexity e) ++
           printf "%s :: %s\n" name (implementationType e) ++
           implementationCode e

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
