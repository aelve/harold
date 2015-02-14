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

-- | An entry in the knowledge base.
data Entry
  = Function {
      entryName               :: String
    , entryPackage            :: String
    , entryModules            :: [String]
    -- | Asymptotic complexity of the function. It's not a field of
    -- 'FunctionImplementation' because we want to distinguish between
    -- "default" complexity and "weird" complexity, but it's done wrong anyway
    -- so it should just be removed.
    , funcComplexity          :: Maybe String
    -- | Implementations of the function.
    , funcImpls               :: [FuncImpl]
    }
  | Class {
      entryName               :: String
    , entryPackage            :: String
    , entryModules            :: [String]
    -- | Implementations of the class.
    , classImpls              :: [ClassImpl]
    }

-- | Implementation of a function.
data FuncImpl = FuncImpl {
  -- | Name (@report@, @naive@, etc.).
    funcImplName        :: String
  -- | Type signature.
  , funcImplType        :: String
  -- | Asymptotic complexity.
  , funcImplComplexity  :: Maybe String
  -- | Code.
  , funcImplCode        :: String
  }

-- | Implementation of a class.
data ClassImpl = ClassImpl {
  -- | Name (@report@, @naive@, etc.).
    classImplName       :: String
  -- | Type signature.
  , classImplType       :: String
  -- | Methods.
  , classImplMethods    :: [ClassMethod]
  }

-- | Method of a class.
data ClassMethod = ClassMethod {
  -- Name of the method.
    classMethodName     :: String
  -- | Type signature (without class constraint).
  , classMethodType     :: String
  }

instance FromJSON Entry where
  parseJSON (Object v) = parseFunction <|> parseClass
    where
      parseFunction = Function
        <$> v .:  "name"
        <*> v .:  "package"
        <*> v .:  "modules"
        <*> v .:? "complexity"
        <*> v .:  "implementations"
      parseClass = Class
        <$> v .:  "name"
        <*> v .:  "package"
        <*> v .:  "modules"
        <*> v .:  "implementations"
  parseJSON _          = mzero

instance FromJSON FuncImpl where
  parseJSON (Object v) = FuncImpl
    <$> v .:  "name"
    <*> v .:  "type"
    <*> v .:? "complexity"
    <*> v .:  "code"
  parseJSON _          = mzero

instance FromJSON ClassImpl where
  parseJSON (Object v) = ClassImpl
    <$> v .:  "name"
    <*> v .:  "type"
    <*> v .:  "methods"
  parseJSON _          = mzero

instance FromJSON ClassMethod where
  parseJSON (Object v) = ClassMethod
    <$> v .:  "name"
    <*> v .:  "type"
  parseJSON _          = mzero

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

-- | Produce a textual description of the entry in the knowledge base.
showEntry :: Entry -> String

showEntry (Function name pkg modules comp impls) = unlines . concat $
  [
    [name]
  , let types = nub (map funcImplType impls)
    in  map ("  :: " ++) types
  , [""]
  , [printf "available from %s (%s)" pkg (intercalate ", " modules)]
  , [""]
  , ["implementations:"]
  , [""]
  , map (indent 2 . showFuncImpl name) impls
  ]

showEntry (Class name pkg modules impls) = unlines . concat $
  [
    [name]
  , [""]
  , [printf "available from %s (%s)" pkg (intercalate ", " modules)]
  , [""]
  , ["implementations:"]
  , [""]
  , [concatMap (indent 2 . showClassImpl) impls]
  ]

-- | Show an implementation of a function.
showFuncImpl
  :: String     -- ^ Function name.
  -> FuncImpl
  -> String
showFuncImpl funcName (FuncImpl name signature comp code) = unlines . concat $
  [
    [name ++ ":"]
  , ["    -- complexity: " ++ c | Just c <- [comp]]
  , ["    " ++ funcName ++ " :: " ++ signature]
  , [indent 4 code]
  ]

showClassImpl :: ClassImpl -> String
showClassImpl (ClassImpl name signature methods) = unlines . concat $
  [
    [name ++ ":"]
  , ["    class " ++ signature ++ " where"]
  , [concatMap (indent 6 . showClassMethod) methods]
  ]

showClassMethod :: ClassMethod -> String
showClassMethod (ClassMethod name signature) =
  name ++ " :: " ++ signature

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
    let matching = filter ((== query) . entryName) entries
    putStr . intercalate "\n" . map showEntry $ matching
    repl entries
