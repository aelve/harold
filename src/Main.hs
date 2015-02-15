{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List (intercalate, nub)
import           Data.List.Split (chunksOf)
import           Data.Maybe
import           Data.Yaml
import qualified System.FilePath.Find as Find
import           System.IO
import           Text.Printf

type Version = String

-- | Version ranges in which a function or whatever was available. The 1st
-- version is when something was introduces, the 2nd - when it was removed,
-- the 3rd - when it was introduced again, etc.
type VersionRanges = [Version]

-- | Display version ranges in a human-readable way.
--
-- >>> showVersionRanges ["4.2"]
-- "since v4.2"
--
-- >>> showVersionRanges ["0", "4.2"]
-- "removed in v4.2"
--
-- >>> showVersionRanges ["1.3.3", "4.2"]
-- "introduced in v1.3.3, removed in v4.2"
--
-- >>> showVersionRanges ["0", "1.3.3", "4.2"]
-- "was removed in v1.3.3, reintroduced in v4.2"
--
showVersionRanges :: VersionRanges -> String
showVersionRanges vs = case vs of
  []          -> ""
  ["0"]       -> ""
  [v]         -> printf "since v%s" v
  ["0", u]    -> printf "removed in v%s" u
  [v, u]      -> printf "introduced in v%s, removed in v%s" v u
  ["0", u, v] -> printf "was removed in v%s, reintroduced in v%s" u v
  vus         -> "present in versions " ++
                 intercalate ", " (map showRange (chunksOf 2 vus))
                 where
                   showRange [v, u] = v ++ "–" ++ u
                   showRange [v]    = v ++ " and onwards"

-- | Location of an 'Entry' – package and modules containing it, along with
-- versions of the package for which it holds true.
data Location = Location {
    locPackage  :: String
  , locModules  :: [String]
  , locVersions :: VersionRanges
  }

-- | An entry in the knowledge base.
data Entry
  = Function {
      entryName               :: String
    , entryLocation           :: [Location]
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
    , entryLocation           :: [Location]
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

-- | The default package name is "base". The default version range is "always
-- been there".
instance FromJSON Location where
  parseJSON (Object v) = Location
    <$> v .:? "package" .!= "base"
    <*> v .:  "modules"
    <*> v .:? "versions" .!= []
  parseJSON _          = mzero

instance FromJSON Entry where
  parseJSON (Object v) = parseFunction <|> parseClass
    where
      parseFunction = Function
        <$> v .:  "name"
        <*> v .:  "location"
        <*> v .:? "complexity"
        <*> v .:  "implementations"
      parseClass = Class
        <$> v .:  "name"
        <*> v .:  "location"
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

-- | Produces stuff like "base (Prelude, Data.List); since v4.7.0.0".
showLocation :: Location -> String
showLocation (Location package modules versions) =
  package ++ " (" ++ intercalate ", " modules ++ ")" ++
  if not (null versions) then "; " ++ showVersionRanges versions
                         else ""

-- | Produce a textual description of the entry in the knowledge base.
showEntry :: Entry -> String

showEntry (Function name locs comp impls) = unlines . concat $
  [
    [name]
  , let types = nub (map funcImplType impls)
    in  map ("  :: " ++) types
  , [""]
  , ["available from:"]
  , map (\l -> "  " ++ showLocation l) locs
  , [""]
  , ["implementations:"]
  , [""]
  , map (indent 2 . showFuncImpl name) impls
  ]

showEntry (Class name locs impls) = unlines . concat $
  [
    [name]
  , [""]
  , ["available from:"]
  , map (\l -> "  " ++ showLocation l) locs
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
