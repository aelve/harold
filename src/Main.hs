{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List (intercalate, nub, isPrefixOf)
import           Data.List.Split (chunksOf)
import           Data.Maybe
import           Data.Yaml
import qualified System.FilePath.Find as Find
import           System.IO
import           Text.Printf

-- | Like 'unwords', but also adds commas.
--
-- >>> list ["1", "2", "3"]
-- "1, 2, 3"
--
list :: [String] -> String
list = intercalate ", "

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
                 list (map showRange (chunksOf 2 vus))
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
  | Data {
      entryName               :: String
    , entryLocation           :: [Location]
    , dataImpls               :: [DataImpl]
    }

-- | Implementation of a function/constructor.
data FuncImpl = FuncImpl {
  -- | Name (@report@, @naive@, etc.).
    funcImplName        :: String
  -- | Type signature.
  , funcImplType        :: String
  -- | Asymptotic complexity.
  , funcImplComplexity  :: Maybe String
  -- | Code (can be absent for constructors of class methods).
  , funcImplCode        :: Maybe String
  }

-- | Implementation of a class.
data ClassImpl = ClassImpl {
  -- | Name (@report@, @naive@, etc.).
    classImplName       :: String
  -- | Type signature.
  , classImplType       :: String
  -- | Methods.
  , classImplMethods    :: [ClassMethods]
  }

-- | Methods of a class (the reason for having several methods in one object
-- is that we'd like to preserve original grouping of methods).
data ClassMethods = ClassMethods {
  -- Names of the methods.
    classMethodsNames    :: [String]
  -- | Type signature, common for all methods (without class constraint).
  , classMethodsType     :: String
  }

-- | Implementation of a datatype.
data DataImpl = DataImpl {
    dataImplName         :: String
  -- | A signature (something like "Maybe a").
  , dataImplType         :: String
  -- | List of automatically derived instances.
  , dataImplDeriving     :: [String]
  , dataImplConstructors :: [Constructor]
  }

-- | A datatype constructor.
data Constructor = Constructor {
    constructorName      :: String
  -- | For 'True' it'd be @[]@. For 'Just', @["a"]@.
  , constructorParams    :: [String]
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
  parseJSON (Object v) =
    -- The order has to be like this because the function parser accepts any
    -- names, but not other parsers (e.g. parseClass wants the name to start
    -- with "class").
    parseClass <|> parseData <|> parseFunction
    where
      parseFunction = do
        entryName      <- v .:  "name"
        entryLocation  <- v .:  "location"
        funcComplexity <- v .:? "complexity"
        funcImpls      <- v .:  "implementations"
        return Function{..}
      parseClass = do
        entryName     <- v .: "name"
        guard ("class " `isPrefixOf` entryName)
        entryLocation <- v .: "location"
        classImpls    <- v .: "implementations"
        return Class{..}
      parseData = do
        entryName     <- v .: "name"
        guard ("data " `isPrefixOf` entryName)
        entryLocation <- v .: "location"
        dataImpls     <- v .: "implementations"
        return Data{..}
  parseJSON _          = mzero

instance FromJSON FuncImpl where
  parseJSON (Object v) = FuncImpl
    <$> v .:  "name"
    <*> v .:  "type"
    <*> v .:? "complexity"
    <*> v .:? "code"
  parseJSON _          = mzero

instance FromJSON ClassImpl where
  parseJSON (Object v) = ClassImpl
    <$> v .:  "name"
    <*> v .:  "type"
    <*> v .:  "methods"
  parseJSON _          = mzero

instance FromJSON ClassMethods where
  parseJSON (Object v) = ClassMethods
    <$> nameOrNames
    <*> v .:  "type"
    where
      -- This parses either a single string, or a list of strings. It relies
      -- on '.:' failing the parse if an object of a different type was encountered.
      nameOrNames = 
        fmap (:[]) (v .: "name") <|>    -- Here '.:' expects a single String.
        (v .: "name")                   -- Here '.:' expects [String].
  parseJSON _          = mzero

instance FromJSON DataImpl where
  parseJSON (Object v) = DataImpl
    <$> v .:  "name"
    <*> v .:  "type"
    <*> v .:? "deriving" .!= []
    <*> v .:  "constructors"
  parseJSON _          = mzero

instance FromJSON Constructor where
  parseJSON (Object v) = Constructor
    <$> v .:  "name"
    <*> v .:? "params" .!= []
  parseJSON _          = mzero

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

-- | Produces stuff like "base (Prelude, Data.List); since v4.7.0.0".
showLocation :: Location -> String
showLocation (Location package modules versions) =
  package ++ " (" ++ list modules ++ ")" ++
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

showEntry (Data name locs impls) = unlines . concat $
  [
    [name]
  , [""]
  , ["available from:"]
  , map (\l -> "  " ++ showLocation l) locs
  , [""]
  , ["implementations:"]
  , [""]
  , [concatMap (indent 2 . showDataImpl) impls]
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
  , [indent 4 c | Just c <- [code]]
  ]

showClassImpl :: ClassImpl -> String
showClassImpl (ClassImpl name signature methods) = unlines . concat $
  [
    [name ++ ":"]
  , ["    class " ++ signature ++ " where"]
  , [concatMap (indent 6 . showClassMethods) methods]
  ]

showClassMethods :: ClassMethods -> String
showClassMethods (ClassMethods names signature) =
  list names ++ " :: " ++ signature

showDataImpl :: DataImpl -> String
showDataImpl (DataImpl name signature derivs constrs) = unlines . concat $
  [
    [name ++ ":"]
  , ["    data " ++ signature]
  , ["      = " ++ showConstructor c | c <- take 1 constrs]
  , ["      | " ++ showConstructor c | c <- drop 1 constrs]
  , ["      deriving (" ++ list derivs ++ ")" | not (null derivs)]
  ]

showConstructor :: Constructor -> String
showConstructor (Constructor name params) =
  unwords (name : params)

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
