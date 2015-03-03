{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , MultiParamTypeClasses
  , FunctionalDependencies
  , TypeSynonymInstances
  , FlexibleInstances
  , TemplateHaskell
  #-}

module Main where

-- Generic useful stuff.
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
-- Yaml parsing.
import           Data.Aeson (withObject, withText)
import           Data.Yaml
-- Lenses.
import           Lens.Micro
import           Lens.Micro.Each
import           Lens.Micro.TH
-- IO.
import qualified System.FilePath.Find as Find
import           System.IO
-- Text.
import qualified Data.Text as T
import           Text.Printf
import           Text.Read (readMaybe)
import           Data.Char
-- Lists.
import           Data.List (intercalate, nub, isPrefixOf)
import           Data.List.Split (chunksOf)

-- | Like 'unwords', but also adds commas.
--
-- >>> list ["1", "2", "3"]
-- "1, 2, 3"
--
list :: [String] -> String
list = intercalate ", "

-- | Check whether the string is an operator, according to Haskell syntax.
isOperator :: String -> Bool
isOperator = all isOperatorChar
  where
    isOperatorChar c = (isSymbol c || isPunctuation c) &&
                       (c `notElem` "_'()[]{}\"")

-- | Wrap an operator in parens, leave other names unchanged.
--
-- >>> showAsName "+"
-- "(+)"
--
-- >>> showAsName "f"
-- "f"
--
showAsName :: String -> String
showAsName s
  | isOperator s = "(" ++ s ++ ")"
  | otherwise    = s

-- | Wrap a function in backticks, leave operators unchanged.
--
-- >>> showAsOperator "+"
-- "+"
--
-- >>> showAsOperator "f"
-- "`f`"
--
showAsOperator :: String -> String
showAsOperator s
  | isOperator s = s
  | otherwise    = "`" ++ s ++ "`"

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
    locationPackage  :: String
  , locationModules  :: [String]
  , locationVersions :: VersionRanges
  }

data Fixity = Fixity Int FixityDirection

data FixityDirection = InfixL | InfixR | InfixN

-- | An entry in the knowledge base.
data Entry
  = Function {
      entryName     :: String
    , entryLocation :: [Location]
    -- | Implementations of the function.
    , funcImpls     :: [FuncImpl]
    }
  | Class {
      entryName     :: String
    , entryLocation :: [Location]
    -- | Implementations of the class.
    , classImpls    :: [ClassImpl]
    }
  | Data {
      entryName     :: String
    , entryLocation :: [Location]
    -- | Implementations of the datatype.
    , dataImpls     :: [DataImpl]
    }

-- | Implementation of a function/constructor.
data FuncImpl = FuncImpl {
  -- | Genre (@report@, @naive@, etc.).
    funcImplGenre       :: String
  -- | Type signature.
  , funcImplSignature   :: String
  -- | Asymptotic complexity.
  , funcImplComplexity  :: Maybe String
  -- | Fixity of the function.
  , funcImplFixity      :: Maybe Fixity
  -- | Source (can be absent for constructors of class methods).
  , funcImplSource      :: Maybe String
  }

-- | Implementation of a class.
data ClassImpl = ClassImpl {
  -- | Genre (@report@, @naive@, etc.).
    classImplGenre      :: String
  -- | Signature (everything between @class@ and @where@).
  , classImplSignature  :: String
  -- | Methods.
  , classImplMethods    :: [ClassMethods]
  }

-- | Methods of a class (the reason for having several methods in one object
-- is that we'd like to preserve original grouping of methods).
data ClassMethods = ClassMethods {
  -- | Names of the methods.
    classMethodsNames     :: [String]
  -- | Type signature, common for all methods (without class constraint).
  , classMethodsSignature :: String
  }

-- | Implementation of a datatype.
data DataImpl = DataImpl {
  -- | Genre (@report@, @naive@, etc.).
    dataImplGenre        :: String
  -- | A signature (everything between @data@ and @=@).
  , dataImplSignature    :: String
  -- | List of automatically derived instances.
  , dataImplDeriving     :: [String]
  -- | Contstructors.
  , dataImplConstructors :: [Constructor]
  }

-- | A datatype constructor.
data Constructor = Constructor {
    constructorName      :: String
  -- | For 'True' it'd be @[]@. For 'Just', @["a"]@.
  , constructorParams    :: [String]
  }

makeFields ''Location
makeFields ''Entry
makeFields ''FuncImpl
makeFields ''ClassImpl
makeFields ''ClassMethods
makeFields ''DataImpl
makeFields ''Constructor

-- | The default package name is "base". The default version range is "always
-- been there".
instance FromJSON Location where
  parseJSON = withObject "location" $ \v ->
    Location
      <$> v .:? "package" .!= "base"
      <*> v .:  "modules"
      <*> v .:? "versions" .!= []

instance FromJSON Entry where
  parseJSON = withObject "entry" $ \v ->
    -- The order has to be like this because the function parser accepts any
    -- names, but not other parsers (e.g. parseClass wants the name to start
    -- with "class").
    parseClass v <|> parseData v <|> parseFunction v
    where
      parseFunction v = do
        entryName     <- v .:  "name"
        entryLocation <- v .:  "location"
        funcImpls     <- v .:  "implementations"
        return Function{..}
      parseClass v = do
        entryName     <- v .:  "name"
        guard ("class " `isPrefixOf` entryName)
        entryLocation <- v .:  "location"
        classImpls    <- v .:  "implementations"
        return Class{..}
      parseData v = do
        entryName     <- v .:  "name"
        guard ("data " `isPrefixOf` entryName)
        entryLocation <- v .:  "location"
        dataImpls     <- v .:  "implementations"
        return Data{..}

instance FromJSON Fixity where
  parseJSON = withText "fixity" $ \s -> do
    let (d, n) = break (== ' ') (T.unpack s)
    dir <- case d of
      "infixl" -> return InfixL
      "infixr" -> return InfixR
      "infix"  -> return InfixN
      other    -> fail ("unknown fixity: '" ++ other ++ "'")
    prec <- case readMaybe n of
      Nothing -> fail ("couldn't read '" ++ n ++ "' as a number")
      Just p  -> return p
    unless (prec >= 0 && prec <= 9) $
      fail ("precedence can't be " ++ show prec)
    return (Fixity prec dir)

instance FromJSON FuncImpl where
  parseJSON = withObject "function implementation" $ \v ->
    FuncImpl
      <$> v .:  "name"
      <*> v .:  "type"
      <*> v .:? "complexity"
      <*> v .:? "fixity"
      <*> v .:? "code"

instance FromJSON ClassImpl where
  parseJSON = withObject "class implementation" $ \v ->
    ClassImpl
      <$> v .:  "name"
      <*> v .:  "type"
      <*> v .:  "methods"

instance FromJSON ClassMethods where
  parseJSON = withObject "class methods" $ \v ->
    ClassMethods
      <$> nameOrNames v
      <*> v .:  "type"
    where
      -- This parses either a single string, or a list of strings. It relies
      -- on '.:' failing the parse if an object of a different type was encountered.
      nameOrNames v =
        fmap (:[]) (v .: "name") <|>    -- Here '.:' expects a single String.
        (v .: "name")                   -- Here '.:' expects [String].

instance FromJSON DataImpl where
  parseJSON = withObject "datatype implementation" $ \v ->
    DataImpl
      <$> v .:  "name"
      <*> v .:  "type"
      <*> v .:? "deriving" .!= []
      <*> v .:  "constructors"

instance FromJSON Constructor where
  parseJSON = withObject "datatype constructor" $ \v ->
    Constructor
      <$> v .:  "name"
      <*> v .:? "params" .!= []

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

showEntry (Function name locs impls) = unlines . concat $
  [
    [showAsName name]
  , let types = nub (impls ^.. each.signature)
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
showFuncImpl name (FuncImpl genre signature comp fixity code) =
  unlines . concat $
  [
    [genre ++ ":"]
  , ["    " ++ showFixity f ++ " " ++ showAsOperator name
      | Just f <- [fixity]]
  , ["    -- complexity: " ++ c | Just c <- [comp]]
  , ["    " ++ showAsName name ++ " :: " ++ signature]
  , [indent 4 c | Just c <- [code]]
  ]

-- | Produces stuff like "infixl 7".
showFixity :: Fixity -> String
showFixity (Fixity prec dir) =
  case dir of {InfixL -> "infixl"; InfixR -> "infixr"; InfixN -> "infix"} ++
  " " ++ show prec

showClassImpl :: ClassImpl -> String
showClassImpl (ClassImpl genre signature methods) = unlines . concat $
  [
    [genre ++ ":"]
  , ["    class " ++ signature ++ " where"]
  , [concatMap (indent 6 . showClassMethods) methods]
  ]

showClassMethods :: ClassMethods -> String
showClassMethods (ClassMethods names signature) =
  list (map showAsName names) ++ " :: " ++ signature

showDataImpl :: DataImpl -> String
showDataImpl (DataImpl genre signature derivs constrs) = unlines . concat $
  [
    [genre ++ ":"]
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
    let matching = filter ((== query) . view name) entries
    putStr . intercalate "\n" . map showEntry $ matching
    repl entries
