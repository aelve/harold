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
import           Data.Foldable (for_)
import           Data.Maybe
-- Yaml parsing.
import           Data.Aeson (withObject, withText)
import           Data.Yaml
-- Lenses.
import           Lens.Micro
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
import           Data.List (intersperse, intercalate, nub, isPrefixOf)
import           Data.List.Split (chunksOf)
-- Harold-specific.
import           Harold.TextOutput

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

-- | Package version.
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

-- | Haskell operator fixity (precedence + associativity).
data Fixity = Fixity Int FixityDirection

-- | Haskell operator associativity.
data FixityDirection
  = InfixL            -- ^ Left-associative.
  | InfixR            -- ^ Right-associative.
  | InfixN            -- ^ Non-associative.

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
      -- on '.:' failing the parse if an object of a different type was
      -- encountered.
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

-- | Produces stuff like "base (Prelude, Data.List); since v4.7.0.0".
showLocation :: Location -> TextOutput
showLocation (Location package modules versions) = do
  line [package, " (", list modules, ")"]
  unless (null versions) $
    line ["; ", showVersionRanges versions]

-- | Produce a textual description of the entry in the knowledge base.
showEntry :: Entry -> TextOutput

showEntry (Function name locs impls) = do
  line1 (showAsName name)
  let signatures = nub (impls ^.. each.signature)
  indent 2 $
    mapM_ (\s -> line [":: ", s]) signatures
  blank

  line1 "available from:"
  indent 2 $
    mapM_ showLocation locs
  blank

  line1 "implementations:"
  blank

  indent 2 $
    sequence_ . intersperse blank $
      map (showFuncImpl name) impls

showEntry (Class name locs impls) = do
  line1 name
  blank

  line1 "available from:"
  indent 2 $
    mapM_ showLocation locs
  blank

  line1 "implementations:"
  blank

  indent 2 $
    sequence_ . intersperse blank $
      map showClassImpl impls

showEntry (Data name locs impls) = do
  line1 name
  blank

  line1 "available from:"
  indent 2 $
    mapM_ showLocation locs
  blank

  line1 "implementations:"
  blank

  indent 2 $
    sequence_ . intersperse blank $
      map showDataImpl impls

-- | Show an implementation of a function.
showFuncImpl
  :: String     -- ^ Function name.
  -> FuncImpl
  -> TextOutput
showFuncImpl name (FuncImpl genre signature comp fixity code) = do
  line [genre, ":"]
  indent 4 $ do
    for_ fixity $ \f ->
      line [showFixity f, " ", showAsOperator name]
    for_ comp $ \c ->
      line ["-- complexity: ", c]
    line [showAsName name, " :: ", signature]
    mapM_ line1 (maybe [] lines code)

showFixityDirection :: FixityDirection -> String
showFixityDirection d = case d of
  InfixL -> "infixl"
  InfixR -> "infixr"
  InfixN -> "infix"

-- | Produces stuff like "infixl 7".
showFixity :: Fixity -> String
showFixity (Fixity prec dir) =
  showFixityDirection dir ++ " " ++ show prec

showClassImpl :: ClassImpl -> TextOutput
showClassImpl (ClassImpl genre signature methods) = do
  line [genre, ":"]
  indent 4 $ do
    line ["class ", signature, " where"]
    indent 2 $
      mapM_ showClassMethods methods

showClassMethods :: ClassMethods -> TextOutput
showClassMethods (ClassMethods names signature) =
  line [list (map showAsName names), " :: ", signature]

showDataImpl :: DataImpl -> TextOutput
showDataImpl (DataImpl genre signature derivs constrs) = do
  line [genre, ":"]
  indent 4 $ do
    line ["data ", signature]
    indent 2 $ do
      case constrs of
        []      -> return ()
        (c1:cs) -> do
          line ["= ", showConstructor c1]
          mapM_ (\c -> line ["| ", showConstructor c]) cs
      unless (null derivs) $
        line ["deriving (", list derivs, ")"]

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
    putStr . getText .
      sequence_ . intersperse blank .
      map showEntry $ matching
    repl entries
