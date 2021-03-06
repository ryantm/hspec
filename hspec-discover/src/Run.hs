{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | A preprocessor that finds and combines specs.
module Run (
  run

-- exported for testing
, importList
, fileToSpec
, findSpecs
, getFilesRecursive
, driverWithFormatter
, moduleName
) where
import           Control.Monad
import           Control.Applicative
import           Data.List
import           Data.Char
import           Data.Maybe
import           Data.String
import           System.Environment
import           System.Exit
import           System.IO
import           System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist)
import           System.FilePath hiding (combine)

import           Config

instance IsString ShowS where
  fromString = showString

type Spec = String

run :: [String] -> IO ()
run args_ = do
  name <- getProgName
  case args_ of
    src : _ : dst : args -> case parseConfig name args of
      Left err -> do
        hPutStrLn stderr err
        exitFailure
      Right c -> do
        when (configNested c) (hPutStrLn stderr "hspec-discover: WARNING - The `--nested' flag is deprecated and will be removed in a future release!")
        specs <- findSpecs src
        writeFile dst (mkSpecModule src c specs)
    _ -> do
      hPutStrLn stderr (usage name)
      exitFailure

mkSpecModule :: FilePath -> Config -> [Spec] -> String
mkSpecModule src c nodes =
  ( "{-# LINE 1 " . shows src . " #-}"
  . showString ("module " ++ module_ ++" where\n")
  . importList nodes
  . maybe driver (driverWithFormatter (null nodes)) (configFormatter c)
  . formatSpecs nodes
  ) "\n"
  where
    driver =
        showString "import Test.Hspec\n"
      . case configNoMain c of
          False ->
              showString "main :: IO ()\n"
            . showString "main = hspec $ "
          True ->
              showString "spec :: Spec\n"
            . showString "spec = "
    module_ = if configNoMain c then pathToModule src else "Main"
    pathToModule f = let
        fileName = last $ splitDirectories f
        m:ms = takeWhile (/='.') fileName
     in
        toUpper m:ms


driverWithFormatter :: Bool -> String -> ShowS
driverWithFormatter isEmpty f =
    (if isEmpty then id else "import Test.Hspec\n")
  . showString "import Test.Hspec.Runner\n"
  . showString "import qualified " . showString (moduleName f) . showString "\n"
  . showString "main :: IO ()\n"
  . showString "main = hspecWithFormatter " . showString f . showString " $ "

moduleName :: String -> String
moduleName = reverse . dropWhile (== '.') . dropWhile (/= '.') . reverse

-- | Generate imports for a list of specs.
importList :: [Spec] -> ShowS
importList = foldr (.) "" . map f
  where
    f :: Spec -> ShowS
    f name = "import qualified " . showString name . "Spec\n"

-- | Combine a list of strings with (>>).
sequenceS :: [ShowS] -> ShowS
sequenceS = foldr (.) "" . intersperse " >> "

-- | Convert a list of specs to code.
formatSpecs :: [Spec] -> ShowS
formatSpecs xs
  | null xs   = "return ()"
  | otherwise = sequenceS (map formatSpec xs)

-- | Convert a spec to code.
formatSpec :: Spec -> ShowS
formatSpec name = "describe " . shows name . " " . showString name . "Spec.spec"

findSpecs :: FilePath -> IO [Spec]
findSpecs src = do
  let (dir, file) = splitFileName src
  mapMaybe fileToSpec . filter (/= file) <$> getFilesRecursive dir

fileToSpec :: FilePath -> Maybe String
fileToSpec f = intercalate "." . reverse <$> case reverse $ splitDirectories f of
  x:xs -> case stripSuffix "Spec.hs" x <|> stripSuffix "Spec.lhs" x of
    Nothing -> Nothing
    Just "" -> Nothing
    Just ys -> Just (ys : xs)
  _ -> Nothing
  where
    stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
    stripSuffix suffix str = reverse <$> stripPrefix (reverse suffix) (reverse str)

getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive baseDir = sort <$> go []
  where
    go :: FilePath -> IO [FilePath]
    go dir = do
      c <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents (baseDir </> dir)
      dirs <- filterM (doesDirectoryExist . (baseDir </>)) c >>= mapM go
      files <- filterM (doesFileExist . (baseDir </>)) c
      return (files ++ concat dirs)
