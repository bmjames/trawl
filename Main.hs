
module Main where

import Process

import Prelude hiding (foldr)
import Data.Foldable (foldr)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.String.Utils (replace)
import Distribution.InstalledPackageInfo
import Options.Applicative
import System.Directory (canonicalizePath, doesFileExist)
import System.Exit hiding (die)
import System.FilePath ((</>), addExtension)
import System.IO (hPutStrLn, stderr)

data TrawlOpts = FindPackage String | FindModule String

trawlOpts :: Parser TrawlOpts
trawlOpts = FindPackage <$> strOption ( long "package"
                                       <> short 'p'
                                       <> metavar "PACKAGE"
                                       <> help "Find the haddock index for PACKAGE" )
        <|> FindModule  <$> strOption ( long "module"
                                     <> short 'm'
                                     <> metavar "MODULE"
                                     <> help "Find the haddock page for MODULE" )

main :: IO ()
main = execParser opts >>= trawl where
  opts = info (helper <*> trawlOpts) fullDesc

trawl :: TrawlOpts -> IO ()
trawl (FindPackage pkg) = do
  haddockIndex <- packageHaddockIndex pkg
  printExistingFileOrDie haddockIndex
trawl (FindModule mod) = do
  haddockFile <- moduleHaddock mod
  printExistingFileOrDie haddockFile

packageHaddockIndex :: String -> IO FilePath
packageHaddockIndex pkg = (</> "index.html") <$> packageHaddock pkg

packageHaddock :: String -> IO FilePath
packageHaddock pkg = do
  out <- ghcPkg ["describe", pkg]
  let errMsg = "Output of `ghc-pkg describe " ++ pkg ++ "` contained no haddock paths"
  case parseInstalledPackageInfo out of
    ParseFailed err -> die $ "Failed to parse package info: " ++ show err
    ParseOk _ info  -> case haddockHTMLs info of
                         path:_ -> canonicalHaddockPath (pkgRoot info) path
                         _      -> die errMsg

  where canonicalHaddockPath rootPath y = canonicalizePath $ foldr (replace "$topdir") y rootPath

moduleHaddock :: String -> IO FilePath
moduleHaddock mod = do
  out <- ghcPkg ["find-module", mod]
  let errMsg = "Output of `ghc-pkg find-module " ++ mod ++ "` contained no packages"
  haddockRoot <- case words out of pkg:_ -> packageHaddock pkg
                                   _     -> die errMsg
  let moduleFile = intercalate "-" (splitOn "." mod) `addExtension` "html"
  return $ haddockRoot </> moduleFile

ghcPkg :: [String] -> IO String
ghcPkg args = do
  ProcessResult exitCode out err <- processResult "." "ghc-pkg" $ ["-v0", "--simple-output"] ++ args
  case exitCode of
    ExitSuccess   -> return out
    ExitFailure c -> do
      hPutStrLn stderr err
      die $ "ghc-pkg exited with code " ++ show c

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

printExistingFileOrDie :: FilePath -> IO ()
printExistingFileOrDie file = do
  exists <- doesFileExist file
  if exists
     then putStrLn file
     else die $ "File does not exist: " ++ file
