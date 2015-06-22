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

newtype PackageName = PackageName String
newtype ModuleName = ModuleName String

data TrawlOpts = FindPackage PackageName | FindModule ModuleName

trawlOpts :: Parser TrawlOpts
trawlOpts =
  FindPackage . PackageName <$>
    strOption (long "package" <>
               short 'p' <>
               metavar "PACKAGE" <>
               help "Find the haddock index for PACKAGE") <|>
  FindModule . ModuleName <$>
    strOption (long "module" <>
               short 'm' <>
               metavar "MODULE" <>
               help "Find the haddock page for MODULE")

main :: IO ()
main = execParser opts >>= trawl >>= uncurry printExistingFileOrDie
  where
    opts = info (helper <*> trawlOpts) fullDesc

trawl :: TrawlOpts -> IO (PackageName, FilePath)
trawl (FindPackage pkg) = (,) pkg <$> packageHaddockIndex pkg
trawl (FindModule mod) = moduleHaddock mod

packageHaddockIndex :: PackageName -> IO FilePath
packageHaddockIndex pkg = (</> "index.html") <$> packageHaddock pkg

packageHaddock :: PackageName -> IO FilePath
packageHaddock (PackageName pkg) = do
  out <- ghcPkg ["describe", pkg]
  let errMsg = "Output of `ghc-pkg describe " ++ pkg ++ "` contained no haddock paths"
  case parseInstalledPackageInfo out of
    ParseFailed err -> die $ "Failed to parse package info: " ++ show err
    ParseOk _ info  -> case haddockHTMLs info of
                         path:_ -> return $ realHaddockPath (pkgRoot info) path
                         _      -> die errMsg

  where realHaddockPath rootPath root = foldr (replace "$topdir") root rootPath

moduleHaddock :: ModuleName -> IO (PackageName, FilePath)
moduleHaddock (ModuleName mod) = do
  out <- ghcPkg ["find-module", mod]
  let errMsg = "Output of `ghc-pkg find-module " ++ mod ++ "` contained no packages"
  case words out of
    h:_ -> do
      let moduleFile = intercalate "-" (splitOn "." mod) `addExtension` "html"
          pkg = PackageName h
      haddockRoot <- packageHaddock pkg
      return (pkg, haddockRoot </> moduleFile)
    _ -> die errMsg

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

printExistingFileOrDie :: PackageName -> FilePath -> IO ()
printExistingFileOrDie (PackageName pkg) file = do
  exists <- doesFileExist file
  if exists
     then canonicalizePath file >>= putStrLn
     else do
       hPutStrLn stderr $ "File does not exist: " ++ file
       die suggestion

  where
    suggestion = "(Try reinstalling the package " ++ pkg ++
                 " with the --enable-documentation flag)"
