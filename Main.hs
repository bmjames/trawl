module Main where

import Process

import Data.Maybe (listToMaybe)
import Data.List (intersperse)
import Data.List.Split (splitOn)
import Distribution.InstalledPackageInfo
import Options.Applicative
import System.Exit
import System.FilePath ((</>), addExtension)

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
  Just haddockIndex <- packageHaddockIndex pkg
  putStrLn haddockIndex
trawl (FindModule mod) = do
  Just haddockFile <- moduleHaddock mod
  putStrLn haddockFile

packageHaddockIndex :: String -> IO (Maybe FilePath)
packageHaddockIndex pkg = (fmap . fmap) (</> "index.html") $ packageHaddock pkg

packageHaddock :: String -> IO (Maybe FilePath)
packageHaddock pkg = do
  ProcessResult ExitSuccess out _ <- ghcPkg ["describe", pkg]
  case parseInstalledPackageInfo out of
    ParseFailed err -> die $ "Failed to parse package info: " ++ show err
    ParseOk _ info  -> return $ listToMaybe (haddockHTMLs info)

moduleHaddock :: String -> IO (Maybe FilePath)
moduleHaddock mod = do
  ProcessResult ExitSuccess out _ <- ghcPkg ["find-module", mod]
  haddockRoot <- case words out of pkg:_ -> packageHaddock pkg
  let moduleFile = concat (intersperse "-" $ splitOn "." mod) `addExtension` "html"
  return $ (</> moduleFile) <$> haddockRoot

ghcPkg :: [String] -> IO ProcessResult
ghcPkg args = processResult "." "ghc-pkg" $ ["-v0", "--global", "--simple-output"] ++ args

die :: String -> IO a
die msg = putStrLn msg >> exitFailure
