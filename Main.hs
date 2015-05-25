module Main where

import Process

import Data.List
import Options.Applicative
import System.Exit

data TrawlOpts = FindPackage String | FindModule String

trawlOpts :: Parser TrawlOpts
trawlOpts = FindPackage <$> strOption ( long "package"
                                       <> short 'p'
                                       <> metavar "PACKAGE"
                                       <> help "Find the haddock index for PACKAGE" )
        <|> FindModule  <$> strOption ( long "module"
                                     <> short 'm'
                                     <> metavar "MODULE"
                                     <> help "Find the haddock index for the package containing MODULE" )

main :: IO ()
main = execParser opts >>= trawl where
  opts = info (helper <*> trawlOpts) fullDesc

trawl :: TrawlOpts -> IO ()
trawl (FindPackage pkg) = do
  Just haddockIndex <- findPackageHaddock pkg
  putStrLn haddockIndex
trawl (FindModule mod) = do
  ProcessResult ExitSuccess out _ <- ghcPkg ["find-module", mod]
  Just haddockIndex <- case words out of pkg:_ -> findPackageHaddock pkg
  putStrLn haddockIndex

findPackageHaddock :: String -> IO (Maybe FilePath)
findPackageHaddock pkg = do
  ProcessResult ExitSuccess out _ <- ghcPkg ["describe", pkg]
  return $ drop 14 <$> find ("haddock-html:" `isPrefixOf`) (lines out)

ghcPkg :: [String] -> IO ProcessResult
ghcPkg args = processResult "." "ghc-pkg" $ ["-v0", "--global", "--simple-output"] ++ args
