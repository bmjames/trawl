module Main where

import Process

import Prelude hiding (foldr)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Char
import Data.Foldable (foldr)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Monoid
import Distribution.InstalledPackageInfo
import Options.Applicative
import Options.Applicative.Types (ReadM(..), ParseError(..))
import System.Directory (canonicalizePath, doesFileExist)
import System.Exit hiding (die)
import System.FilePath ((</>), addExtension)
import System.IO (hPutStrLn, stderr)
import Text.Regex (mkRegex, subRegex)

newtype PackageName = PackageName String
newtype ModuleName = ModuleName String

data TrawlOpts = TrawlOpts
  { optsCmd :: TrawlCmd
  , optsEnv :: TrawlEnv
  }

data TrawlCmd
  = FindPackage PackageName
  | FindModule ModuleName
  | FindMember ModuleName String

data TrawlEnv = TrawlEnv { envStack :: Bool }

trawlOpts :: Parser TrawlOpts
trawlOpts =
  let pCmd =
        FindPackage . PackageName <$>
          strOption (long "package" <>
                     short 'p' <>
                     metavar "PACKAGE" <>
                     help "Find the haddock index for PACKAGE") <|>
        FindModule . ModuleName <$>
          strOption (long "module" <>
                     short 'm' <>
                     metavar "MODULE" <>
                     help "Find the haddock page for MODULE") <|>
        uncurry FindMember <$>
          option readModuleMember (long "member" <>
                                   short 'v' <>
                                   metavar "MODULE.MEMBER" <>
                                   help "Find the haddock page section for MEMBER of MODULE")

      pEnv = TrawlEnv <$> switch (long "stack" <> help "Use stack environment")

      readModuleMember :: ReadM (ModuleName, String)
      readModuleMember = ReadM . ReaderT $ \s ->
        case splitOn "." s of [x] -> fail "Expected format MODULE.MEMBER"
                              xs  -> return (ModuleName $ intercalate "." (init xs), last xs)

  in TrawlOpts <$> pCmd <*> pEnv

main :: IO ()
main = execParser opts >>= trawl
  where
    opts = info (helper <*> trawlOpts) fullDesc

trawl :: TrawlOpts -> IO ()
trawl (TrawlOpts (FindPackage pkg) env) =
  packageHaddockIndex env pkg >>= canonicalExistingFile pkg >>= putStr
trawl (TrawlOpts (FindModule mod) env) =
  moduleHaddock env mod >>= uncurry canonicalExistingFile >>= putStr
trawl (TrawlOpts (FindMember mod mem) env) = do
  canonicalPath <- moduleHaddock env mod >>= uncurry canonicalExistingFile
  putStr $ appendFragment mem canonicalPath

appendFragment :: String -> FilePath -> String
appendFragment f p = "file://" ++ p ++ "#v:" ++ makeAnchorId f

packageHaddockIndex :: TrawlEnv -> PackageName -> IO FilePath
packageHaddockIndex env pkg = (</> "index.html") <$> packageHaddock env pkg

packageHaddock :: TrawlEnv -> PackageName -> IO FilePath
packageHaddock env (PackageName pkg) = do
  out <- ghcPkg env ["describe", pkg]
  let errMsg = "Output of `ghc-pkg describe " ++ pkg ++ "` contained no haddock paths"
  case parseInstalledPackageInfo out of
    ParseFailed err -> die $ "Failed to parse package info: " ++ show err
    ParseOk _ info  -> case haddockHTMLs info of
                         path:_ -> return $ realHaddockPath (pkgRoot info) path
                         _      -> die errMsg

  where realHaddockPath = flip $ foldl (subRegex (mkRegex "\\$topdir"))

moduleHaddock :: TrawlEnv -> ModuleName -> IO (PackageName, FilePath)
moduleHaddock env (ModuleName mod) = do
  out <- ghcPkg env ["find-module", mod]
  let errMsg = "Output of `ghc-pkg find-module " ++ mod ++ "` contained no packages"
  case reverse (words out) of
    h:_ -> do
      let moduleFile = intercalate "-" (splitOn "." mod) `addExtension` "html"
          pkg = PackageName h
      haddockRoot <- packageHaddock env pkg
      return (pkg, haddockRoot </> moduleFile)
    _ -> die errMsg

ghcPkgCmd :: TrawlEnv -> [String] -> (String, [String])
ghcPkgCmd (TrawlEnv stack) args =
  let (cmd : cmdOpts) =
        (if stack then ["stack", "exec", "--"] else []) ++
        ["ghc-pkg", "-v0", "--simple-output"]
  in (cmd, cmdOpts ++ args)

ghcPkg :: TrawlEnv -> [String] -> IO String
ghcPkg env args =
  let (cmd, cmdArgs) = ghcPkgCmd env args
  in do ProcessResult exitCode out err <- processResult "." cmd cmdArgs
        case exitCode of
          ExitSuccess   -> return out
          ExitFailure c -> do
            hPutStrLn stderr err
            die $ cmd ++ " exited with code " ++ show c

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

canonicalExistingFile :: PackageName -> FilePath -> IO FilePath
canonicalExistingFile (PackageName pkg) file =
  let hint = "Try reinstalling the package " ++ pkg ++ " with --enable-documentation (cabal) or --haddock (stack)"
  in do
    exists <- doesFileExist file
    if exists
       then canonicalizePath file
       else do
         hPutStrLn stderr $ "File does not exist: " ++ file
         die hint

makeAnchorId :: String -> String
makeAnchorId [] = []
makeAnchorId (x:xs) = escape isAlpha x ++ concatMap (escape isLegal) xs
  where
    escape p c | p c = [c]
               | otherwise = '-' : show (ord c) ++ "-"
    isLegal ':' = True
    isLegal '_' = True
    isLegal '.' = True
    isLegal c = isAscii c && isAlphaNum c
