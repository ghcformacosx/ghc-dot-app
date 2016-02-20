-- #!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

{-

This builds a relocatable ghc-$version.app in dist/build,
including cabal-install 1.22.0.0

TODO:

* Pre-install some packages?

-}
module Main (main) where

import Data.Char (toUpper)
import Control.Applicative ((<$>))
import System.Environment (getArgs, unsetEnv)
import System.Directory
  ( getCurrentDirectory, getDirectoryContents, doesDirectoryExist
  , setCurrentDirectory, createDirectoryIfMissing, doesFileExist
  , copyFile
  )
import System.FilePath ((</>), dropExtension, takeExtension, takeFileName)
import System.Process (callProcess, readProcess)
import Control.Monad (when, filterM, foldM)
import System.Posix.Files
  ( getSymbolicLinkStatus, getFileStatus, isSymbolicLink, fileSize )
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Control.Exception as C
import System.Console.GetOpt
  ( ArgOrder(..)
  , OptDescr(..)
  , ArgDescr(..)
  , usageInfo
  , getOpt
  )

-- Layout:
--
--   dist/download/[ghc-$version-*.tar.bz2]
--   dist/unpack/ghc-$version
--   dist/build/ghc-$version.app/Contents/{lib,bin}/
--

data BuildState = BuildState
  { buildRel             :: Release
  , buildCabalRel        :: Release
  , buildStackRel        :: Release
  , buildDistDir         :: String
  , buildDownloadDir     :: String
  , buildUnpackDir       :: String
  , buildUnpackDest      :: String
  , buildBuildDir        :: String
  , buildAppDir          :: String
  , buildPrefixDir       :: String
  , buildPkgRoot         :: String
  , buildGhcName         :: String
  , buildBinDir          :: String
  , buildConfDir         :: String
  , buildShareDir        :: String
  } deriving (Show, Eq)

data Releases = Releases
  { releasesCabal :: Release
  , releasesStack :: Release
  , releasesGhc   :: Release
  }

data Release = Release
  { releaseVersion :: String
  , releaseUrl     :: String
  , releaseSha1    :: String
  , releaseSize    :: Int
  } deriving (Show, Eq)

data Rule = Rule
  { ruleName         :: String
  , ruleCheck        :: IO Bool
  , ruleRun          :: IO ()
  , ruleDependencies :: [Rule]
  }

releaseFileName :: Release -> FilePath
releaseFileName = takeFileName . releaseUrl

buildState :: Releases -> FilePath -> BuildState
buildState releases here = b
  where
    rel = releasesGhc releases
    cabalRel = releasesCabal releases
    stackRel = releasesStack releases
    distDir = here </> "dist"
    n = buildGhcName b
    b = BuildState
      { buildRel             = rel
      , buildCabalRel        = cabalRel
      , buildStackRel        = stackRel
      , buildDistDir         = distDir
      , buildDownloadDir     = distDir </> "download"
      , buildUnpackDir       = distDir </> "unpack"
      , buildUnpackDest      = buildUnpackDir b </> n
      , buildBuildDir        = distDir </> "build"
      , buildAppDir          = buildBuildDir b </> n ++ ".app"
      , buildPrefixDir       = buildAppDir b </> "Contents"
      , buildPkgRoot         = buildPrefixDir b </> "lib" </> n
      , buildGhcName         = "ghc-" ++ releaseVersion rel
      , buildBinDir          = buildPrefixDir b </> "bin"
      , buildConfDir         = buildPkgRoot b </> "package.conf.d"
      , buildShareDir        = buildPrefixDir b </> "share"
      }

defRule :: Rule
defRule = Rule
  { ruleName         = "unnamed rule"
  , ruleCheck        = return False
  , ruleRun          = return ()
  , ruleDependencies = []
  }

latestGhc :: Release
latestGhc = Release
  { releaseVersion = "7.10.3"
  , releaseUrl     = "http://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-x86_64-apple-darwin.tar.xz"
  , releaseSha1    = "fe0d26e6c65e911fbc2897700c35f561d3b56710"
  , releaseSize    = 88133560
  }

latestCabal :: Release
latestCabal = Release
  { releaseVersion = "1.22.6.0"
  , releaseUrl     = "https://s3.halcyon.sh/osx-10.9-x86_64/halcyon-cabal-1.22.6.0.tar.gz"
  , releaseSha1    = "e941f12d5fde8820eb8374637712add1e168be14"
  , releaseSize    = 4583511
  }

latestStack :: Release
latestStack = Release
  { releaseVersion = "0.1.10.0"
  , releaseUrl     = "https://github.com/commercialhaskell/stack/releases/download/v0.1.10.0/stack-0.1.10.0-osx-x86_64.tar.gz"
  , releaseSha1    = "14336e401a08c4995ed4c4ee9f06da13f71a77c3"
  , releaseSize    = 8547551
  }

latestReleases :: Releases
latestReleases = Releases
  { releasesCabal = latestCabal
  , releasesStack = latestStack
  , releasesGhc   = latestGhc
  }

shellPreamble :: T.Text
shellPreamble = T.unlines
  [ "#!/bin/bash"
  , "SOURCE=\"${BASH_SOURCE[0]}\""
  , "# resolve $SOURCE until the file is no longer a symlink"
  , "while [ -h \"$SOURCE\" ]; do"
  , "  DIR=\"$( cd -P \"$( dirname \"$SOURCE\" )\" && pwd )\""
  , "   SOURCE=\"$(readlink \"$SOURCE\")\""
  , "  # if $SOURCE was a relative symlink, we need to resolve it relative to"
  , "  # the path where the symlink file was located"
  , "  [[ $SOURCE != /* ]] && SOURCE=\"$DIR/$SOURCE\""
  , "done"
  , "DIR=\"$( cd -P \"$(dirname \"$( dirname \"$SOURCE\" )\")\" && pwd )\""
  ]

isScript :: FilePath -> IO Bool
isScript = (not . isSymbolicLink <$>) . getSymbolicLinkStatus

runRule :: Rule -> IO ()
runRule rule = do
  putStrLn (ruleName rule)
  shouldRun <- not <$> ruleCheck rule
  when shouldRun $ do
    mapM_ runRule (ruleDependencies rule)
    ruleRun rule

fixupBin :: BuildState -> IO ()
fixupBin (BuildState { buildBinDir, buildPrefixDir }) = do
  binFiles <- map (buildBinDir </>) . filter ((/='.') . head) <$>
                getDirectoryContents buildBinDir
  scripts <- filterM isScript binFiles
  mapM_ (fixupScript buildPrefixDir) scripts

fixupScript :: FilePath -> FilePath -> IO ()
fixupScript buildPrefixDir fileName =
  B.readFile fileName >>=
    either (const $ return ()) replaceText . T.decodeUtf8'
  where
    prefixDir = T.pack buildPrefixDir
    binsh = "#!/bin/sh\n"
    replaceText s = when (binsh `T.isPrefixOf` s) $ do
      putStrLn fileName
      T.writeFile fileName .
        T.append shellPreamble .
        T.drop (T.length binsh) .
        T.replace prefixDir "${DIR}" $
        s

sanityCheck :: BuildState -> Rule
sanityCheck (BuildState { buildConfDir, buildDistDir }) = defRule
  { ruleName = "sanityCheck " ++ buildConfDir
  , ruleRun = do
      files <- filter ((".conf"==) . takeExtension) <$> getDirectoryContents buildConfDir
      mapM_ (checkBuildDir distDir . (buildConfDir </>)) files
  }
  where
    distDir  = T.pack buildDistDir

checkBuildDir :: T.Text -> FilePath -> IO ()
checkBuildDir distDir fileName = do
  s <- T.readFile fileName
  when (distDir `T.isInfixOf` s) $ do
    putStrLn ("FAIL: " ++ fileName)
    fail fileName

fixupConf :: BuildState -> IO ()
fixupConf (BuildState { buildPkgRoot, buildConfDir, buildShareDir }) = do
  files <- filter ((".conf"==) . takeExtension) <$> getDirectoryContents buildConfDir
  mapM_ (fixPkgRoot pkgRoot shareDir . (buildConfDir </>)) files
  where
    pkgRoot  = T.pack buildPkgRoot
    shareDir = T.pack buildShareDir

fixPkgRoot :: T.Text -> T.Text -> FilePath -> IO ()
fixPkgRoot pkgRoot shareDir fileName = do
  s <- T.readFile fileName
  when (pkgRoot `T.isInfixOf` s) $ do
    putStrLn fileName
    T.writeFile fileName .
      T.replace pkgRoot "${pkgroot}" .
      T.replace shareDir (T.append pkgRoot "/../../share") $
      s

recachePkg :: BuildState -> IO ()
recachePkg bs =
  callProcess (buildBinDir bs </> "ghc-pkg") ["recache"]

ensureDir :: FilePath -> Rule
ensureDir s = defRule
  { ruleName = "ensureDir " ++ show s
  , ruleRun  = createDirectoryIfMissing True s
  }

withDir :: FilePath -> IO a -> IO a
withDir s =
  C.bracket saveDir setCurrentDirectory . const
  where
    saveDir = do
      dir <- getCurrentDirectory
      setCurrentDirectory s
      return dir

andM :: Monad m => [m Bool] -> m Bool
andM = foldr go (return True)
  where go m acc = m >>= \x -> if x then acc else return x

sha1 :: FilePath -> IO String
sha1 fileName = do
  output <- readProcess "openssl" ["sha1", fileName] ""
  case map (reverse . words) . lines $ output of
    ((hash:_):_) -> return hash
    _            -> return ""

downloadRelease :: (BuildState -> Release) -> BuildState -> Rule
downloadRelease getRel bs@(BuildState { buildDownloadDir }) = defRule
  { ruleName         = "download " ++ releaseFileName rel
  , ruleCheck        = checkDownload
  , ruleDependencies = [ ensureDir buildDownloadDir ]
  , ruleRun          = do
    callProcess "curl" [ "-L", "-s", "-o", tarFileName, releaseUrl rel ]
    didFail <- not <$> checkDownload
    when didFail . fail $ "release tarball did not match expected size or sha1"
  }
  where
    tarFileName = buildDownloadDir </> releaseFileName rel
    rel = getRel bs
    checkDownload = andM
      [ doesFileExist tarFileName
      , (releaseSize rel ==) . fromIntegral . fileSize <$> getFileStatus tarFileName
      , (releaseSha1 rel ==) <$> sha1 tarFileName
      ]

unpackRelease :: (BuildState -> Release)
              -> FilePath
              -> BuildState -> Rule
unpackRelease getRel unpackDest bs@(BuildState
    { buildUnpackDir, buildDownloadDir }) = defRule
  { ruleName         = "unpack " ++ releaseFileName rel
  , ruleCheck        = doesDirectoryExist unpackDest
  , ruleDependencies = [ downloadRelease getRel bs
                       , ensureDir buildUnpackDir ]
  , ruleRun          = withDir buildUnpackDir $
      callProcess "tar" [ "xf", tarFileName ]
  }
  where
    rel = getRel bs
    tarFileName = buildDownloadDir </> releaseFileName rel

buildRelease :: BuildState -> Rule
buildRelease bs@(BuildState
    { buildPrefixDir, buildGhcName, buildBinDir, buildUnpackDest }) = defRule
  { ruleName         = "build " ++ buildGhcName
  , ruleCheck        = doesFileExist (buildBinDir </> "ghc")
  , ruleDependencies = [ unpackRelease buildRel buildUnpackDest bs
                       , ensureDir buildPrefixDir ]
  , ruleRun          = withDir buildUnpackDest $ do
      callProcess "./configure" [ "--prefix=" ++ buildPrefixDir ]
      callProcess "make" ["install"]
      mapM_ ($ bs)
        [ fixupBin
        , fixupConf
        , recachePkg
        ]
  }

installCabal :: BuildState -> Rule
installCabal bs@(BuildState { buildUnpackDir, buildBinDir }) = defRule
  { ruleName         = "install cabal " ++ releaseVersion (buildCabalRel bs)
  , ruleCheck        = doesFileExist cabalDest
  , ruleDependencies = [ unpackRelease buildCabalRel cabalSrc bs ]
  , ruleRun          = copyFile cabalSrc cabalDest
  }
  where
    cabalSrc  = buildUnpackDir </> "bin" </> "cabal"
    cabalDest = buildBinDir </> "cabal"

installStack :: BuildState -> Rule
installStack bs@(BuildState { buildUnpackDir, buildBinDir }) = defRule
  { ruleName         = "install stack " ++ releaseVersion (buildStackRel bs)
  , ruleCheck        = doesFileExist stackDest
  , ruleDependencies = [ unpackRelease buildStackRel stackSrc bs ]
  , ruleRun          = copyFile stackSrc stackDest
  }
  where
    stackSrc  = buildUnpackDir </> "stack-" ++ releaseVersion (buildStackRel bs) ++ "-osx-x86_64" </> "stack"
    stackDest = buildBinDir </> "stack"

buildApp :: BuildState -> Rule
buildApp bs = defRule
  { ruleName         = "building " ++ buildAppDir bs
  , ruleDependencies = map ($ bs) [ buildRelease
                                  , installCabal
                                  , installStack
                                  , sanityCheck ]
  }

die :: [String] -> IO a
die errs = do
  hPutStrLn stderr (concat errs ++ usageInfo header options)
  exitFailure
  where header = "Usage: runhaskell Main.hs [OPTION...]"

parseReleases :: Releases -> [String] -> IO Releases
parseReleases releases argv = case getOpt Permute options argv of
  (opts, [], []) -> foldM (flip id) releases opts
  (_, _, errs)   -> die (if showHelp then [] else errs)
  where
    showHelp = "--help" `elem` argv

options :: [OptDescr (Releases -> IO Releases)]
options = do
  (prodName, updateProd) <-
    [ ("ghc", \f x -> do
        release <- f (releasesGhc x)
        return x { releasesGhc = release })
    , ("cabal", \f x -> do
        release <- f (releasesCabal x)
        return x { releasesCabal = release })
    , ("stack", \f x -> do
        release <- f (releasesStack x)
        return x { releasesStack = release })
    ]
  (setterName, setter) <-
    [ ("url", \arg r -> return r { releaseUrl = arg })
    , ("version", \arg r -> return r { releaseVersion = arg })
    , ("sha1", \arg r -> return r { releaseSha1 = arg })
    , ("size", \arg r -> case reads arg of
        [(size, "")] -> return r { releaseSize = size }
        _            ->
          die [ "option --" ++ prodName ++
                "-size requires a number argument SIZE\n"])
    ]
  return $ Option [] [prodName ++ "-" ++ setterName]
             (ReqArg (updateProd . setter) (map toUpper setterName))
             (prodName ++ " " ++ setterName)

main :: IO ()
main = do
  -- this is necessary to allow `stack exec main` to do the right thing
  -- without --no-ghc-package-path
  unsetEnv "GHC_PACKAGE_PATH"
  releases <- getArgs >>= parseReleases latestReleases
  appRule <- buildApp . buildState releases <$> getCurrentDirectory
  runRule appRule
