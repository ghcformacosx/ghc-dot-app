-- #!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

{-

This builds a relocatable ghc-7.8.2.app in dist/build,
including cabal-install 1.20.0.1

TODO:

* Add a sensible Info.plist and related metadata (icon, etc.)
* Create an executable that will help the user add this GHC to their PATH
* Pre-install some packages?

-}
module Main (main) where

import Control.Applicative ((<$>))
import System.Directory
  ( getCurrentDirectory, getDirectoryContents, doesDirectoryExist
  , setCurrentDirectory, createDirectoryIfMissing, doesFileExist
  , copyFile )
import System.FilePath ((</>), takeExtension, takeFileName)
import System.Process (callProcess, readProcess)
import Control.Monad (when, filterM)
import System.Posix.Files
  ( getSymbolicLinkStatus, getFileStatus, isSymbolicLink, fileSize )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Control.Exception as C

-- Layout:
--
--   dist/download/[ghc-7.8.2-*.tar.xz]
--   dist/unpack/ghc-7.8.2
--   dist/build/ghc-[7.8.2].app/Contents/{lib,bin}/
--

data BuildState = BuildState
  { buildRel             :: Release
  , buildCabalRel        :: Release
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

buildState :: Release -> Release -> FilePath -> BuildState
buildState rel cabalRel here = b
  where
    distDir = here </> "dist"
    n = buildGhcName b
    b = BuildState
      { buildRel             = rel
      , buildCabalRel        = cabalRel
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
  { releaseVersion = "7.8.2"
  , releaseUrl     = "http://www.haskell.org/ghc/dist/7.8.2/ghc-7.8.2-x86_64-apple-darwin-mavericks.tar.xz"
  , releaseSha1    = "5219737fb38f882532712047f6af32fc73a91f0f"
  , releaseSize    = 69679460
  }

latestCabal :: Release
latestCabal = Release
  { releaseVersion = "1.20.0.1"
  , releaseUrl     = "http://www.haskell.org/cabal/release/cabal-install-1.20.0.1/cabal-x86_64-apple-darwin-mavericks.tar.gz"
  , releaseSha1    = "393450339916581ebce50a797d23f86b11ae4200"
  , releaseSize    = 2951089
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
recachePkg bs = do
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
  , ruleCheck        = andM
      [ doesFileExist tarFileName
      , (releaseSize rel ==) . fromIntegral . fileSize <$> getFileStatus tarFileName
      , (releaseSha1 rel ==) <$> sha1 tarFileName
      ]
  , ruleDependencies = [ ensureDir buildDownloadDir ]
  , ruleRun          = do
    callProcess "curl" [ "-s", "-o", tarFileName, releaseUrl rel ]
  }
  where
    tarFileName = buildDownloadDir </> releaseFileName rel
    rel = getRel bs

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
    { buildPrefixDir, buildGhcName, buildBinDir
    , buildUnpackDest, buildPkgRoot }) = defRule
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
    cabalSrc  = buildUnpackDir </> "dist" </> "build" </> "cabal" </> "cabal"
    cabalDest = buildBinDir </> "cabal"
   

buildApp :: BuildState -> Rule
buildApp bs = defRule
  { ruleName         = "building " ++ buildAppDir bs
  , ruleDependencies = map ($ bs) [ buildRelease, installCabal ]
  }

main :: IO ()
main =
  getCurrentDirectory >>= runRule . buildApp . buildState latestGhc latestCabal
