{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Network.HTTP.Conduit
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.Process
import Text.Show.Pretty
import Text.Taggy

-- TODO: configuration file
main :: IO ()
main = do
  let workspace = "/home/maczniak/aoa/projects/libpopular/work"
  cwd <- getCurrentDirectory
  workspaceCreated <- prepareWorkspace workspace
  goWorkspace workspace
  progName <- getProgName
  repoURLs <- getArgs
  if length repoURLs == 0
    then die ("USAGE: " ++ progName ++ " repositoryURL ...") else return ()
  res <- browseRepo workspace (repoURLs !! 0)
  return ()

-- TODO: undo parent directories
prepareWorkspace :: FilePath -> IO Bool
prepareWorkspace dir = do
  exist <- doesDirectoryExist dir
  case exist of
       False     -> createDirectoryIfMissing True dir
       otherwise -> return ()
  return exist

goWorkspace :: FilePath -> IO ()
goWorkspace dir = do
  setCurrentDirectory dir

-- TODO: undo
--browseRepo :: FilePath -> String -> IO ()
browseRepo dir repoURL = do
  let basename = takeBaseName repoURL
  exist <- doesDirectoryExist basename
  case exist of
       False     -> callProcess "git" ["clone", repoURL]
       otherwise -> return ()
  setCurrentDirectory basename
  packages <- cabalDependPackages $ basename ++ ".cabal"
  (pkgModMap, modURLMap) <- moduleList packages
  pPrint pkgModMap
  pPrint modURLMap
  return (pkgModMap, modURLMap)

cabalDependPackages :: FilePath -> IO [String]
cabalDependPackages cabalFile =
  readPackageDescription verbose cabalFile >>= (return . packageList)

packageList :: GenericPackageDescription -> [String]
packageList pkgDesc =
  delete myOwn $
    foldr union [] $
      libraryPkgs ++ executablePkgs ++ testSuitePkgs ++ benchmarkPkgs
  where myOwn = unPackageName $ pkgName $ package $ packageDescription pkgDesc
        libraryPkgs = map (readBuildInfo . libBuildInfo . condTreeData) $
                          maybeToList (condLibrary pkgDesc)
        executablePkgs = map (get buildInfo) $ condExecutables pkgDesc
        testSuitePkgs  = map (get testBuildInfo) $ condTestSuites pkgDesc
        benchmarkPkgs  = map (get benchmarkBuildInfo) $ condBenchmarks pkgDesc
        get f = readBuildInfo . f . condTreeData . snd
        readBuildInfo bi = [ unPackageName packageName |
          Dependency packageName _ <- targetBuildDepends bi ]

moduleList :: [String] -> IO (M.Map String (S.Set String), M.Map String String)
moduleList pkgs = do
  maps <- mapM moduleInPackage pkgs
  return (M.unionsWith S.union $ map fst maps, M.unions $ map snd maps)

moduleInPackage :: String
                   -> IO (M.Map String (S.Set String), M.Map String String)
moduleInPackage pkg = do
  body <- simpleHttp ("https://hackage.haskell.org/package/" ++ pkg)
  let modBody = between "<div id=\"modules\">"
                        "<p style=\"font-size: small\">"
                        "<div id=\"downloads\">"
                        (toText body)
  let tags = findModLinks $ taggyWith True modBody
  let pkgModMap = M.fromListWith S.union
                    [(pkg, S.singleton $ toString x) | (x, _) <- tags]
  let modURLMap = M.fromList [(toString k, toString v) | (k, v) <- tags]
  return (pkgModMap, modURLMap)
  where between from until1 until2 = fst . LT.breakOn until2
                                   . fst . LT.breakOn until1
                                   . snd . LT.breakOn from
        findModLinks (TagOpen "a" attrs _:TagText modName:TagClose "a":xs) =
          (modName, findAttr "href" attrs) : findModLinks xs
        findModLinks (x:xs) = findModLinks xs
        findModLinks [] = []

toText = LT.pack . LC.unpack
toString = T.unpack -- . LT.toStrict

findAttr attrName (x:xs) = if attrKey x == attrName then attrValue x
                                                    else findAttr attrName xs
findAttr attrName [] = ""

