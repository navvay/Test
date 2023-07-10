#!/bin/bash

# Check if the input .cabal file is provided
if [ -z "$1" ]; then
  echo "Usage: ./dependency_check.sh <CABAL_FILE>"
  exit 1
fi

# Haskell script to extract and check dependencies
HASKELL_SCRIPT=$(cat <<'EOT'
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import           Distribution.PackageDescription.Parsec
import           Distribution.PackageDescription.Configuration
import           Distribution.PackageDescription
import           Distribution.Types.PackageName
import           Distribution.Types.VersionRange
import           Distribution.Version
import           Distribution.Text
import           Hackage.Security.Client
import           Hackage.Security.Client.Repository.Cache
import           Hackage.Security.Util.Path
import           System.Environment
import           System.Directory
import           System.Exit
import           System.FilePath

-- | Extracts dependencies from the given .cabal file
extractDependencies :: FilePath -> IO [Dependency]
extractDependencies cabalFile = do
  contents <- B.readFile cabalFile

  case parseGenericPackageDescriptionMaybe contents of
    ParseFailed err -> do
      putStrLn $ "Failed to parse " ++ cabalFile ++ ": " ++ show err
      exitFailure
    ParseOk _ gpd -> do
      let flags = []
          mlib = condLibrary gpd
          exes = condExecutables gpd

      let combinedDeps = allBuildDepends mlib ++ concatMap (allBuildDepends . snd) exes
          dependencies = nub $ map (\(Dependency name _) -> name) combinedDeps

      return dependencies

-- | Retrieves the latest version of a package from Hackage
getLatestVersion :: PackageName -> IO (Maybe Version)
getLatestVersion packageName = do
  let repoPath = defaultHackageRepoPath
      repoContext = RepoContext
        { repoContextLayout = topLevelLayout (Filesystem repoPath)
        , repoContextHttpLib = undefined -- or use http-client
        }

  repoIndex <- cachedIndex repoContext

  let pkgIndex = packageIndex repoIndex
  return $ lookupLatestVersion packageName pkgIndex

-- | Generates a text file with outdated dependencies
generateOutdatedDepsFile :: FilePath -> [PackageName] -> IO ()
generateOutdatedDepsFile outFile outdatedDeps = do
  let content = unlines $ map (\pkgName -> show pkgName ++ ": <latest version>") outdatedDeps
  writeFile outFile content

-- | Main function
main :: IO ()
main = do
  cabalFile <- head <$> getArgs
  outdatedDepsFile <- (++ ".outdated") <$> getCurrentDirectory

  dependencies <- extractDependencies cabalFile
  latestVersions <- mapM getLatestVersion dependencies

  let outdatedDeps = [pkgName | (pkgName, Just latestVer) <- zip dependencies latestVersions, latestVer > packageVersion pkgName]
  
  if null outdatedDeps
    then putStrLn "All dependencies are up to date."
    else do
      putStrLn "The following dependencies are outdated:"
      mapM_ (\dep -> putStrLn $ show dep ++ ": <latest version>") outdatedDeps
      generateOutdatedDepsFile outdatedDepsFile outdatedDeps
      putStrLn $ "Outdated dependencies written to: " ++ outdatedDepsFile
EOT
)

# Create a temporary Haskell file
TMP_FILE=$(mktemp)
echo "$HASKELL_SCRIPT" > "$TMP_FILE"

# Run the Haskell script with the provided .cabal file as an argument
cabalFile=$(realpath "$1")
runhaskell "$TMP_FILE" "$cabalFile"

# Clean up temporary Haskell file
rm "$TMP_FILE"
