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
import           Distribution.PackageDescription.Parsec.ParseResult (runParseResult)
import           Distribution.PackageDescription.Parse (parseGenericPackageDescription)
import           Distribution.PackageDescription
import           Distribution.Types.PackageName
import           Distribution.Types.VersionRange
import           Distribution.Version
import           Distribution.Text
import           Data.List (nub)
import           System.Process

-- | Extracts dependencies from the given .cabal file
extractDependencies :: FilePath -> IO [Dependency]
extractDependencies cabalFile = do
  contents <- B.readFile cabalFile

  let gpdResult = runParseResult $ parseGenericPackageDescription contents
  case gpdResult of
    (_, Left err) -> do
      putStrLn $ "Failed to parse " ++ cabalFile ++ ": " ++ show err
      exitFailure
    (_, Right gpd) -> do
      let flags = []
          mlib = condLibrary gpd
          exes = condExecutables gpd

      let combinedDeps = allBuildDepends mlib ++ concatMap (allBuildDepends . snd) exes
          dependencies = nub $ map (\(Dependency name _) -> name) combinedDeps

      return dependencies

-- | Retrieves the latest version of a package from Hackage
getLatestVersion :: PackageName -> IO (Maybe Version)
getLatestVersion packageName = do
  let url = "https://hackage.haskell.org/package/" ++ packageNameStr ++ "/preferred"
      packageNameStr = unPackageName packageName

  versionStr <- readProcess "curl" [url] []
  
  case simpleParse versionStr of
    Just version -> return $ Just version
    Nothing -> return Nothing

-- | Main function
main :: IO ()
main = do
  cabalFile <- head <$> getArgs

  dependencies <- extractDependencies cabalFile
  latestVersions <- mapM getLatestVersion dependencies

  let outdatedDeps = [pkgName | (pkgName, Just latestVer) <- zip dependencies latestVersions, latestVer > packageVersion pkgName]
  
  if null outdatedDeps
    then putStrLn "All dependencies are up to date."
    else do
      putStrLn "The following dependencies are outdated:"
      mapM_ (\dep -> putStrLn $ show dep ++ ": <latest version>") outdatedDeps
      putStrLn "::set-output name=dependencies::$(echo \"$outdatedDeps\" | tr '\n' ',' | sed 's/,$//')"
EOT
)

# Create a temporary Haskell file
TMP_FILE=$(mktemp)
echo "$HASKELL_SCRIPT" > "$TMP_FILE"

# Run the Haskell script with the provided .cabal file as an argument
cabalFile=$(realpath "$1")
output=$(runhaskell "$TMP_FILE" "$cabalFile")
dependencies=$(echo "$output" | awk '/::set-output name=dependencies::/{print $2}')

# Print the dependencies
echo "Outdated Dependencies: $dependencies"

# Clean up temporary Haskell file
rm "$TMP_FILE"
