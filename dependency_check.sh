#!/bin/bash

# Check if the input .cabal file is provided
if [ -z "$1" ]; then
  echo "Usage: ./dependency_check.sh <CABAL_FILE>"
  exit 1
fi

# Haskell script to check dependencies
HASKELL_SCRIPT=$(cat <<'EOT'
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forM)
import qualified Data.ByteString.Char8 as B
import           Data.List (nub)
import           Distribution.PackageDescription.Parsec
import           Distribution.PackageDescription.Configuration
import           Distribution.PackageDescription.Parse (parseGenericPackageDescription)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import           Distribution.Types.PackageName
import           Distribution.Types.VersionRange
import           Distribution.Version
import           System.Environment
import           System.Process

-- | Extracts dependencies from the given .cabal file using cabal tool
extractDependencies :: FilePath -> IO [Dependency]
extractDependencies cabalFile = do
  output <- readProcess "cabal" ["v2-build", "--dry-run", "--dependencies", "--enable-tests", "--enable-benchmarks"] cabalFile
  let dependencies = extractPackageName . words $ output
  return $ nub dependencies

-- | Extracts the package names from the output
extractPackageName :: [String] -> [Dependency]
extractPackageName [] = []
extractPackageName (_:"dependent":pkg:rest) = Dependency (mkPackageName pkg) anyVersion : extractPackageName rest
extractPackageName (_:xs) = extractPackageName xs

-- | Retrieves the latest version of a package from Hackage
getLatestVersion :: PackageName -> IO (Maybe Version)
getLatestVersion packageName = do
  let url = "https://hackage.haskell.org/package/" ++ packageNameStr ++ "/preferred"
      packageNameStr = unPackageName packageName

  versionStr <- readProcess "curl" [url] []
  
  case simpleParseVersion versionStr of
    Just version -> return $ Just version
    Nothing -> return Nothing

-- | Main function
main :: IO ()
main = do
  cabalFile <- head <$> getArgs

  dependencies <- extractDependencies cabalFile
  latestVersions <- mapM getLatestVersion dependencies

  let outdatedDeps = [pkgName | (pkgName, Just latestVer) <- zip dependencies latestVersions, latestVer > pkgVersion pkgName]
  
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
