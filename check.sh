#!/bin/bash

# Parse Package.swift
PACKAGE_FILE="Package.swift"
DEPENDENCIES=$(sed -n '/dependencies:/,/]/p' $PACKAGE_FILE | awk -F'"' '/url:/{print $2 ":" $4}')

# Check for the latest version of each dependency
OUTDATED_VERSIONS=""
for DEPENDENCY in $DEPENDENCIES; do
    # Extract dependency name and version
    DEP_NAME=$(echo $DEPENDENCY | cut -d: -f1)
    DEP_VERSION=$(echo $DEPENDENCY | cut -d: -f2)
    
    # Extract the repository owner and name from the GitHub URL
    REPO_OWNER=$(echo $DEP_NAME | cut -d/ -f4)
    REPO_NAME=$(echo $DEP_NAME | cut -d/ -f5)

    # Retrieve the latest version from the GitHub API
    LATEST_VERSION=$(curl -s "https://api.github.com/repos/$REPO_OWNER/$REPO_NAME/releases/latest" | jq -r '.tag_name')

    if [ "$LATEST_VERSION" != "null" ] && [ "$LATEST_VERSION" != "$DEP_VERSION" ]; then
        OUTDATED_VERSIONS+="\n$DEP_NAME: $DEP_VERSION -> $LATEST_VERSION"
    fi
done

if [ -n "$OUTDATED_VERSIONS" ]; then
    echo -e "Outdated versions:$OUTDATED_VERSIONS"
else
    echo "All dependencies are up to date!"
fi
