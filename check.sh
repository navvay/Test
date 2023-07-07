#!/bin/bash

# Parse Package.swift
PACKAGE_FILE="Package.swift"
DEPENDENCIES=$(sed -n '/dependencies:/,/]/p' $PACKAGE_FILE | awk -F'"' '/url:/{print $2 ":" $4}')

# Check for the latest version of each dependency
MISSING_VERSIONS="missing_versions.txt"
> $MISSING_VERSIONS

OUTDATED_VERSIONS="outdated_versions.txt"
> $OUTDATED_VERSIONS

for DEPENDENCY in $DEPENDENCIES; do
    # Extract dependency name and version
    DEP_NAME=$(echo $DEPENDENCY | cut -d: -f1)
    DEP_VERSION=$(echo $DEPENDENCY | cut -d: -f2)

    # Retrieve the latest version from Swift Package Index API
    LATEST_VERSION=$(curl -s "https://api.swiftpackageindex.com/package/$DEP_NAME" | jq -r '.versions | keys | .[-1]')

    if [ "$LATEST_VERSION" != "$DEP_VERSION" ]; then
        echo "$DEP_NAME: $LATEST_VERSION" >> $MISSING_VERSIONS
        echo "$DEP_NAME: $DEP_VERSION -> $LATEST_VERSION" >> $OUTDATED_VERSIONS
    fi
done

if [ -s $MISSING_VERSIONS ]; then
    echo "Missing versions written to $MISSING_VERSIONS"
    cat $OUTDATED_VERSIONS
else
    echo "All dependencies are up to date!"
fi
