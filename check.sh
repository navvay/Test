#!/bin/bash

# Function to extract the latest version of a package from the Swift Package Manager dump
function get_latest_version {
    package_name=$1
    package_dump=$2
    latest_version=$(echo "$package_dump" | grep -A1 "\"name\": \"$package_name\"" | grep "\"version\"" | awk -F'"' '{print $4}')
    echo "$latest_version"
}

# Extract and check the latest version for each dependency
check_dependency() {
    package_name=$1
    package_url=$2
    package_requirement=$3

    echo "Checking $package_name..."

    # Clone the repository to get the latest tags
    git clone --depth=1 $package_url temp_repository
    cd temp_repository

    # Get the latest version from the package dump
    package_dump=$(swift package dump-package)
    latest_version=$(get_latest_version "$package_name" "$package_dump")

    # Print the latest version
    echo "Latest version of $package_name: $latest_version"

    cd ..
    rm -rf temp_repository
}

# Read the Package.swift file and extract dependencies
dependencies=$(awk '/\.package\(url:/{flag=1;next}/\),/{flag=0}flag' Package.swift)

while IFS= read -r dependency; do
    package_name=$(echo "$dependency" | grep -oP '(?<=name: ")[^"]+')
    package_url=$(echo "$dependency" | grep -oP '(?<=url: ")[^"]+')
    package_requirement=$(echo "$dependency" | grep -oP '(?<=from: ")[^"]+')

    check_dependency "$package_name" "$package_url" "$package_requirement"
done <<< "$dependencies"
