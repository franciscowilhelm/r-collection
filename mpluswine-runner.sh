#!/bin/bash

# Path to your .exe (in Windows format)
EXE_PATH="C:\\Program Files\\Mplus\\Mplus.exe"

MAC_FILE="$1"
if [ -z "$MAC_FILE" ]; then
  echo "Usage: $0 /path/to/file"
  exit 1
fi

# Convert macOS path to Windows path for Wine
WIN_FILE=$(echo "$MAC_FILE" | sed 's#/#\\#g' | sed 's#^\\#Z:\\#')

# Get folder of input file
MAC_DIR=$(dirname "$MAC_FILE")

# Change to the input file's directory, then run Wine
cd "$MAC_DIR" || exit 1
wine "$EXE_PATH" "$WIN_FILE"
    