#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "requests>=2.31.0",
# ]
# ///
"""
Fix Lidarr naming configuration to remove duplicate 'library' folder.

Changes artistFolderFormat from 'library/{Artist Name}' to '{Artist Name}'.
"""

import os
import sys
import requests

# Get API key from environment
api_key = os.environ.get("LIDARR_API_KEY")
if not api_key:
    print("Error: LIDARR_API_KEY environment variable not set")
    sys.exit(1)

base_url = "https://lidarr.sbr.pm"
headers = {"X-Api-Key": api_key, "Content-Type": "application/json"}

print("Fetching current naming configuration...")
try:
    response = requests.get(f"{base_url}/api/v1/config/naming", headers=headers)
    response.raise_for_status()
    config = response.json()
except Exception as e:
    print(f"Error fetching config: {e}")
    sys.exit(1)

print(f"\nCurrent Artist Folder Format: {config.get('artistFolderFormat')}")

if config.get('artistFolderFormat') == 'library/{Artist Name}':
    print("\nChanging to: {Artist Name}")

    config['artistFolderFormat'] = '{Artist Name}'

    try:
        response = requests.put(f"{base_url}/api/v1/config/naming/{config['id']}",
                               headers=headers, json=config)
        response.raise_for_status()
        print("✓ Successfully updated naming configuration!")
        print("\nNew Artist Folder Format: {Artist Name}")
        print("\nNote: This only affects NEW artists added to Lidarr.")
        print("Existing artists with duplicate paths need to be fixed with:")
        print("  arr lidarr fix-duplicate-paths https://lidarr.sbr.pm -k $LIDARR_API_KEY")
    except Exception as e:
        print(f"Error updating config: {e}")
        sys.exit(1)
elif config.get('artistFolderFormat') == '{Artist Name}':
    print("✓ Already configured correctly!")
else:
    print(f"\nUnexpected format: {config.get('artistFolderFormat')}")
    print("Please manually review this setting.")
