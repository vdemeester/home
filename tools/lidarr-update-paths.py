#!/usr/bin/env -S uv run --quiet --script
# /// script
# dependencies = [
#   "requests",
# ]
# ///
"""
Update artist paths in Lidarr to use a 'library' subdirectory.

This script:
1. Fetches all artists from Lidarr API
2. Checks if their path is directly in the music folder or already
   contains 'library'
3. Updates paths that need to be moved to
   <music_folder>/library/<artist>

Usage:
    ./lidarr-update-paths.py <lidarr_url> <api_key> <music_folder>

Example:
    ./lidarr-update-paths.py http://localhost:8686 your-api-key /data/music
"""

import argparse
import sys
from pathlib import Path
from typing import Any, Dict, List

import requests


def get_all_artists(base_url: str, api_key: str) -> List[Dict[str, Any]]:
    """Fetch all artists from Lidarr API."""
    url = f"{base_url}/api/v1/artist"
    headers = {"X-Api-Key": api_key}

    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        print(f"Error fetching artists: {e}", file=sys.stderr)
        sys.exit(1)


def update_artist_path(
    base_url: str, api_key: str, artist_id: int, new_path: str
) -> bool:
    """Update an artist's path via Lidarr API."""
    url = f"{base_url}/api/v1/artist/{artist_id}"
    headers = {"X-Api-Key": api_key, "Content-Type": "application/json"}

    # First, get the current artist data
    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        artist_data = response.json()
    except requests.exceptions.RequestException as e:
        print(f"Error fetching artist {artist_id}: {e}", file=sys.stderr)
        return False

    # Update the path
    artist_data["path"] = new_path

    # Send the update
    try:
        response = requests.put(url, headers=headers, json=artist_data)
        response.raise_for_status()
        return True
    except requests.exceptions.RequestException as e:
        print(f"Error updating artist {artist_id}: {e}", file=sys.stderr)
        return False


def main():
    parser = argparse.ArgumentParser(
        description="Update Lidarr artist paths to use library subdirectory"
    )
    parser.add_argument(
        "lidarr_url", help="Lidarr base URL (e.g., http://localhost:8686)"
    )
    parser.add_argument("api_key", help="Lidarr API key")
    parser.add_argument("music_folder", help="Base music folder path")
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be updated without making changes",
    )

    args = parser.parse_args()

    # Normalize URLs and paths
    base_url = args.lidarr_url.rstrip("/")
    music_folder = Path(args.music_folder).resolve()
    library_folder = music_folder / "library"

    print(f"Fetching artists from {base_url}...")
    artists = get_all_artists(base_url, args.api_key)
    print(f"Found {len(artists)} artists\n")

    needs_update = []
    already_in_library = []
    unknown_location = []

    # Analyze all artists
    for artist in artists:
        artist_name = artist.get("artistName", "Unknown")
        current_path = Path(artist.get("path", ""))
        artist_id = artist.get("id")

        # Check if path is directly in music_folder
        if current_path.parent == music_folder:
            new_path = library_folder / current_path.name
            needs_update.append(
                (artist_id, artist_name, current_path, new_path)
            )
        # Check if already in library subfolder
        elif "library" in current_path.parts:
            already_in_library.append((artist_name, current_path))
        else:
            unknown_location.append((artist_name, current_path))

    # Print summary
    print("=" * 80)
    print("SUMMARY")
    print("=" * 80)

    if already_in_library:
        msg = f"\n✓ Already in library folder ({len(already_in_library)} "
        msg += "artists):"
        print(msg)
        for name, path in already_in_library[:5]:  # Show first 5
            print(f"  - {name}: {path}")
        if len(already_in_library) > 5:
            print(f"  ... and {len(already_in_library) - 5} more")

    if needs_update:
        print(f"\n→ Needs update ({len(needs_update)} artists):")
        for artist_id, name, old_path, new_path in needs_update:
            print(f"  - {name}:")
            print(f"      FROM: {old_path}")
            print(f"      TO:   {new_path}")

    if unknown_location:
        print(f"\n⚠ Unknown location ({len(unknown_location)} artists):")
        for name, path in unknown_location[:5]:  # Show first 5
            print(f"  - {name}: {path}")
        if len(unknown_location) > 5:
            print(f"  ... and {len(unknown_location) - 5} more")

    # Perform updates
    if needs_update and not args.dry_run:
        print(f"\n{'=' * 80}")
        print("UPDATING PATHS")
        print("=" * 80)

        success_count = 0
        fail_count = 0

        for artist_id, name, old_path, new_path in needs_update:
            print(f"\nUpdating {name}...", end=" ")
            success = update_artist_path(
                base_url, args.api_key, artist_id, str(new_path)
            )
            if success:
                print("✓ SUCCESS")
                success_count += 1
            else:
                print("✗ FAILED")
                fail_count += 1

        print(f"\n{'=' * 80}")
        print(f"Results: {success_count} updated, {fail_count} failed")
        print("=" * 80)
    elif needs_update and args.dry_run:
        print(
            "\n[DRY RUN] No changes were made. "
            "Remove --dry-run to apply updates."
        )
    else:
        print("\nNo artists need updating!")


if __name__ == "__main__":
    main()
