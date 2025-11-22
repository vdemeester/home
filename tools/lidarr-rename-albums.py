#!/usr/bin/env -S uv run --quiet --script
# /// script
# dependencies = [
#   "requests",
# ]
# ///
"""
Rename albums in Lidarr with interactive confirmation.

This script:
1. Fetches all artists from Lidarr API
2. Checks which artists have albums with files that need renaming
3. Previews the rename changes for each album
4. Asks for confirmation before applying renames

Usage:
    ./lidarr-rename-albums.py <lidarr_url> <api_key>

Example:
    ./lidarr-rename-albums.py http://localhost:8686 your-api-key
"""

import argparse
import sys
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


def get_artist_albums(
    base_url: str, api_key: str, artist_id: int
) -> List[Dict[str, Any]]:
    """Fetch all albums for a specific artist."""
    url = f"{base_url}/api/v1/album"
    headers = {"X-Api-Key": api_key}
    params = {"artistId": artist_id}

    try:
        response = requests.get(url, headers=headers, params=params)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        print(
            f"Error fetching albums for artist {artist_id}: {e}",
            file=sys.stderr,
        )
        return []


def get_rename_preview(
    base_url: str, api_key: str, artist_id: int
) -> List[Dict[str, Any]]:
    """Get preview of files that will be renamed for an artist."""
    url = f"{base_url}/api/v1/rename"
    headers = {"X-Api-Key": api_key}
    params = {"artistId": artist_id}

    try:
        response = requests.get(url, headers=headers, params=params)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        print(
            f"Error fetching rename preview for artist {artist_id}: {e}",
            file=sys.stderr,
        )
        return []


def execute_rename(
    base_url: str, api_key: str, artist_id: int, file_ids: List[int]
) -> Dict[str, Any]:
    """Execute rename operation for an artist."""
    url = f"{base_url}/api/v1/command"
    headers = {"X-Api-Key": api_key, "Content-Type": "application/json"}
    payload = {
        "name": "RenameFiles",
        "artistId": artist_id,
        "files": file_ids,
    }

    try:
        response = requests.post(url, headers=headers, json=payload)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        print(
            f"Error executing rename for artist {artist_id}: {e}",
            file=sys.stderr,
        )
        return {}


def ask_confirmation(prompt: str) -> bool:
    """Ask user for yes/no confirmation."""
    while True:
        response = input(f"{prompt} (y/n): ").lower().strip()
        if response in ["y", "yes"]:
            return True
        elif response in ["n", "no"]:
            return False
        else:
            print("Please answer 'y' or 'n'")


def main():
    parser = argparse.ArgumentParser(
        description="Rename Lidarr albums with confirmation"
    )
    parser.add_argument(
        "lidarr_url", help="Lidarr base URL (e.g., http://localhost:8686)"
    )
    parser.add_argument("api_key", help="Lidarr API key")
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be renamed without making changes",
    )
    parser.add_argument(
        "--no-confirm",
        "--yolo",
        action="store_true",
        dest="no_confirm",
        help="Skip interactive confirmation (use with caution)",
    )

    args = parser.parse_args()

    # Normalize URLs
    base_url = args.lidarr_url.rstrip("/")

    print(f"Fetching artists from {base_url}...")
    all_artists = get_all_artists(base_url, args.api_key)
    print(f"Found {len(all_artists)} artists\n")

    artists_with_renames = []
    artists_without_renames = []

    # Check each artist for rename candidates
    print("Checking which artists have albums needing renaming...")
    for artist in all_artists:
        artist_id = artist.get("id")
        artist_name = artist.get("artistName", "Unknown")

        rename_preview = get_rename_preview(
            base_url, args.api_key, artist_id
        )

        if rename_preview:
            # Get album count for this artist
            albums = get_artist_albums(base_url, args.api_key, artist_id)
            album_count = len(albums) if albums else 0

            artists_with_renames.append(
                (artist_id, artist_name, album_count, rename_preview)
            )
        else:
            artists_without_renames.append(artist_name)

    # Print summary
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)

    if artists_without_renames:
        count = len(artists_without_renames)
        print(f"\n✓ No renames needed ({count} artists):")
        for name in artists_without_renames[:5]:  # Show first 5
            print(f"  - {name}")
        if len(artists_without_renames) > 5:
            print(f"  ... and {len(artists_without_renames) - 5} more")

    if artists_with_renames:
        count = len(artists_with_renames)
        print(f"\n→ Artists with renames needed: {count}")

    if not artists_with_renames:
        print("\nNo artists need renaming!")
        return

    # Process each artist that needs renaming
    print("\n" + "=" * 80)
    print("RENAME PREVIEW")
    print("=" * 80)

    renamed_count = 0
    skipped_count = 0

    for artist_id, artist_name, album_count, rename_preview in (
        artists_with_renames
    ):
        print(f"\n{'=' * 80}")
        print(f"Artist: {artist_name}")
        print(f"Albums: {album_count}")
        print(f"Files to rename: {len(rename_preview)}")
        print("=" * 80)

        # Show preview of renames (limit to first 10)
        display_limit = 10
        for i, item in enumerate(rename_preview[:display_limit]):
            existing_path = item.get("existingPath", "Unknown")
            new_path = item.get("newPath", "Unknown")
            print(f"\n  File {i + 1}:")
            print(f"    FROM: {existing_path}")
            print(f"    TO:   {new_path}")

        if len(rename_preview) > display_limit:
            remaining = len(rename_preview) - display_limit
            print(f"\n  ... and {remaining} more files")

        # Ask for confirmation (unless in dry-run or no-confirm mode)
        should_rename = False

        if args.dry_run:
            print("\n[DRY RUN] Skipping actual rename")
        elif args.no_confirm:
            should_rename = True
            print("\n[NO CONFIRM] Proceeding with rename...")
        else:
            file_word = "file" if len(rename_preview) == 1 else "files"
            prompt = (
                f"\nRename {len(rename_preview)} {file_word} "
                f"for '{artist_name}'?"
            )
            should_rename = ask_confirmation(prompt)

        if should_rename and not args.dry_run:
            print("Executing rename...")
            # Extract track file IDs from preview
            file_ids = [item.get("trackFileId") for item in rename_preview]
            file_ids = [fid for fid in file_ids if fid is not None]

            if file_ids:
                result = execute_rename(
                    base_url, args.api_key, artist_id, file_ids
                )
                if result:
                    print("✓ Rename command queued successfully")
                    renamed_count += 1
                else:
                    print("✗ Failed to queue rename command")
                    skipped_count += 1
            else:
                print("✗ No valid file IDs found")
                skipped_count += 1
        else:
            if not args.dry_run:
                print("Skipped")
            skipped_count += 1

    # Final summary
    print("\n" + "=" * 80)
    print("FINAL SUMMARY")
    print("=" * 80)

    if args.dry_run:
        print(
            f"\n[DRY RUN] Found {len(artists_with_renames)} artists "
            "that need renaming"
        )
        print("No changes were made. Remove --dry-run to apply renames.")
    else:
        print(f"\nArtists processed: {len(artists_with_renames)}")
        print(f"  - Renamed: {renamed_count}")
        print(f"  - Skipped: {skipped_count}")

        if renamed_count > 0:
            print(
                "\nNote: Rename operations are queued. "
                "Check Lidarr's queue for progress."
            )


if __name__ == "__main__":
    main()
