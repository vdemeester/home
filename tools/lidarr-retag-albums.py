#!/usr/bin/env -S uv run --quiet --script
# /// script
# dependencies = [
#   "requests",
# ]
# ///
"""
Retag albums in Lidarr with interactive confirmation.

This script:
1. Fetches all artists from Lidarr API
2. Checks which artists have albums with files that need retagging
3. Previews the retag changes for each album
4. Asks for confirmation before applying retags

Usage:
    ./lidarr-retag-albums.py <lidarr_url> <api_key>

Example:
    ./lidarr-retag-albums.py http://localhost:8686 your-api-key
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


def get_retag_preview(
    base_url: str, api_key: str, artist_id: int
) -> List[Dict[str, Any]]:
    """Get preview of files that will be retagged for an artist."""
    url = f"{base_url}/api/v1/retag"
    headers = {"X-Api-Key": api_key}
    params = {"artistId": artist_id}

    try:
        response = requests.get(url, headers=headers, params=params)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        print(
            f"Error fetching retag preview for artist {artist_id}: {e}",
            file=sys.stderr,
        )
        return []


def execute_retag(
    base_url: str, api_key: str, artist_id: int, file_ids: List[int]
) -> Dict[str, Any]:
    """Execute retag operation for an artist."""
    url = f"{base_url}/api/v1/command"
    headers = {"X-Api-Key": api_key, "Content-Type": "application/json"}
    payload = {
        "name": "RetagFiles",
        "artistId": artist_id,
        "files": file_ids,
    }

    try:
        response = requests.post(url, headers=headers, json=payload)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        print(
            f"Error executing retag for artist {artist_id}: {e}",
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


def format_tag_changes(changes: List[Dict[str, Any]]) -> str:
    """Format tag changes for display."""
    lines = []
    for change in changes:
        field = change.get("field", "")
        old_val = change.get("oldValue", "")
        new_val = change.get("newValue", "")
        if old_val != new_val:
            lines.append(f"      {field}: '{old_val}' → '{new_val}'")
    return "\n".join(lines) if lines else "      No changes"


def main():
    parser = argparse.ArgumentParser(
        description="Retag Lidarr albums with confirmation"
    )
    parser.add_argument(
        "lidarr_url", help="Lidarr base URL (e.g., http://localhost:8686)"
    )
    parser.add_argument("api_key", help="Lidarr API key")
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be retagged without making changes",
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

    artists_with_retags = []
    artists_without_retags = []

    # Check each artist for retag candidates
    print("Checking which artists have albums needing retagging...")
    for artist in all_artists:
        artist_id = artist.get("id")
        artist_name = artist.get("artistName", "Unknown")

        retag_preview = get_retag_preview(
            base_url, args.api_key, artist_id
        )

        if retag_preview:
            # Get album count for this artist
            albums = get_artist_albums(base_url, args.api_key, artist_id)
            album_count = len(albums) if albums else 0

            artists_with_retags.append(
                (artist_id, artist_name, album_count, retag_preview)
            )
        else:
            artists_without_retags.append(artist_name)

    # Print summary
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)

    if artists_without_retags:
        count = len(artists_without_retags)
        print(f"\n✓ No retags needed ({count} artists):")
        for name in artists_without_retags[:5]:  # Show first 5
            print(f"  - {name}")
        if len(artists_without_retags) > 5:
            print(f"  ... and {len(artists_without_retags) - 5} more")

    if artists_with_retags:
        count = len(artists_with_retags)
        print(f"\n→ Artists with retags needed: {count}")

    if not artists_with_retags:
        print("\nNo artists need retagging!")
        return

    # Process each artist that needs retagging
    print("\n" + "=" * 80)
    print("RETAG PREVIEW")
    print("=" * 80)

    retagged_count = 0
    skipped_count = 0

    for artist_id, artist_name, album_count, retag_preview in (
        artists_with_retags
    ):
        print(f"\n{'=' * 80}")
        print(f"Artist: {artist_name}")
        print(f"Albums: {album_count}")
        print(f"Files to retag: {len(retag_preview)}")
        print("=" * 80)

        # Show preview of retags (limit to first 5)
        display_limit = 5
        for i, item in enumerate(retag_preview[:display_limit]):
            path = item.get("path", "Unknown")
            changes = item.get("changes", {})
            print(f"\n  File {i + 1}: {path}")
            print(format_tag_changes(changes))

        if len(retag_preview) > display_limit:
            remaining = len(retag_preview) - display_limit
            print(f"\n  ... and {remaining} more files")

        # Ask for confirmation (unless in dry-run or no-confirm mode)
        should_retag = False

        if args.dry_run:
            print("\n[DRY RUN] Skipping actual retag")
        elif args.no_confirm:
            should_retag = True
            print("\n[NO CONFIRM] Proceeding with retag...")
        else:
            file_word = "file" if len(retag_preview) == 1 else "files"
            prompt = (
                f"\nRetag {len(retag_preview)} {file_word} "
                f"for '{artist_name}'?"
            )
            should_retag = ask_confirmation(prompt)

        if should_retag and not args.dry_run:
            print("Executing retag...")
            # Extract track file IDs from preview
            file_ids = [item.get("trackFileId") for item in retag_preview]
            file_ids = [fid for fid in file_ids if fid is not None]

            if file_ids:
                result = execute_retag(
                    base_url, args.api_key, artist_id, file_ids
                )
                if result:
                    print("✓ Retag command queued successfully")
                    retagged_count += 1
                else:
                    print("✗ Failed to queue retag command")
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
            f"\n[DRY RUN] Found {len(artists_with_retags)} artists "
            "that need retagging"
        )
        print("No changes were made. Remove --dry-run to apply retags.")
    else:
        print(f"\nArtists processed: {len(artists_with_retags)}")
        print(f"  - Retagged: {retagged_count}")
        print(f"  - Skipped: {skipped_count}")

        if retagged_count > 0:
            print(
                "\nNote: Retag operations are queued. "
                "Check Lidarr's queue for progress."
            )


if __name__ == "__main__":
    main()
