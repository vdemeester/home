#!/usr/bin/env -S uv run --quiet --script
# /// script
# dependencies = [
#   "requests",
# ]
# ///
"""
Rename series episodes in Sonarr with interactive confirmation.

This script:
1. Fetches all series from Sonarr API
2. Checks which series have episodes that need renaming
3. Previews the rename changes for each series
4. Asks for confirmation before applying renames

Usage:
    ./sonarr-rename-series.py <sonarr_url> <api_key>

Example:
    ./sonarr-rename-series.py http://localhost:8989 your-api-key
"""

import argparse
import sys
from typing import Any, Dict, List

import requests


def get_all_series(base_url: str, api_key: str) -> List[Dict[str, Any]]:
    """Fetch all series from Sonarr API."""
    url = f"{base_url}/api/v3/series"
    headers = {"X-Api-Key": api_key}

    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        print(f"Error fetching series: {e}", file=sys.stderr)
        sys.exit(1)


def get_rename_preview(
    base_url: str, api_key: str, series_id: int
) -> List[Dict[str, Any]]:
    """Get preview of files that will be renamed for a series."""
    url = f"{base_url}/api/v3/rename"
    headers = {"X-Api-Key": api_key}
    params = {"seriesId": series_id}

    try:
        response = requests.get(url, headers=headers, params=params)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        print(
            f"Error fetching rename preview for series {series_id}: {e}",
            file=sys.stderr,
        )
        return []


def execute_rename(
    base_url: str, api_key: str, series_id: int, file_ids: List[int]
) -> Dict[str, Any]:
    """Execute rename operation for a series."""
    url = f"{base_url}/api/v3/command"
    headers = {"X-Api-Key": api_key, "Content-Type": "application/json"}
    payload = {
        "name": "RenameFiles",
        "seriesId": series_id,
        "files": file_ids,
    }

    try:
        response = requests.post(url, headers=headers, json=payload)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        print(
            f"Error executing rename for series {series_id}: {e}",
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
        description="Rename Sonarr series episodes with confirmation"
    )
    parser.add_argument(
        "sonarr_url", help="Sonarr base URL (e.g., http://localhost:8989)"
    )
    parser.add_argument("api_key", help="Sonarr API key")
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
    base_url = args.sonarr_url.rstrip("/")

    print(f"Fetching series from {base_url}...")
    all_series = get_all_series(base_url, args.api_key)
    print(f"Found {len(all_series)} series\n")

    series_with_renames = []
    series_without_renames = []

    # Check each series for rename candidates
    print("Checking which series need renaming...")
    for series in all_series:
        series_id = series.get("id")
        series_title = series.get("title", "Unknown")

        rename_preview = get_rename_preview(
            base_url, args.api_key, series_id
        )

        if rename_preview:
            series_with_renames.append(
                (series_id, series_title, rename_preview)
            )
        else:
            series_without_renames.append(series_title)

    # Print summary
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)

    if series_without_renames:
        count = len(series_without_renames)
        print(f"\n✓ No renames needed ({count} series):")
        for title in series_without_renames[:5]:  # Show first 5
            print(f"  - {title}")
        if len(series_without_renames) > 5:
            print(f"  ... and {len(series_without_renames) - 5} more")

    if series_with_renames:
        count = len(series_with_renames)
        print(f"\n→ Series with renames needed: {count}")

    if not series_with_renames:
        print("\nNo series need renaming!")
        return

    # Process each series that needs renaming
    print("\n" + "=" * 80)
    print("RENAME PREVIEW")
    print("=" * 80)

    renamed_count = 0
    skipped_count = 0

    for series_id, series_title, rename_preview in series_with_renames:
        print(f"\n{'=' * 80}")
        print(f"Series: {series_title}")
        print(f"Episodes to rename: {len(rename_preview)}")
        print("=" * 80)

        # Show preview of renames (limit to first 10)
        display_limit = 10
        for i, item in enumerate(rename_preview[:display_limit]):
            existing_path = item.get("existingPath", "Unknown")
            new_path = item.get("newPath", "Unknown")
            print(f"\n  Episode {i + 1}:")
            print(f"    FROM: {existing_path}")
            print(f"    TO:   {new_path}")

        if len(rename_preview) > display_limit:
            remaining = len(rename_preview) - display_limit
            print(f"\n  ... and {remaining} more episodes")

        # Ask for confirmation (unless in dry-run or no-confirm mode)
        should_rename = False

        if args.dry_run:
            print("\n[DRY RUN] Skipping actual rename")
        elif args.no_confirm:
            should_rename = True
            print("\n[NO CONFIRM] Proceeding with rename...")
        else:
            prompt = (
                f"\nRename {len(rename_preview)} episodes "
                f"for '{series_title}'?"
            )
            should_rename = ask_confirmation(prompt)

        if should_rename and not args.dry_run:
            print("Executing rename...")
            # Extract episode file IDs from preview
            file_ids = [item.get("episodeFileId") for item in rename_preview]
            file_ids = [fid for fid in file_ids if fid is not None]

            if file_ids:
                result = execute_rename(
                    base_url, args.api_key, series_id, file_ids
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
            f"\n[DRY RUN] Found {len(series_with_renames)} series "
            "that need renaming"
        )
        print("No changes were made. Remove --dry-run to apply renames.")
    else:
        print(f"\nSeries processed: {len(series_with_renames)}")
        print(f"  - Renamed: {renamed_count}")
        print(f"  - Skipped: {skipped_count}")

        if renamed_count > 0:
            print(
                "\nNote: Rename operations are queued. "
                "Check Sonarr's queue for progress."
            )


if __name__ == "__main__":
    main()
