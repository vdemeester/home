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

from typing import Any, Dict, List

from arrlib import (
    ArrClient,
    create_arr_parser,
    get_confirmation_decision,
    print_final_summary,
    print_item_list,
    print_section_header,
)


def get_rename_preview(
    client: ArrClient, series_id: int
) -> List[Dict[str, Any]]:
    """Get preview of files that will be renamed for a series."""
    return client.get("/api/v3/rename", params={"seriesId": series_id})


def execute_rename(
    client: ArrClient, series_id: int, file_ids: List[int]
) -> Dict[str, Any]:
    """Execute rename operation for a series."""
    payload = {
        "name": "RenameFiles",
        "seriesId": series_id,
        "files": file_ids,
    }
    return client.post("/api/v3/command", payload)


def main():
    parser = create_arr_parser(
        "Sonarr", "Rename Sonarr series episodes with confirmation", 8989
    )
    args = parser.parse_args()

    # Create client
    client = ArrClient(args.sonarr_url, args.api_key)

    print(f"Fetching series from {client.base_url}...")
    all_series = client.get("/api/v3/series")
    print(f"Found {len(all_series)} series\n")

    series_with_renames = []
    series_without_renames = []

    # Check each series for rename candidates
    print("Checking which series need renaming...")
    for series in all_series:
        series_id = series.get("id")
        series_title = series.get("title", "Unknown")

        rename_preview = get_rename_preview(client, series_id)

        if rename_preview:
            series_with_renames.append(
                (series_id, series_title, rename_preview)
            )
        else:
            series_without_renames.append(series_title)

    # Print summary
    print_section_header("SUMMARY")
    print_item_list(series_without_renames, "✓ No renames needed")

    if series_with_renames:
        count = len(series_with_renames)
        print(f"\n→ Series with renames needed: {count}")

    if not series_with_renames:
        print("\nNo series need renaming!")
        return

    # Process each series that needs renaming
    print_section_header("RENAME PREVIEW")

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

        # Ask for confirmation
        prompt = (
            f"\nRename {len(rename_preview)} episodes "
            f"for '{series_title}'?"
        )
        should_rename = get_confirmation_decision(args, prompt)

        if should_rename:
            print("Executing rename...")
            # Extract episode file IDs from preview
            file_ids = [item.get("episodeFileId") for item in rename_preview]
            file_ids = [fid for fid in file_ids if fid is not None]

            if file_ids:
                result = execute_rename(client, series_id, file_ids)
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
    if args.dry_run:
        print_section_header("FINAL SUMMARY")
        print(
            f"\n[DRY RUN] Found {len(series_with_renames)} series "
            "that need renaming"
        )
        print("No changes were made. Remove --dry-run to apply renames.")
    else:
        print_final_summary(
            len(series_with_renames),
            renamed_count,
            skipped_count,
            "Renamed",
        )


if __name__ == "__main__":
    main()
