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

from typing import Any, Dict, List

from arrlib import (
    ArrClient,
    create_arr_parser,
    get_confirmation_decision,
    print_final_summary,
    print_item_list,
    print_section_header,
)


def get_artist_albums(
    client: ArrClient, artist_id: int
) -> List[Dict[str, Any]]:
    """Fetch all albums for a specific artist."""
    return client.get("/api/v1/album", params={"artistId": artist_id})


def get_retag_preview(
    client: ArrClient, artist_id: int
) -> List[Dict[str, Any]]:
    """Get preview of files that will be retagged for an artist."""
    return client.get("/api/v1/retag", params={"artistId": artist_id})


def execute_retag(
    client: ArrClient, artist_id: int, file_ids: List[int]
) -> Dict[str, Any]:
    """Execute retag operation for an artist."""
    payload = {
        "name": "RetagFiles",
        "artistId": artist_id,
        "files": file_ids,
    }
    return client.post("/api/v1/command", payload)


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
    parser = create_arr_parser(
        "Lidarr", "Retag Lidarr albums with confirmation", 8686
    )
    args = parser.parse_args()

    # Create client
    client = ArrClient(args.lidarr_url, args.api_key)

    print(f"Fetching artists from {client.base_url}...")
    all_artists = client.get("/api/v1/artist")
    print(f"Found {len(all_artists)} artists\n")

    artists_with_retags = []
    artists_without_retags = []

    # Check each artist for retag candidates
    print("Checking which artists have albums needing retagging...")
    for artist in all_artists:
        artist_id = artist.get("id")
        artist_name = artist.get("artistName", "Unknown")

        retag_preview = get_retag_preview(client, artist_id)

        if retag_preview:
            # Get album count for this artist
            albums = get_artist_albums(client, artist_id)
            album_count = len(albums) if albums else 0

            artists_with_retags.append(
                (artist_id, artist_name, album_count, retag_preview)
            )
        else:
            artists_without_retags.append(artist_name)

    # Print summary
    print_section_header("SUMMARY")
    print_item_list(artists_without_retags, "✓ No retags needed")

    if artists_with_retags:
        count = len(artists_with_retags)
        print(f"\n→ Artists with retags needed: {count}")

    if not artists_with_retags:
        print("\nNo artists need retagging!")
        return

    # Process each artist that needs retagging
    print_section_header("RETAG PREVIEW")

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

        # Ask for confirmation
        file_word = "file" if len(retag_preview) == 1 else "files"
        prompt = (
            f"\nRetag {len(retag_preview)} {file_word} "
            f"for '{artist_name}'?"
        )
        should_retag = get_confirmation_decision(args, prompt)

        if should_retag:
            print("Executing retag...")
            # Extract track file IDs from preview
            file_ids = [item.get("trackFileId") for item in retag_preview]
            file_ids = [fid for fid in file_ids if fid is not None]

            if file_ids:
                result = execute_retag(client, artist_id, file_ids)
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
    if args.dry_run:
        print_section_header("FINAL SUMMARY")
        print(
            f"\n[DRY RUN] Found {len(artists_with_retags)} artists "
            "that need retagging"
        )
        print("No changes were made. Remove --dry-run to apply retags.")
    else:
        print_final_summary(
            len(artists_with_retags),
            retagged_count,
            skipped_count,
            "Retagged",
        )


if __name__ == "__main__":
    main()
