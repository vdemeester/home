"""
Rename albums in Lidarr with interactive confirmation.

This script:
1. Fetches all artists from Lidarr API
2. Checks which artists have albums with files that need renaming
3. Previews the rename changes for each album
4. Asks for confirmation before applying renames

Usage:
    arr lidarr rename-albums <lidarr_url> <api_key>

Example:
    arr lidarr rename-albums http://localhost:8686 your-api-key
"""

from typing import Any, Dict, List

from lib import (
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


def get_rename_preview(
    client: ArrClient, artist_id: int
) -> List[Dict[str, Any]]:
    """Get preview of files that will be renamed for an artist."""
    return client.get("/api/v1/rename", params={"artistId": artist_id})


def execute_rename(
    client: ArrClient, artist_id: int, file_ids: List[int]
) -> Dict[str, Any]:
    """Execute rename operation for an artist."""
    payload = {
        "name": "RenameFiles",
        "artistId": artist_id,
        "files": file_ids,
    }
    return client.post("/api/v1/command", payload)


def main():
    parser = create_arr_parser(
        "Lidarr", "Rename Lidarr albums with confirmation", 8686
    )
    args = parser.parse_args()

    # Create client
    client = ArrClient(args.lidarr_url, args.api_key)

    print(f"Fetching artists from {client.base_url}...")
    all_artists = client.get("/api/v1/artist")
    print(f"Found {len(all_artists)} artists\n")

    artists_with_renames = []
    artists_without_renames = []

    # Check each artist for rename candidates
    print("Checking which artists have albums needing renaming...")
    for artist in all_artists:
        artist_id = artist.get("id")
        artist_name = artist.get("artistName", "Unknown")

        rename_preview = get_rename_preview(client, artist_id)

        if rename_preview:
            # Get album count for this artist
            albums = get_artist_albums(client, artist_id)
            album_count = len(albums) if albums else 0

            artists_with_renames.append(
                (artist_id, artist_name, album_count, rename_preview)
            )
        else:
            artists_without_renames.append(artist_name)

    # Print summary
    print_section_header("SUMMARY")
    print_item_list(artists_without_renames, "✓ No renames needed")

    if artists_with_renames:
        count = len(artists_with_renames)
        print(f"\n→ Artists with renames needed: {count}")

    if not artists_with_renames:
        print("\nNo artists need renaming!")
        return

    # Process each artist that needs renaming
    print_section_header("RENAME PREVIEW")

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

        # Ask for confirmation
        file_word = "file" if len(rename_preview) == 1 else "files"
        prompt = (
            f"\nRename {len(rename_preview)} {file_word} "
            f"for '{artist_name}'?"
        )
        should_rename = get_confirmation_decision(args, prompt)

        if should_rename:
            print("Executing rename...")
            # Extract track file IDs from preview
            file_ids = [item.get("trackFileId") for item in rename_preview]
            file_ids = [fid for fid in file_ids if fid is not None]

            if file_ids:
                result = execute_rename(client, artist_id, file_ids)
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
            f"\n[DRY RUN] Found {len(artists_with_renames)} artists "
            "that need renaming"
        )
        print("No changes were made. Remove --dry-run to apply renames.")
    else:
        print_final_summary(
            len(artists_with_renames),
            renamed_count,
            skipped_count,
            "Renamed",
        )


if __name__ == "__main__":
    main()
