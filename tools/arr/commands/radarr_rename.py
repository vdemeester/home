"""
Rename movies in Radarr with interactive confirmation.

This script:
1. Fetches all movies from Radarr API
2. Checks which movies have files that need renaming
3. Previews the rename changes for each movie
4. Asks for confirmation before applying renames

Usage:
    arr radarr rename <radarr_url> <api_key>

Example:
    arr radarr rename http://localhost:7878 your-api-key
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


def get_rename_preview(
    client: ArrClient, movie_id: int
) -> List[Dict[str, Any]]:
    """Get preview of files that will be renamed for a movie."""
    return client.get("/api/v3/rename", params={"movieId": movie_id})


def execute_rename(client: ArrClient, movie_id: int) -> Dict[str, Any]:
    """Execute rename operation for a movie."""
    payload = {"name": "RenameMovie", "movieIds": [movie_id]}
    return client.post("/api/v3/command", payload)


def main():
    parser = create_arr_parser(
        "Radarr", "Rename Radarr movies with confirmation", 7878
    )
    args = parser.parse_args()

    # Create client
    client = ArrClient(args.radarr_url, args.api_key)

    print(f"Fetching movies from {client.base_url}...")
    all_movies = client.get("/api/v3/movie")
    print(f"Found {len(all_movies)} movies\n")

    movies_with_renames = []
    movies_without_renames = []

    # Check each movie for rename candidates
    print("Checking which movies need renaming...")
    for movie in all_movies:
        movie_id = movie.get("id")
        movie_title = movie.get("title", "Unknown")
        year = movie.get("year", "")
        display_title = (
            f"{movie_title} ({year})" if year else movie_title
        )

        rename_preview = get_rename_preview(client, movie_id)

        if rename_preview:
            movies_with_renames.append(
                (movie_id, display_title, rename_preview)
            )
        else:
            movies_without_renames.append(display_title)

    # Print summary
    print_section_header("SUMMARY")
    print_item_list(movies_without_renames, "✓ No renames needed")

    if movies_with_renames:
        count = len(movies_with_renames)
        print(f"\n→ Movies with renames needed: {count}")

    if not movies_with_renames:
        print("\nNo movies need renaming!")
        return

    # Process each movie that needs renaming
    print_section_header("RENAME PREVIEW")

    renamed_count = 0
    skipped_count = 0

    for movie_id, movie_title, rename_preview in movies_with_renames:
        print(f"\n{'=' * 80}")
        print(f"Movie: {movie_title}")
        print(f"Files to rename: {len(rename_preview)}")
        print("=" * 80)

        # Show preview of renames
        for i, item in enumerate(rename_preview):
            existing_path = item.get("existingPath", "Unknown")
            new_path = item.get("newPath", "Unknown")
            print(f"\n  File {i + 1}:")
            print(f"    FROM: {existing_path}")
            print(f"    TO:   {new_path}")

        # Ask for confirmation
        file_word = "file" if len(rename_preview) == 1 else "files"
        prompt = (
            f"\nRename {len(rename_preview)} {file_word} "
            f"for '{movie_title}'?"
        )
        should_rename = get_confirmation_decision(args, prompt)

        if should_rename:
            print("Executing rename...")
            result = execute_rename(client, movie_id)
            if result:
                print("✓ Rename command queued successfully")
                renamed_count += 1
            else:
                print("✗ Failed to queue rename command")
                skipped_count += 1
        else:
            if not args.dry_run:
                print("Skipped")
            skipped_count += 1

    # Final summary
    if args.dry_run:
        print_section_header("FINAL SUMMARY")
        print(
            f"\n[DRY RUN] Found {len(movies_with_renames)} movies "
            "that need renaming"
        )
        print("No changes were made. Remove --dry-run to apply renames.")
    else:
        print_final_summary(
            len(movies_with_renames),
            renamed_count,
            skipped_count,
            "Renamed",
        )


if __name__ == "__main__":
    main()
