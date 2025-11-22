#!/usr/bin/env -S uv run --quiet --script
# /// script
# dependencies = [
#   "requests",
# ]
# ///
"""
Rename movies in Radarr with interactive confirmation.

This script:
1. Fetches all movies from Radarr API
2. Checks which movies have files that need renaming
3. Previews the rename changes for each movie
4. Asks for confirmation before applying renames

Usage:
    ./radarr-rename-movies.py <radarr_url> <api_key>

Example:
    ./radarr-rename-movies.py http://localhost:7878 your-api-key
"""

import argparse
import sys
from typing import Any, Dict, List

import requests


def get_all_movies(base_url: str, api_key: str) -> List[Dict[str, Any]]:
    """Fetch all movies from Radarr API."""
    url = f"{base_url}/api/v3/movie"
    headers = {"X-Api-Key": api_key}

    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        print(f"Error fetching movies: {e}", file=sys.stderr)
        sys.exit(1)


def get_rename_preview(
    base_url: str, api_key: str, movie_id: int
) -> List[Dict[str, Any]]:
    """Get preview of files that will be renamed for a movie."""
    url = f"{base_url}/api/v3/rename"
    headers = {"X-Api-Key": api_key}
    params = {"movieId": movie_id}

    try:
        response = requests.get(url, headers=headers, params=params)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        print(
            f"Error fetching rename preview for movie {movie_id}: {e}",
            file=sys.stderr,
        )
        return []


def execute_rename(
    base_url: str, api_key: str, movie_id: int
) -> Dict[str, Any]:
    """Execute rename operation for a movie."""
    url = f"{base_url}/api/v3/command"
    headers = {"X-Api-Key": api_key, "Content-Type": "application/json"}
    payload = {"name": "RenameMovie", "movieIds": [movie_id]}

    try:
        response = requests.post(url, headers=headers, json=payload)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        print(
            f"Error executing rename for movie {movie_id}: {e}",
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
        description="Rename Radarr movies with confirmation"
    )
    parser.add_argument(
        "radarr_url", help="Radarr base URL (e.g., http://localhost:7878)"
    )
    parser.add_argument("api_key", help="Radarr API key")
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be renamed without making changes",
    )
    parser.add_argument(
        "--no-confirm",
        action="store_true",
        help="Skip interactive confirmation (use with caution)",
    )

    args = parser.parse_args()

    # Normalize URLs
    base_url = args.radarr_url.rstrip("/")

    print(f"Fetching movies from {base_url}...")
    all_movies = get_all_movies(base_url, args.api_key)
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

        rename_preview = get_rename_preview(
            base_url, args.api_key, movie_id
        )

        if rename_preview:
            movies_with_renames.append(
                (movie_id, display_title, rename_preview)
            )
        else:
            movies_without_renames.append(display_title)

    # Print summary
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)

    if movies_without_renames:
        count = len(movies_without_renames)
        print(f"\n✓ No renames needed ({count} movies):")
        for title in movies_without_renames[:5]:  # Show first 5
            print(f"  - {title}")
        if len(movies_without_renames) > 5:
            print(f"  ... and {len(movies_without_renames) - 5} more")

    if movies_with_renames:
        count = len(movies_with_renames)
        print(f"\n→ Movies with renames needed: {count}")

    if not movies_with_renames:
        print("\nNo movies need renaming!")
        return

    # Process each movie that needs renaming
    print("\n" + "=" * 80)
    print("RENAME PREVIEW")
    print("=" * 80)

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

        # Ask for confirmation (unless in dry-run or no-confirm mode)
        should_rename = False

        if args.dry_run:
            print("\n[DRY RUN] Skipping actual rename")
        elif args.no_confirm:
            should_rename = True
            print("\n[NO CONFIRM] Proceeding with rename...")
        else:
            file_word = "file" if len(rename_preview) == 1 else "files"
            should_rename = ask_confirmation(
                f"\nRename {len(rename_preview)} {file_word} "
                f"for '{movie_title}'?"
            )

        if should_rename and not args.dry_run:
            print("Executing rename...")
            result = execute_rename(base_url, args.api_key, movie_id)
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
    print("\n" + "=" * 80)
    print("FINAL SUMMARY")
    print("=" * 80)

    if args.dry_run:
        print(
            f"\n[DRY RUN] Found {len(movies_with_renames)} movies "
            "that need renaming"
        )
        print("No changes were made. Remove --dry-run to apply renames.")
    else:
        print(f"\nMovies processed: {len(movies_with_renames)}")
        print(f"  - Renamed: {renamed_count}")
        print(f"  - Skipped: {skipped_count}")

        if renamed_count > 0:
            print(
                "\nNote: Rename operations are queued. "
                "Check Radarr's queue for progress."
            )


if __name__ == "__main__":
    main()
