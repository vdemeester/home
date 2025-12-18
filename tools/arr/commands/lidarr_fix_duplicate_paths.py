"""
Fix duplicate path segments in Lidarr artist paths.

This script:
1. Fetches all artists from Lidarr API
2. Detects duplicate path segments (e.g., /music/library/library/Artist)
3. Fixes them to remove the duplicate (e.g., /music/library/Artist)
4. Updates the artist paths via PUT /api/v1/artist/{id}
"""


from lib import ArrClient, CommandContext, print_section_header


def strip_duplicate_path_segments(path: str) -> tuple[str, bool, str]:
    """
    Detect and strip duplicate path segments.

    For example: /neo/music/library/library -> /neo/music/library

    Returns:
        Tuple of (cleaned_path, was_modified, duplicate_segment)
    """
    parts = [p for p in path.split('/') if p]  # Split and remove empty parts

    # Check for any duplicate consecutive segments
    for i in range(len(parts) - 1):
        if parts[i] == parts[i + 1]:
            # Found duplicate - remove it
            cleaned_parts = parts[:i + 1] + parts[i + 2:]
            cleaned = '/' + '/'.join(cleaned_parts)
            return (cleaned, True, parts[i])

    return (path, False, "")


def run(url: str, api_key: str, dry_run: bool, no_confirm: bool):
    """Execute the lidarr fix-duplicate-paths command."""
    # Create client and context
    lidarr = ArrClient(url, api_key)
    ctx = CommandContext(dry_run, no_confirm)

    print_section_header("FETCHING ARTISTS FROM LIDARR")
    print("Fetching all artists...")
    artists = lidarr.get("/api/v1/artist")
    print(f"Found {len(artists)} artists\n")

    # Analyze all artists for duplicate path segments
    needs_fixing = []
    already_correct = []

    for artist in artists:
        artist_name = artist.get("artistName", "Unknown")
        artist_id = artist.get("id")
        current_path = artist.get("path", "")

        cleaned_path, has_duplicate, duplicate_segment = strip_duplicate_path_segments(current_path)

        if has_duplicate:
            needs_fixing.append({
                "id": artist_id,
                "name": artist_name,
                "old_path": current_path,
                "new_path": cleaned_path,
                "duplicate": duplicate_segment,
            })
        else:
            already_correct.append(artist_name)

    # Print summary
    print_section_header("SUMMARY")

    if already_correct:
        print(f"\n✓ Already correct ({len(already_correct)} artists)")
        if len(already_correct) <= 10:
            for name in already_correct:
                print(f"  - {name}")
        else:
            for name in already_correct[:5]:
                print(f"  - {name}")
            print(f"  ... and {len(already_correct) - 5} more")

    if needs_fixing:
        print(f"\n→ Needs fixing ({len(needs_fixing)} artists):\n")
        for artist_info in needs_fixing:
            print(f"  • {artist_info['name']}")
            print(f"    Duplicate segment: '{artist_info['duplicate']}'")
            print(f"    FROM: {artist_info['old_path']}")
            print(f"    TO:   {artist_info['new_path']}")
            print()
    else:
        print("\n✓ No artists with duplicate path segments found!")
        return

    # Perform fixes
    if not ctx.dry_run:
        # Ask for confirmation
        if not ctx.no_confirm:
            response = input(f"\nFix {len(needs_fixing)} artist path(s)? (y/n): ").lower().strip()
            if response not in ["y", "yes"]:
                print("Operation cancelled")
                return

        print_section_header("FIXING PATHS")

        success_count = 0
        fail_count = 0

        for artist_info in needs_fixing:
            artist_id = artist_info["id"]
            artist_name = artist_info["name"]
            new_path = artist_info["new_path"]

            print(f"\nFixing {artist_name}...", end=" ")

            # Fetch current artist data
            artist_data = lidarr.get(f"/api/v1/artist/{artist_id}")
            if not artist_data:
                print("✗ FAILED (could not fetch artist data)")
                fail_count += 1
                continue

            # Update the path
            artist_data["path"] = new_path

            # Send the update
            result = lidarr.put(f"/api/v1/artist/{artist_id}", artist_data)
            if result:
                print("✓ SUCCESS")
                success_count += 1
            else:
                print("✗ FAILED")
                fail_count += 1

        print_section_header("FINAL SUMMARY")
        print(f"\nTotal artists processed: {len(needs_fixing)}")
        print(f"  - Successfully fixed: {success_count}")
        print(f"  - Failed: {fail_count}")

        if success_count > 0:
            print("\n✓ Path fixes have been applied!")
            print("Note: You may need to trigger a 'Rescan Artist Folder' in Lidarr")
            print("      for the changes to take full effect.")
    else:
        print("\n[DRY RUN] No changes were made. Remove --dry-run to apply fixes.")
