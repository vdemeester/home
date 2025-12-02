"""
Update artist metadata profile settings in Lidarr.

This script allows you to change the metadata profile for artists in Lidarr.
You can update all artists or filter by name pattern.
"""

from typing import Any, Dict, List

from lib import (
    ArrClient,
    CommandContext,
    get_confirmation_decision,
    print_item_list,
    print_section_header,
)


def get_artists(client: ArrClient) -> List[Dict[str, Any]]:
    """Get all artists from Lidarr."""
    return client.get("/api/v1/artist")


def get_metadata_profiles(client: ArrClient) -> List[Dict[str, Any]]:
    """Get all metadata profiles from Lidarr."""
    return client.get("/api/v1/metadataprofile")


def filter_artists(
    artists: List[Dict[str, Any]], pattern: str = None
) -> List[Dict[str, Any]]:
    """
    Filter artists by name pattern.

    Args:
        artists: List of artist objects
        pattern: Optional substring to match in artist names (case-insensitive)

    Returns:
        Filtered list of artists
    """
    if not pattern:
        return artists

    pattern_lower = pattern.lower()
    return [
        artist
        for artist in artists
        if pattern_lower in artist.get("artistName", "").lower()
    ]


def update_artist_metadata_profile(
    client: ArrClient, artist: Dict[str, Any], metadata_profile_id: int
) -> bool:
    """
    Update an artist's metadata profile.

    Args:
        client: Lidarr API client
        artist: Artist object to update
        metadata_profile_id: Metadata profile ID to set

    Returns:
        True if successful, False otherwise
    """
    # Update the artist object with new metadata profile
    artist["metadataProfileId"] = metadata_profile_id

    # Send PUT request to update the artist
    result = client.put("/api/v1/artist", artist)

    return bool(result and result.get("id"))


def run(
    lidarr_url: str,
    lidarr_api_key: str,
    metadata_profile_name: str,
    artist_pattern: str,
    dry_run: bool,
    no_confirm: bool,
):
    """Execute the lidarr update-metadata-profile command."""
    # Create client and context
    lidarr = ArrClient(lidarr_url, lidarr_api_key)
    ctx = CommandContext(dry_run, no_confirm)

    # Fetch metadata profiles
    print_section_header("FETCHING METADATA PROFILES")
    print("Retrieving metadata profiles...")
    metadata_profiles = get_metadata_profiles(lidarr)

    if not metadata_profiles:
        print("\nNo metadata profiles found in Lidarr!")
        return

    print(f"Found {len(metadata_profiles)} metadata profile(s):\n")
    for profile in metadata_profiles:
        print(f"  - {profile.get('name')} (ID: {profile.get('id')})")

    # Find the target metadata profile
    target_profile = None
    if metadata_profile_name:
        # Search by name (case-insensitive)
        for profile in metadata_profiles:
            profile_name = profile.get("name", "").lower()
            if profile_name == metadata_profile_name.lower():
                target_profile = profile
                break

        if not target_profile:
            print(
                f"\nError: Metadata profile '{metadata_profile_name}' "
                "not found!"
            )
            print("\nAvailable profiles:")
            for profile in metadata_profiles:
                print(f"  - {profile.get('name')}")
            return
    else:
        # Use the first profile as default
        target_profile = metadata_profiles[0]
        print(
            f"\nNo profile specified, using default: "
            f"{target_profile.get('name')}"
        )

    metadata_profile_id = target_profile.get("id")
    metadata_profile_name = target_profile.get("name")

    print(
        f"\nTarget metadata profile: {metadata_profile_name} "
        f"(ID: {metadata_profile_id})"
    )

    # Fetch all artists
    print_section_header("FETCHING ARTISTS FROM LIDARR")
    print("Retrieving artists...")
    all_artists = get_artists(lidarr)

    if not all_artists:
        print("\nNo artists found in Lidarr!")
        return

    print(f"Found {len(all_artists)} total artists in Lidarr")

    # Filter artists if pattern provided
    if artist_pattern:
        print(f"\nFiltering artists by pattern: '{artist_pattern}'")
        filtered_artists = filter_artists(all_artists, artist_pattern)
        print(f"Matched {len(filtered_artists)} artists")
    else:
        filtered_artists = all_artists
        print("\nNo filter specified - will update ALL artists")

    if not filtered_artists:
        print("\nNo artists match the specified pattern!")
        return

    # Display artists to be updated
    print_section_header("ARTISTS TO UPDATE")
    artist_names = [
        artist.get("artistName", "Unknown") for artist in filtered_artists
    ]
    prefix_msg = (
        f"Artists that will be set to metadata profile "
        f"'{metadata_profile_name}'"
    )
    print_item_list(artist_names, prefix_msg, max_display=20)

    # Confirm operation
    confirm_msg = (
        f"\nUpdate metadata profile for {len(filtered_artists)} "
        f"artist(s) to '{metadata_profile_name}'?"
    )
    if not get_confirmation_decision(ctx, confirm_msg):
        if not ctx.dry_run:
            print("Operation cancelled")
        return

    # Update artists
    print_section_header("UPDATING ARTISTS")

    updated_count = 0
    failed_count = 0

    for idx, artist in enumerate(filtered_artists, 1):
        artist_name = artist.get("artistName", "Unknown")
        current_profile_id = artist.get("metadataProfileId", "Unknown")
        print(
            f"[{idx}/{len(filtered_artists)}] {artist_name} "
            f"(current profile ID: {current_profile_id})"
        )

        if not ctx.dry_run:
            success = update_artist_metadata_profile(
                lidarr, artist, metadata_profile_id
            )
            if success:
                print(
                    f"  ✓ Updated to metadata profile "
                    f"'{metadata_profile_name}'"
                )
                updated_count += 1
            else:
                print("  ✗ Failed to update")
                failed_count += 1
        else:
            print(
                f"  [DRY RUN] Would update to metadata profile "
                f"'{metadata_profile_name}'"
            )
            updated_count += 1

    # Final summary
    print_section_header("FINAL SUMMARY")
    print(f"\nTotal artists: {len(all_artists)}")
    print(f"Artists selected: {len(filtered_artists)}")
    print(f"  - Successfully updated: {updated_count}")
    if failed_count > 0:
        print(f"  - Failed: {failed_count}")

    if ctx.dry_run:
        print(
            "\n[DRY RUN] No changes were made. "
            "Remove --dry-run to update artists."
        )
    elif updated_count > 0:
        print(
            f"\nArtists are now set to metadata profile: "
            f"{metadata_profile_name}\n"
            "This will affect which album types and releases are "
            "monitored and downloaded."
        )
