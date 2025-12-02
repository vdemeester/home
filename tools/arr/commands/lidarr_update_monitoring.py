"""
Update artist monitoring settings in Lidarr.

This script allows you to change the monitoring mode for artists in Lidarr.
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


def update_artist_monitoring(
    client: ArrClient,
    artist: Dict[str, Any],
    monitor_mode: str,
    monitor_new_items: str = None,
) -> bool:
    """
    Update an artist's monitoring settings.

    Args:
        client: Lidarr API client
        artist: Artist object to update
        monitor_mode: Monitoring mode (all, future, missing, existing, none)
        monitor_new_items: Monitor new items mode (all, new, none) - optional

    Returns:
        True if successful, False otherwise
    """
    # Update the artist object with new monitoring settings
    artist["addOptions"] = {"monitor": monitor_mode}

    # Update monitorNewItems if specified
    if monitor_new_items is not None:
        artist["monitorNewItems"] = monitor_new_items

    # Send PUT request to update the artist
    result = client.put("/api/v1/artist", artist)

    return bool(result and result.get("id"))


def run(
    lidarr_url: str,
    lidarr_api_key: str,
    monitor_mode: str,
    monitor_new_items: str,
    artist_pattern: str,
    dry_run: bool,
    no_confirm: bool,
):
    """Execute the lidarr update-monitoring command."""
    # Create client and context
    lidarr = ArrClient(lidarr_url, lidarr_api_key)
    ctx = CommandContext(dry_run, no_confirm)

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

    # Build description of what will be updated
    update_desc = f"monitor mode '{monitor_mode}'"
    if monitor_new_items:
        update_desc += f" and monitor new items '{monitor_new_items}'"

    print_item_list(
        artist_names,
        f"Artists that will be set to {update_desc}",
        max_display=20,
    )

    # Confirm operation
    confirm_msg = (
        f"\nUpdate monitoring for {len(filtered_artists)} artist(s) "
        f"to '{monitor_mode}'"
    )
    if monitor_new_items:
        confirm_msg += f" (monitor new items: '{monitor_new_items}')"
    confirm_msg += "?"

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
        current_monitor_new = artist.get("monitorNewItems", "Unknown")
        print(
            f"[{idx}/{len(filtered_artists)}] {artist_name} "
            f"(current monitorNewItems: {current_monitor_new})"
        )

        if not ctx.dry_run:
            success = update_artist_monitoring(
                lidarr, artist, monitor_mode, monitor_new_items
            )
            if success:
                msg = f"  ✓ Updated to monitor '{monitor_mode}'"
                if monitor_new_items:
                    msg += f" (monitor new items: '{monitor_new_items}')"
                print(msg)
                updated_count += 1
            else:
                print("  ✗ Failed to update")
                failed_count += 1
        else:
            msg = f"  [DRY RUN] Would update to monitor '{monitor_mode}'"
            if monitor_new_items:
                msg += f" (monitor new items: '{monitor_new_items}')"
            print(msg)
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
        summary_msg = f"\nArtists are now set to monitor: {monitor_mode}"
        if monitor_new_items:
            summary_msg += f"\nMonitor new items: {monitor_new_items}"
        summary_msg += (
            "\nLidarr will automatically search for albums based on "
            "these settings."
        )
        print(summary_msg)
