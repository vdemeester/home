"""
Sync Spotify playlists to Lidarr.

This script:
1. Fetches tracks from specified Spotify playlists
2. Extracts unique artists from the playlist tracks
3. Checks which artists are already in Lidarr
4. Adds missing artists to Lidarr with monitoring options
5. Optionally monitors specific albums that appear in playlists
"""

import time
from typing import Any, Dict, List, Set

from lib import (
    ArrClient,
    CommandContext,
    SpotifyClient,
    get_confirmation_decision,
    print_item_list,
    print_section_header,
    select_with_fzf,
)


def get_quality_profile_id(client: ArrClient) -> int:
    """Get the first available quality profile ID."""
    profiles = client.get("/api/v1/qualityprofile")
    if profiles and len(profiles) > 0:
        return profiles[0].get("id")
    return 1  # Default fallback


def get_metadata_profile_id(client: ArrClient) -> int:
    """Get the first available metadata profile ID."""
    profiles = client.get("/api/v1/metadataprofile")
    if profiles and len(profiles) > 0:
        return profiles[0].get("id")
    return 1  # Default fallback


def search_artist_in_lidarr(
    client: ArrClient, artist_name: str
) -> List[Dict[str, Any]]:
    """Search for an artist in Lidarr's database."""
    return client.get("/api/v1/search", params={"term": artist_name})


def get_existing_artists(client: ArrClient) -> Set[str]:
    """Get set of artist names already in Lidarr."""
    artists = client.get("/api/v1/artist")
    return {artist.get("artistName", "").lower() for artist in artists}


def add_artist_to_lidarr(
    client: ArrClient,
    artist: Dict[str, Any],
    root_folder: str,
    quality_profile_id: int,
    metadata_profile_id: int,
    monitor: str = "all",
) -> Dict[str, Any]:
    """
    Add an artist to Lidarr.

    Args:
        client: Lidarr API client
        artist: Artist data from search results
        root_folder: Root folder path for music
        quality_profile_id: Quality profile ID
        metadata_profile_id: Metadata profile ID
        monitor: Monitoring option (all, future, missing, existing, none)

    Returns:
        API response
    """
    payload = {
        "artistName": artist.get("artistName"),
        "foreignArtistId": artist.get("foreignArtistId"),
        "qualityProfileId": quality_profile_id,
        "metadataProfileId": metadata_profile_id,
        "rootFolderPath": root_folder,
        "monitored": True,
        "addOptions": {"monitor": monitor, "searchForMissingAlbums": False},
    }
    return client.post("/api/v1/artist", payload)


def run(
    lidarr_url: str,
    lidarr_api_key: str,
    spotify_client_id: str,
    spotify_client_secret: str,
    spotify_username: str,
    playlist_ids: List[str],
    root_folder: str,
    monitor: str,
    request_delay: float,
    dry_run: bool,
    no_confirm: bool,
):
    """Execute the lidarr sync-spotify command."""
    # Create clients and context
    lidarr = ArrClient(lidarr_url, lidarr_api_key)
    ctx = CommandContext(dry_run, no_confirm)

    # Determine if we need interactive mode
    use_interactive = not playlist_ids and spotify_username

    # Initialize Spotify client (always use client credentials)
    print("Initializing Spotify client...")
    spotify = SpotifyClient(spotify_client_id, spotify_client_secret)

    # Get playlist IDs interactively if needed
    if use_interactive:
        print(
            f"Fetching public playlists for user '{spotify_username}' "
            "from Spotify..."
        )
        user_playlists = spotify.get_user_playlists(spotify_username)

        if not user_playlists:
            print(
                f"\nNo public playlists found for user '{spotify_username}'!"
            )
            print(
                "Note: Only public playlists are accessible. "
                "Private playlists cannot be listed."
            )
            return

        print(f"Found {len(user_playlists)} public playlists\n")
        print(
            "Use fzf to select playlists (TAB to select, "
            "ENTER to confirm, ESC to cancel)"
        )

        # Use fzf for selection
        selected_ids = select_with_fzf(
            user_playlists, "{name} (by {owner}, {tracks_total} tracks)"
        )

        if not selected_ids:
            print("\nNo playlists selected. Exiting.")
            return

        playlist_ids = selected_ids
        print(f"\nSelected {len(playlist_ids)} playlist(s)\n")
    elif not playlist_ids:
        print(
            "\nError: No playlist IDs provided and no Spotify username set."
        )
        print(
            "Either provide playlist IDs as arguments or use "
            "--spotify-username for interactive mode."
        )
        print("\nExamples:")
        print(
            "  arr lidarr sync-spotify http://localhost:8686 "
            "-u your-username"
        )
        print(
            "  arr lidarr sync-spotify http://localhost:8686 "
            "PLAYLIST_ID_1 PLAYLIST_ID_2"
        )
        return

    # Get Lidarr configuration
    quality_profile_id = get_quality_profile_id(lidarr)
    metadata_profile_id = get_metadata_profile_id(lidarr)

    # Track all unique artists and their albums
    all_artists = {}  # artist_name -> {spotify_id, albums: set()}
    playlist_info = []

    print_section_header("FETCHING SPOTIFY PLAYLISTS")

    for playlist_id in playlist_ids:
        try:
            info = spotify.get_playlist_info(playlist_id)
            playlist_info.append(info)
            print(
                f"\nPlaylist: {info['name']} "
                f"(by {info['owner']}, {info['tracks_total']} tracks)"
            )

            tracks = spotify.get_playlist_tracks(playlist_id)
            print(f"  Retrieved {len(tracks)} tracks")

            # Extract artists and albums
            for track in tracks:
                for artist in track.get("artists", []):
                    artist_name = artist.get("name")
                    if artist_name:
                        if artist_name not in all_artists:
                            all_artists[artist_name] = {
                                "spotify_id": artist.get("id"),
                                "albums": set(),
                            }
                        # Track which albums appear in playlists
                        if track.get("album"):
                            all_artists[artist_name]["albums"].add(
                                track["album"]
                            )

        except Exception as e:
            print(f"Error fetching playlist {playlist_id}: {e}")
            continue

    if not all_artists:
        print("\nNo artists found in playlists!")
        return

    print(f"\n\nFound {len(all_artists)} unique artists across all playlists")

    # Check which artists are already in Lidarr
    print_section_header("CHECKING LIDARR")
    print("Fetching existing artists from Lidarr...")
    existing_artists = get_existing_artists(lidarr)
    print(f"Found {len(existing_artists)} artists already in Lidarr")

    # Separate artists into existing and missing
    artists_to_add = []
    artists_already_in_lidarr = []

    for artist_name, artist_data in all_artists.items():
        if artist_name.lower() in existing_artists:
            artists_already_in_lidarr.append(artist_name)
        else:
            artists_to_add.append((artist_name, artist_data))

    # Print summary
    print_section_header("SUMMARY")
    print_item_list(artists_already_in_lidarr, "Already in Lidarr")

    if artists_to_add:
        print(f"\n→ Artists to add: {len(artists_to_add)}")
        for artist_name, _ in artists_to_add[:10]:
            print(f"  - {artist_name}")
        if len(artists_to_add) > 10:
            print(f"  ... and {len(artists_to_add) - 10} more")
    else:
        print("\nAll artists from the playlists are already in Lidarr!")
        return

    # Ask for confirmation to proceed
    if not get_confirmation_decision(
        ctx, f"\nAdd {len(artists_to_add)} artists to Lidarr?"
    ):
        if not ctx.dry_run:
            print("Operation cancelled")
        return

    # Add artists to Lidarr
    print_section_header("ADDING ARTISTS TO LIDARR")
    print(
        f"Note: Adding {len(artists_to_add)} artists with delays "
        "to avoid overwhelming Lidarr..."
    )

    added_count = 0
    failed_count = 0

    for idx, (artist_name, artist_data) in enumerate(artists_to_add, 1):
        print(f"\n[{idx}/{len(artists_to_add)}] Searching for: {artist_name}")

        try:
            # Search for artist in Lidarr's MusicBrainz database
            search_results = search_artist_in_lidarr(lidarr, artist_name)

            if not search_results:
                print(f"  ✗ No results found for {artist_name}")
                failed_count += 1
                # Small delay even on failure to avoid hammering the API
                if idx < len(artists_to_add):
                    time.sleep(0.5)
                continue

            # Use the first result (most relevant)
            artist_match = search_results[0]
            artist_mb_name = artist_match.get("artistName", artist_name)

            print(f"  Found: {artist_mb_name}")
            print(f"  Albums in playlists: {len(artist_data['albums'])}")
            for album in list(artist_data["albums"])[:3]:
                print(f"    - {album}")
            if len(artist_data["albums"]) > 3:
                print(
                    f"    ... and {len(artist_data['albums']) - 3} more albums"
                )

            if not ctx.dry_run:
                result = add_artist_to_lidarr(
                    lidarr,
                    artist_match,
                    root_folder,
                    quality_profile_id,
                    metadata_profile_id,
                    monitor,
                )

                if result and result.get("id"):
                    print(f"  ✓ Added successfully (ID: {result['id']})")
                    added_count += 1
                else:
                    print("  ✗ Failed to add artist")
                    failed_count += 1
            else:
                print("  [DRY RUN] Would add this artist")
                added_count += 1

            # Add a delay between requests to avoid overwhelming Lidarr
            if idx < len(artists_to_add):
                delay = request_delay if not ctx.dry_run else 0.5
                time.sleep(delay)

        except Exception as e:
            print(f"  ✗ Error: {e}")
            failed_count += 1
            # Small delay even on error
            if idx < len(artists_to_add):
                time.sleep(0.5)

    # Final summary
    print_section_header("FINAL SUMMARY")
    print(f"\nTotal playlists processed: {len(playlist_info)}")
    print(f"Total unique artists found: {len(all_artists)}")
    print(f"Artists already in Lidarr: {len(artists_already_in_lidarr)}")
    print(f"Artists to add: {len(artists_to_add)}")
    print(f"  - Successfully added: {added_count}")
    print(f"  - Failed: {failed_count}")

    if ctx.dry_run:
        print(
            "\n[DRY RUN] No changes were made. "
            "Remove --dry-run to add artists."
        )
    elif added_count > 0:
        print(
            f"\nMonitoring mode: {monitor}\n"
            "New artists will start searching for albums based on "
            "your Lidarr settings."
        )
