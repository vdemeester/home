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

import requests

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


def get_root_folder_path(client: ArrClient, preferred_path: str = None) -> str:
    """
    Get the appropriate root folder path from Lidarr.

    Args:
        client: Lidarr API client
        preferred_path: Optional preferred root folder path

    Returns:
        Root folder path to use
    """
    folders = client.get("/api/v1/rootfolder")
    if not folders or len(folders) == 0:
        return "/music"  # Default fallback

    # If user specified a preferred path, try to use it
    if preferred_path:
        for folder in folders:
            if folder.get("path") == preferred_path:
                return preferred_path

    # Otherwise, use the first one
    # Show all available folders for user awareness
    print("Available root folders:")
    for idx, folder in enumerate(folders):
        marker = " (using)" if idx == 0 else ""
        print(f"  [{idx+1}] {folder.get('path')}{marker}")

    selected_path = folders[0].get("path")

    # Check for duplicate path segments and fix if found
    cleaned_path, was_modified, duplicate_seg = strip_duplicate_path_segments(selected_path)
    if was_modified:
        print(f"\nWARNING: Root folder has duplicate path segment '{duplicate_seg}'!")
        print(f"  Original: {selected_path}")
        print(f"  Using:    {cleaned_path}")
        print("  Consider fixing the root folder configuration in Lidarr settings.\n")
        return cleaned_path

    return selected_path


def search_musicbrainz_artist(artist_name: str, debug: bool = False) -> Dict[str, Any] | None:
    """
    Search MusicBrainz directly for an artist.

    Returns artist data compatible with Lidarr's format, or None if not found.

    Note: MusicBrainz rate limit is 1 request/second, enforced with sleep.
    """
    def query_mb(search_term: str, require_exact: bool = False):
        """Helper to query MusicBrainz with a specific search term."""
        url = "https://musicbrainz.org/ws/2/artist"
        headers = {
            "User-Agent": "LidarrSpotifySync/1.0 (https://github.com/yourusername/yourrepo)",
            "Accept": "application/json",
        }
        params = {
            "query": f'artist:"{search_term}"',
            "fmt": "json",
            "limit": 5,
        }

        # MusicBrainz rate limit: 1 request per second
        time.sleep(1.0)

        response = requests.get(url, headers=headers, params=params, timeout=10)
        response.raise_for_status()
        data = response.json()

        artists = data.get("artists", [])
        if not artists:
            return None

        # Filter results
        for artist in artists:
            mb_name = artist.get("name", "")

            # For exact match requirement, compare case-insensitive but preserve accents
            if require_exact:
                if mb_name.lower() == search_term.lower():
                    return artist
            else:
                # For normalized match, use our matching function
                if artist_name_matches(mb_name, artist_name):
                    return artist

        return None

    try:
        # First try: exact match with original name (preserves accents)
        if debug:
            print(f"  DEBUG: Querying MusicBrainz for exact match: '{artist_name}'")

        result = query_mb(artist_name, require_exact=True)
        if result:
            mb_name = result.get("name", "")
            if debug:
                print(f"  DEBUG: Found exact match in MusicBrainz: '{mb_name}'")
        else:
            # Second try: normalized version (without accents)
            normalized = normalize_artist_name(artist_name)
            if normalized != artist_name:
                if debug:
                    print(f"  DEBUG: No exact match, trying normalized: '{normalized}'")
                result = query_mb(normalized, require_exact=False)
                if result:
                    mb_name = result.get("name", "")
                    if debug:
                        print(f"  DEBUG: Found normalized match in MusicBrainz: '{mb_name}'")

        if not result:
            if debug:
                print("  DEBUG: MusicBrainz found no matching results")
            return None

        # Convert MusicBrainz data to Lidarr format
        mb_id = result.get("id")
        mb_name = result.get("name", "")
        artist_type = result.get("type", "").capitalize()

        # Map MusicBrainz types to Lidarr types
        if artist_type == "Person":
            artist_type = "Person"
        elif artist_type == "Group":
            artist_type = "Group"
        else:
            artist_type = "Person"  # Default

        return {
            "artistName": mb_name,
            "foreignArtistId": mb_id,
            "artistType": artist_type,
            "disambiguation": result.get("disambiguation", ""),
            "links": [],
            "images": [],
            "genres": [],
            "tags": [],
        }

    except requests.exceptions.RequestException as e:
        if debug:
            print(f"  DEBUG: MusicBrainz query failed: {e}")
        return None
    except Exception as e:
        if debug:
            print(f"  DEBUG: Error parsing MusicBrainz response: {e}")
        return None


def normalize_artist_name(name: str) -> str:
    """
    Normalize artist name for better search matching.

    Removes accents, special characters, and common variations.
    """
    import unicodedata

    normalized = name

    # Normalize different types of hyphens/dashes to regular hyphen
    # U+2010 (HYPHEN), U+2011 (NON-BREAKING HYPHEN), U+2012 (FIGURE DASH),
    # U+2013 (EN DASH), U+2014 (EM DASH), U+2015 (HORIZONTAL BAR)
    hyphen_chars = ['\u2010', '\u2011', '\u2012', '\u2013', '\u2014', '\u2015']
    for hyphen in hyphen_chars:
        normalized = normalized.replace(hyphen, '-')

    # Normalize different types of apostrophes/quotes to regular apostrophe
    # U+2019 (RIGHT SINGLE QUOTATION MARK), U+02BC (MODIFIER LETTER APOSTROPHE)
    # U+2018 (LEFT SINGLE QUOTATION MARK), U+201B (SINGLE HIGH-REVERSED-9 QUOTATION MARK)
    apostrophe_chars = ['\u2019', '\u02BC', '\u2018', '\u201B']
    for apostrophe in apostrophe_chars:
        normalized = normalized.replace(apostrophe, "'")

    # Remove unicode accents
    normalized = unicodedata.normalize('NFD', normalized)
    normalized = ''.join(char for char in normalized if unicodedata.category(char) != 'Mn')

    # Common replacements
    normalized = normalized.replace('&', 'and')
    normalized = normalized.replace('/', ' ')

    return normalized


def artist_name_matches(result_name: str, search_name: str) -> bool:
    """
    Check if a result artist name matches the search name.

    Returns True if they match exactly or very closely (normalized).
    """
    # Normalize both names for comparison
    norm_result = normalize_artist_name(result_name).lower().strip()
    norm_search = normalize_artist_name(search_name).lower().strip()

    # Exact match
    if norm_result == norm_search:
        return True

    # Match without "The"
    if norm_result.startswith("the "):
        norm_result = norm_result[4:]
    if norm_search.startswith("the "):
        norm_search = norm_search[4:]

    return norm_result == norm_search


def filter_search_results(
    results: List[Dict[str, Any]], artist_name: str, debug: bool = False
) -> List[Dict[str, Any]]:
    """
    Filter search results to find artists that match the search name.

    Search results can contain both artists and albums. We need to:
    1. Extract the artist from each result
    2. Check if the artist name matches
    3. Return only matching results
    """
    filtered = []

    for result in results:
        # Extract artist from result (could be direct artist or nested in album)
        result_artist = None
        if "artist" in result:
            result_artist = result["artist"]
        elif "album" in result and isinstance(result["album"], dict):
            result_artist = result["album"].get("artist")
        elif "artistName" in result:
            result_artist = result

        if result_artist:
            result_artist_name = result_artist.get("artistName", "")
            if artist_name_matches(result_artist_name, artist_name):
                filtered.append(result)
                if debug:
                    print(f"  DEBUG: Matched '{result_artist_name}' to '{artist_name}'")
            elif debug:
                print(f"  DEBUG: Rejected '{result_artist_name}' (doesn't match '{artist_name}')")

    return filtered


def search_artist_in_lidarr(
    client: ArrClient, artist_name: str, debug: bool = False
) -> List[Dict[str, Any]]:
    """
    Search for an artist in Lidarr's database with fallback strategies.

    Tries multiple search approaches:
    1. Exact name from Spotify
    2. Normalized name (without accents/special chars)
    3. Name without leading "The"
    4. First word only (for multi-word names)

    Always filters results to find best match.
    """
    # Try exact search first
    results = client.get("/api/v1/search", params={"term": artist_name})
    if debug and not results:
        print(f"  DEBUG: No results for exact search: '{artist_name}'")
    if results:
        if debug:
            print(f"  DEBUG: Found {len(results)} results for '{artist_name}'")
        # Filter to find exact or close matches
        filtered = filter_search_results(results, artist_name, debug)
        if filtered:
            return filtered
        if debug:
            print("  DEBUG: No exact match in results, trying fallbacks")

    # Try normalized version (without accents, & -> and, etc.)
    normalized = normalize_artist_name(artist_name)
    if normalized != artist_name:
        results = client.get("/api/v1/search", params={"term": normalized})
        if debug and not results:
            print(f"  DEBUG: No results for normalized: '{normalized}'")
        if results:
            filtered = filter_search_results(results, artist_name, debug)
            if filtered:
                print(f"  Found using normalized name: '{normalized}'")
                return filtered

    # Try without leading "The"
    if artist_name.lower().startswith("the "):
        without_the = artist_name[4:]
        results = client.get("/api/v1/search", params={"term": without_the})
        if debug and not results:
            print(f"  DEBUG: No results without 'The': '{without_the}'")
        if results:
            filtered = filter_search_results(results, artist_name, debug)
            if filtered:
                print(f"  Found without 'The': '{without_the}'")
                return filtered

    # Last resort: Query MusicBrainz directly
    if debug:
        print("  DEBUG: Lidarr search failed, trying MusicBrainz directly")

    mb_artist = search_musicbrainz_artist(artist_name, debug=debug)
    if mb_artist:
        print(f"  Found in MusicBrainz: '{mb_artist['artistName']}'")
        # Return in the same format as Lidarr search results
        # Wrap in a result structure similar to what Lidarr returns
        return [{"artist": mb_artist}]

    if debug:
        print(f"  DEBUG: All search strategies failed for '{artist_name}'")
    return []


def monitor_artist_albums(
    client: ArrClient,
    artist_id: int,
    playlist_album_names: Set[str],
    search_albums: bool = False,
    debug: bool = False,
) -> tuple[int, int]:
    """
    Monitor specific albums for an artist in Lidarr.

    Args:
        client: Lidarr API client
        artist_id: Lidarr artist ID
        playlist_album_names: Set of album names from playlists
        search_albums: Whether to trigger album search after monitoring
        debug: Enable debug output

    Returns:
        Tuple of (matched_count, monitored_count)
    """
    # Get artist with albums
    artist = client.get(f"/api/v1/artist/{artist_id}")
    if not artist:
        if debug:
            print(f"    DEBUG: Could not fetch artist {artist_id}")
        return (0, 0)

    artist_name = artist.get("artistName", "Unknown")
    albums = artist.get("albums", [])

    # Debug: show artist stats
    if debug:
        monitored = artist.get("monitored", False)
        statistics = artist.get("statistics", {})
        album_count = statistics.get("albumCount", 0)
        print(f"    DEBUG: Artist monitored={monitored}, albumCount={album_count}, albums in response={len(albums)}")

    if not albums:
        if debug:
            print(f"    DEBUG: No albums found for {artist_name}")
        return (0, 0)

    matched_count = 0
    monitored_count = 0

    # Match playlist albums to Lidarr albums
    for album in albums:
        album_title = album.get("title", "")
        album_id = album.get("id")
        already_monitored = album.get("monitored", False)

        # Check if this album matches any in the playlists
        for playlist_album in playlist_album_names:
            # Normalize both for comparison
            if normalize_artist_name(album_title).lower() == normalize_artist_name(playlist_album).lower():
                matched_count += 1

                if not already_monitored:
                    if debug:
                        print(f"    Monitoring album: {album_title}")

                    # Update album to monitored
                    album["monitored"] = True
                    try:
                        client.put(f"/api/v1/album/{album_id}", album)
                        monitored_count += 1

                        # Optionally trigger search
                        if search_albums:
                            client.post("/api/v1/command", {
                                "name": "AlbumSearch",
                                "albumIds": [album_id]
                            })
                    except Exception as e:
                        if debug:
                            print(f"    DEBUG: Failed to monitor album {album_title}: {e}")
                else:
                    if debug:
                        print(f"    Album already monitored: {album_title}")

                break  # Found match, move to next album

    return (matched_count, monitored_count)


def get_existing_artists(client: ArrClient) -> tuple[Set[str], Set[str], Dict[str, int]]:
    """
    Get set of artist names and foreign IDs already in Lidarr.

    Returns:
        Tuple of (normalized artist names set, foreign artist IDs set, foreign_id -> lidarr_id mapping)
    """
    artists = client.get("/api/v1/artist")
    names = set()
    foreign_ids = set()
    foreign_to_lidarr_id = {}

    for artist in artists:
        # Store normalized name
        name = artist.get("artistName", "")
        if name:
            names.add(normalize_artist_name(name).lower())

        # Store foreign artist ID (MusicBrainz ID)
        foreign_id = artist.get("foreignArtistId")
        lidarr_id = artist.get("id")
        if foreign_id:
            foreign_ids.add(foreign_id)
            if lidarr_id:
                foreign_to_lidarr_id[foreign_id] = lidarr_id

    return names, foreign_ids, foreign_to_lidarr_id


def add_artist_to_lidarr(
    client: ArrClient,
    artist: Dict[str, Any],
    root_folder: str,
    quality_profile_id: int,
    metadata_profile_id: int,
    monitor: str = "all",
    debug: bool = False,
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
        debug: Enable debug output

    Returns:
        API response
    """
    # Check for suspicious fields in artist data
    if debug:
        suspicious_fields = ["folder", "path", "rootFolderPath"]
        found_suspicious = {k: artist.get(k) for k in suspicious_fields if k in artist}
        if found_suspicious:
            print(f"  DEBUG: Found fields in artist data: {found_suspicious}")

    # CRITICAL FIX: Remove the folder field from artist data before building payload
    # The folder field contains paths like 'library/ArtistName' which causes
    # Lidarr to append it to rootFolderPath, creating duplicates like:
    # /neo/music/library + library/Artist = /neo/music/library/library/Artist
    artist = dict(artist)  # Make a copy to avoid modifying the original
    artist.pop("folder", None)
    artist.pop("path", None)
    artist.pop("rootFolderPath", None)

    # Build payload with only writable fields from search result
    # IMPORTANT: Do not include 'folder' or 'path' fields as they can cause duplicate paths
    payload = {
        "artistName": artist.get("artistName"),
        "foreignArtistId": artist.get("foreignArtistId"),
        "qualityProfileId": quality_profile_id,
        "metadataProfileId": metadata_profile_id,
        "rootFolderPath": root_folder,
        "monitored": True,
        "albumFolder": True,
        "monitorNewItems": "all",
        # Include metadata from search
        "artistType": artist.get("artistType", ""),
        "disambiguation": artist.get("disambiguation", ""),
        "links": artist.get("links", []),
        "images": artist.get("images", []),
        "genres": artist.get("genres", []),
        "tags": artist.get("tags", []),
        # Add options
        "addOptions": {
            "monitor": monitor,
            "searchForMissingAlbums": False,
        },
    }

    if debug:
        print(f"  DEBUG: Sending rootFolderPath={root_folder}")
        # Verify no folder/path in final payload
        if "folder" in payload or "path" in payload:
            print("  ERROR: folder or path still in payload!")
        else:
            print("  DEBUG: Confirmed no folder/path in payload")

        # Show full payload for debugging
        print(f"  DEBUG: Full payload keys: {list(payload.keys())}")

    return client.post("/api/v1/artist", payload)


def try_add_single_artist(
    lidarr: ArrClient,
    artist_name: str,
    artist_data: Dict[str, Any],
    root_folder: str,
    quality_profile_id: int,
    metadata_profile_id: int,
    monitor: str,
    ctx: CommandContext,
    existing_foreign_ids: Set[str] = None,
    debug: bool = False,
) -> tuple[bool, str, Dict[str, Any] | None]:
    """
    Attempt to add a single artist to Lidarr.

    Args:
        lidarr: Lidarr API client
        artist_name: Artist name from Spotify
        artist_data: Artist data including albums
        root_folder: Root folder path
        quality_profile_id: Quality profile ID
        metadata_profile_id: Metadata profile ID
        monitor: Monitor mode
        ctx: Command context
        existing_foreign_ids: Set of foreign artist IDs already in Lidarr

    Returns:
        Tuple of (success: bool, error_message: str, lidarr_name: str | None)
    """
    if existing_foreign_ids is None:
        existing_foreign_ids = set()

    try:
        # Search for artist in Lidarr's MusicBrainz database
        search_results = search_artist_in_lidarr(lidarr, artist_name, debug=debug)

        if not search_results:
            return (False, "Not found in MusicBrainz", None)

        # Search returns both artists and albums - extract artist from first result
        first_result = search_results[0]

        # If result has 'artist' field, it's an album result - extract the artist
        if "artist" in first_result:
            artist_match = first_result["artist"]
        # If result has 'album' field with nested artist
        elif "album" in first_result and isinstance(first_result["album"], dict):
            album = first_result["album"]
            if "artist" in album:
                artist_match = album["artist"]
            else:
                return (False, f"Album result has no artist field: {list(album.keys())}", None)
        # If result has 'artistName', it's already an artist result
        elif "artistName" in first_result:
            artist_match = first_result
        else:
            # Unknown result type
            return (False, f"Unexpected search result format: {list(first_result.keys())}", None)

        artist_mb_name = artist_match.get("artistName", artist_name)
        foreign_artist_id = artist_match.get("foreignArtistId")

        # Check if artist is already in Lidarr by foreign ID
        if foreign_artist_id and foreign_artist_id in existing_foreign_ids:
            print(f"  Found: {artist_mb_name}")
            print("  Already in Lidarr (detected by MusicBrainz ID)")
            return (False, "Already exists in Lidarr", artist_mb_name)

        print(f"  Found: {artist_mb_name}")
        print(f"  Albums in playlists: {len(artist_data['albums'])}")
        for album in list(artist_data["albums"])[:3]:
            print(f"    - {album}")
        if len(artist_data["albums"]) > 3:
            print(f"    ... and {len(artist_data['albums']) - 3} more albums")

        if not ctx.dry_run:
            result = add_artist_to_lidarr(
                lidarr,
                artist_match,
                root_folder,
                quality_profile_id,
                metadata_profile_id,
                monitor,
                debug=debug,
            )

            if result and result.get("id"):
                actual_path = result.get("path", "unknown")
                print(f"  ✓ Added successfully (ID: {result['id']})")

                if debug:
                    print(f"  DEBUG: Lidarr assigned path: {actual_path}")

                # Check if Lidarr created a duplicate path
                if "library/library" in actual_path:
                    print("  ⚠️  WARNING: Lidarr created duplicate path!")
                    print(f"     We sent rootFolderPath={root_folder}")
                    print(f"     Lidarr created: {actual_path}")

                return (True, "", artist_mb_name)
            else:
                return (False, "Failed to add artist (no ID returned)", artist_mb_name)
        else:
            print("  [DRY RUN] Would add this artist")
            return (True, "", artist_mb_name)

    except Exception as e:
        return (False, f"Error: {str(e)}", None)


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
    all_playlists: bool,
    debug: bool = False,
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

        if all_playlists:
            # Select all playlists automatically
            selected_ids = [p["id"] for p in user_playlists]
            print(f"Selecting all {len(selected_ids)} playlists\n")
        else:
            # Interactive selection with fzf
            print(
                "Use fzf to select playlists (TAB to select, "
                "ENTER to confirm, ESC to cancel)"
            )

            selected_ids = select_with_fzf(
                user_playlists, "{name} (by {owner}, {tracks_total} tracks)"
            )

            if not selected_ids:
                print("\nNo playlists selected. Exiting.")
                return

            print(f"\nSelected {len(selected_ids)} playlist(s)\n")

        playlist_ids = selected_ids
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

    # Check Lidarr's naming configuration (only in debug mode)
    if debug:
        print("\nChecking Lidarr naming configuration...")
        try:
            naming_config = lidarr.get("/api/v1/config/naming")
            artist_folder_format = naming_config.get("artistFolderFormat", "Not set")
            print(f"  Artist Folder Format: {artist_folder_format}")
        except Exception as e:
            print(f"  Could not fetch naming config: {e}")

    # Use specified root folder or auto-detect from Lidarr
    if root_folder and root_folder != "/music":  # /music is the default from CLI
        if debug:
            print(f"\nUsing user-specified root folder: {root_folder}")
    else:
        root_folder = get_root_folder_path(lidarr)
        if debug:
            print(f"\nAuto-detected root folder from Lidarr: {root_folder}")

    print("\nUsing Lidarr configuration:")
    print(f"  Quality Profile ID: {quality_profile_id}")
    print(f"  Metadata Profile ID: {metadata_profile_id}")
    print(f"  Root Folder: {root_folder}")

    # Verify no duplicate segments in root folder
    _, has_duplicate, duplicate_seg = strip_duplicate_path_segments(root_folder)
    if has_duplicate:
        print(f"  WARNING: Root folder has duplicate segment '{duplicate_seg}'!")
        print("  This should have been cleaned by get_root_folder_path()")

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
    existing_names, existing_foreign_ids, foreign_to_lidarr_id = get_existing_artists(lidarr)
    print(f"Found {len(existing_names)} artists already in Lidarr")

    # Separate artists into existing and missing
    artists_to_add = []
    artists_already_in_lidarr = []

    for artist_name, artist_data in all_artists.items():
        # Check normalized name
        normalized_name = normalize_artist_name(artist_name).lower()
        if normalized_name in existing_names:
            artists_already_in_lidarr.append(artist_name)
        else:
            artists_to_add.append((artist_name, artist_data))

    # Print summary
    print_section_header("SUMMARY")
    print_item_list(artists_already_in_lidarr, "Already in Lidarr")

    if artists_to_add:
        print(f"\n→ Artists to add: {len(artists_to_add)}")
        if dry_run:
            # In dry-run mode, show all artists for manual addition
            for artist_name, _ in artists_to_add:
                print(f"  - {artist_name}")
        else:
            # In normal mode, show first 10 to avoid clutter
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

    # Add artists to Lidarr with retry mechanism
    print_section_header("ADDING ARTISTS TO LIDARR")
    print(
        f"Note: Adding {len(artists_to_add)} artists with delays "
        "to avoid overwhelming Lidarr..."
    )

    added_count = 0
    dry_run_artists = []  # Track artists for dry-run output
    failed_artists = []  # Track failed artists for retry
    max_retries = 2

    # First pass: try adding all artists
    for idx, (artist_name, artist_data) in enumerate(artists_to_add, 1):
        print(f"\n[{idx}/{len(artists_to_add)}] Searching for: {artist_name}")

        success, error_msg, lidarr_name = try_add_single_artist(
            lidarr,
            artist_name,
            artist_data,
            root_folder,
            quality_profile_id,
            metadata_profile_id,
            monitor,
            ctx,
            existing_foreign_ids,
            debug,
        )

        if success:
            if ctx.dry_run:
                dry_run_artists.append(
                    {
                        "spotify_name": artist_name,
                        "lidarr_name": lidarr_name or artist_name,
                        "albums": artist_data["albums"],
                    }
                )
            added_count += 1
        else:
            print(f"  ✗ {error_msg}")
            failed_artists.append((artist_name, artist_data, error_msg))

        # Add delay between requests
        if idx < len(artists_to_add):
            delay = request_delay if not ctx.dry_run else 0.5
            time.sleep(delay)

    # Retry failed artists (up to max_retries times)
    retry_round = 1
    while failed_artists and retry_round <= max_retries and not ctx.dry_run:
        print_section_header(f"RETRY ROUND {retry_round}")
        print(f"Retrying {len(failed_artists)} failed artist(s)...")

        still_failed = []

        for idx, (artist_name, artist_data, prev_error) in enumerate(
            failed_artists, 1
        ):
            print(
                f"\n[Retry {retry_round}/{max_retries}] "
                f"[{idx}/{len(failed_artists)}] {artist_name}"
            )
            print(f"  Previous error: {prev_error}")

            success, error_msg, lidarr_name = try_add_single_artist(
                lidarr,
                artist_name,
                artist_data,
                root_folder,
                quality_profile_id,
                metadata_profile_id,
                monitor,
                ctx,
                existing_foreign_ids,
                debug,
            )

            if success:
                added_count += 1
                print("  ✓ Succeeded on retry!")
            else:
                print(f"  ✗ Still failing: {error_msg}")
                still_failed.append((artist_name, artist_data, error_msg))

            # Add delay between retry requests
            if idx < len(failed_artists):
                time.sleep(request_delay)

        failed_artists = still_failed
        retry_round += 1

    failed_count = len(failed_artists)

    # Monitor albums from playlists for ALL artists in Lidarr
    if not ctx.dry_run:
        print_section_header("MONITORING PLAYLIST ALBUMS")
        print("Checking which albums from playlists should be monitored...")
        print("\nNote: Newly added artists may take a few moments for Lidarr to")
        print("      fetch their discography from MusicBrainz. If albums are")
        print("      missing, try running the sync again in a minute or trigger")
        print("      a 'Refresh Artist' in Lidarr's UI.\n")

        # Collect artists to monitor (newly added + already existing)
        artists_to_monitor = []

        # Add newly added artists (need to fetch their IDs)
        # For now, we'll fetch all artists again to get IDs
        print("Fetching updated artist list...")
        _, _, updated_foreign_to_lidarr_id = get_existing_artists(lidarr)

        # Process all artists that have albums in playlists
        for artist_name, artist_data in all_artists.items():
            if not artist_data.get("albums"):
                continue

            # Try to find this artist in Lidarr by checking if we have their foreign ID
            # We need to search for them to get the foreign ID
            search_results = search_artist_in_lidarr(lidarr, artist_name, debug=False)
            if not search_results:
                continue

            # Extract artist from search result
            first_result = search_results[0]
            artist_match = None
            if "artist" in first_result:
                artist_match = first_result["artist"]
            elif "album" in first_result and isinstance(first_result["album"], dict):
                artist_match = first_result["album"].get("artist")
            elif "artistName" in first_result:
                artist_match = first_result

            if not artist_match:
                continue

            foreign_id = artist_match.get("foreignArtistId")
            if foreign_id and foreign_id in updated_foreign_to_lidarr_id:
                lidarr_id = updated_foreign_to_lidarr_id[foreign_id]
                artists_to_monitor.append((artist_name, lidarr_id, artist_data["albums"]))

        if not artists_to_monitor:
            print("No artists with playlist albums found in Lidarr")
        else:
            print(f"Processing {len(artists_to_monitor)} artists...")
            total_matched = 0
            total_monitored = 0
            artists_needing_refresh = []

            for artist_name, lidarr_id, playlist_albums in artists_to_monitor:
                print(f"\n  {artist_name}:")
                matched, monitored = monitor_artist_albums(
                    lidarr,
                    lidarr_id,
                    playlist_albums,
                    search_albums=False,  # Don't auto-search for now
                    debug=debug
                )
                total_matched += matched
                total_monitored += monitored

                if matched == 0:
                    # Check if artist has no albums at all (might need refresh)
                    artist_data = lidarr.get(f"/api/v1/artist/{lidarr_id}")
                    if artist_data and not artist_data.get("albums"):
                        artists_needing_refresh.append((artist_name, lidarr_id))
                        print("    No albums found - may need refresh")
                    else:
                        print("    No matching albums found in Lidarr")
                elif monitored == 0:
                    print(f"    {matched} album(s) already monitored")

            print(f"\n  Total albums matched: {total_matched}")
            print(f"  Total albums newly monitored: {total_monitored}")

            # Offer to refresh artists with no albums
            if artists_needing_refresh:
                print(f"\n  {len(artists_needing_refresh)} artist(s) have no albums and may need refresh:")
                for name, _ in artists_needing_refresh[:5]:
                    print(f"    - {name}")
                if len(artists_needing_refresh) > 5:
                    print(f"    ... and {len(artists_needing_refresh) - 5} more")

                if not ctx.no_confirm:
                    response = input("\n  Trigger refresh for these artists? (y/n): ").lower().strip()
                    if response in ["y", "yes"]:
                        print("\n  Triggering refresh...")
                        for artist_name, artist_id in artists_needing_refresh:
                            print(f"    Refreshing {artist_name}...", end=" ")
                            try:
                                lidarr.post("/api/v1/command", {
                                    "name": "RefreshArtist",
                                    "artistId": artist_id
                                })
                                print("✓")
                            except Exception as e:
                                print(f"✗ ({e})")
                        print("\n  Refresh commands sent. Wait a moment and run sync again")
                        print("  to monitor albums from playlists.")

    # Final summary
    print_section_header("FINAL SUMMARY")
    print(f"\nTotal playlists processed: {len(playlist_info)}")
    print(f"Total unique artists found: {len(all_artists)}")
    print(f"Artists already in Lidarr: {len(artists_already_in_lidarr)}")
    print(f"Artists to add: {len(artists_to_add)}")
    print(f"  - Successfully added: {added_count}")
    print(f"  - Failed after {max_retries} retries: {failed_count}")

    # Show permanently failed artists with error details
    if failed_artists and not ctx.dry_run:
        print_section_header("PERMANENTLY FAILED ARTISTS")
        print(
            f"\nThe following {len(failed_artists)} artist(s) could not be "
            f"added after {max_retries} retry attempts:\n"
        )
        for artist_name, artist_data, error_msg in failed_artists:
            print(f"  • {artist_name}")
            print(f"    Error: {error_msg}")
            if artist_data.get("albums"):
                album_count = len(artist_data["albums"])
                print(f"    Albums in playlists: {album_count}")
        print(
            "\nTip: These failures may be due to:\n"
            "  - Artist not found in MusicBrainz\n"
            "  - Temporary API issues (try running again later)\n"
            "  - Network connectivity problems"
        )

    if ctx.dry_run:
        print(
            "\n[DRY RUN] No changes were made. "
            "Remove --dry-run to add artists."
        )
        if dry_run_artists:
            print_section_header("ARTISTS TO ADD MANUALLY")
            print(
                "\nCopy the artist names below to search and add them "
                "manually in Lidarr:\n"
            )
            for artist in dry_run_artists:
                print(f"• {artist['lidarr_name']}")
                if artist["spotify_name"] != artist["lidarr_name"]:
                    print(f"  (Spotify: {artist['spotify_name']})")
                if artist["albums"]:
                    album_count = len(artist["albums"])
                    print(f"  Albums in playlists: {album_count}")
            print(
                f"\n\nTotal: {len(dry_run_artists)} artists to add manually"
            )
    elif added_count > 0:
        print(
            f"\nMonitoring mode: {monitor}\n"
            "New artists will start searching for albums based on "
            "your Lidarr settings."
        )
