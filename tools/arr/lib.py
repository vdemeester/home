#!/usr/bin/env python3
"""
Shared library for *arr (Sonarr, Radarr, Lidarr) automation scripts.

Provides common functionality for API interaction, user confirmation,
and output formatting across all *arr stack scripts.
"""

import subprocess
import sys
import time
from typing import Any, Dict, List, Optional

import requests


class ArrClient:
    """Base client for *arr API interactions."""

    def __init__(self, base_url: str, api_key: str):
        """
        Initialize the *arr API client.

        Args:
            base_url: Base URL of the *arr service
                      (e.g., http://localhost:8989)
            api_key: API key for authentication
        """
        self.base_url = base_url.rstrip("/")
        self.api_key = api_key
        self.headers = {"X-Api-Key": api_key}

    def get(
        self,
        endpoint: str,
        params: Optional[Dict[str, Any]] = None,
        max_retries: int = 3,
        retry_delay: float = 2.0,
    ) -> List[Dict[str, Any]] | Dict[str, Any]:
        """
        Make a GET request to the *arr API with retry logic.

        Args:
            endpoint: API endpoint path (e.g., /api/v3/series)
            params: Optional query parameters
            max_retries: Maximum number of retry attempts
            retry_delay: Initial delay between retries (seconds)

        Returns:
            JSON response data

        Raises:
            SystemExit: If the request fails after all retries
        """
        url = f"{self.base_url}{endpoint}"

        for attempt in range(max_retries):
            try:
                response = requests.get(
                    url, headers=self.headers, params=params, timeout=30
                )
                response.raise_for_status()
                return response.json()
            except requests.exceptions.HTTPError as e:
                status_code = e.response.status_code if e.response else None

                # Retry on server errors (5xx) or rate limiting (429)
                if status_code in [429, 500, 502, 503, 504]:
                    if attempt < max_retries - 1:
                        wait_time = retry_delay * (2**attempt)
                        print(
                            f"  Server error ({status_code}), "
                            f"retrying in {wait_time}s... "
                            f"(attempt {attempt + 1}/{max_retries})"
                        )
                        time.sleep(wait_time)
                        continue

                # Don't retry on client errors (4xx except 429)
                print(
                    f"Error fetching from {endpoint}: HTTP {status_code}",
                    file=sys.stderr,
                )
                if params:
                    print(f"  Params: {params}", file=sys.stderr)
                if e.response:
                    try:
                        error_detail = e.response.json()
                        print(f"  Detail: {error_detail}", file=sys.stderr)
                    except Exception:
                        print(
                            f"  Response: {e.response.text[:200]}",
                            file=sys.stderr,
                        )
                sys.exit(1)
            except requests.exceptions.Timeout:
                if attempt < max_retries - 1:
                    wait_time = retry_delay * (2**attempt)
                    print(
                        f"  Request timeout, retrying in {wait_time}s... "
                        f"(attempt {attempt + 1}/{max_retries})"
                    )
                    time.sleep(wait_time)
                    continue
                print(f"Error: Request timeout on {endpoint}", file=sys.stderr)
                sys.exit(1)
            except requests.exceptions.RequestException as e:
                print(f"Error fetching from {endpoint}: {e}", file=sys.stderr)
                if params:
                    print(f"  Params: {params}", file=sys.stderr)
                sys.exit(1)

        # Should not reach here, but just in case
        print(f"Error: Failed after {max_retries} attempts", file=sys.stderr)
        sys.exit(1)

    def post(
        self,
        endpoint: str,
        payload: Dict[str, Any],
        max_retries: int = 3,
        retry_delay: float = 2.0,
    ) -> Dict[str, Any]:
        """
        Make a POST request to the *arr API with retry logic.

        Args:
            endpoint: API endpoint path (e.g., /api/v3/command)
            payload: JSON payload to send
            max_retries: Maximum number of retry attempts
            retry_delay: Initial delay between retries (seconds)

        Returns:
            JSON response data (empty dict on failure)
        """
        url = f"{self.base_url}{endpoint}"
        headers = {**self.headers, "Content-Type": "application/json"}

        for attempt in range(max_retries):
            try:
                response = requests.post(
                    url, headers=headers, json=payload, timeout=30
                )
                response.raise_for_status()
                return response.json()
            except requests.exceptions.HTTPError as e:
                status_code = e.response.status_code if e.response else None

                # Retry on server errors (5xx) or rate limiting (429)
                if status_code in [429, 500, 502, 503, 504]:
                    if attempt < max_retries - 1:
                        wait_time = retry_delay * (2**attempt)
                        print(
                            f"  Server error ({status_code}), "
                            f"retrying in {wait_time}s... "
                            f"(attempt {attempt + 1}/{max_retries})"
                        )
                        time.sleep(wait_time)
                        continue

                # Better error reporting
                if status_code:
                    print(
                        f"Error posting to {endpoint}: HTTP {status_code}",
                        file=sys.stderr,
                    )
                else:
                    print(
                        f"Error posting to {endpoint}: {type(e).__name__} - {str(e)}",
                        file=sys.stderr,
                    )

                # Print payload for debugging
                print(f"  Payload: {payload}", file=sys.stderr)

                # Always try to get response details
                if e.response is not None:
                    print(
                        f"  Response status: {e.response.status_code}",
                        file=sys.stderr,
                    )
                    print(
                        f"  Response headers: {dict(e.response.headers)}",
                        file=sys.stderr,
                    )
                    try:
                        error_detail = e.response.json()
                        print(
                            f"  Response JSON: {error_detail}", file=sys.stderr
                        )
                    except Exception:
                        print(
                            f"  Response text: {e.response.text}",
                            file=sys.stderr,
                        )
                else:
                    print("  No response object available", file=sys.stderr)
                return {}
            except requests.exceptions.Timeout:
                if attempt < max_retries - 1:
                    wait_time = retry_delay * (2**attempt)
                    print(
                        f"  Request timeout, retrying in {wait_time}s... "
                        f"(attempt {attempt + 1}/{max_retries})"
                    )
                    time.sleep(wait_time)
                    continue
                print(f"Error: Request timeout on {endpoint}", file=sys.stderr)
                return {}
            except requests.exceptions.RequestException as e:
                print(f"Error posting to {endpoint}: {e}", file=sys.stderr)
                return {}

        return {}

    def put(
        self,
        endpoint: str,
        payload: Dict[str, Any],
        max_retries: int = 3,
        retry_delay: float = 2.0,
    ) -> Dict[str, Any]:
        """
        Make a PUT request to the *arr API with retry logic.

        Args:
            endpoint: API endpoint path (e.g., /api/v3/series)
            payload: JSON payload to send
            max_retries: Maximum number of retry attempts
            retry_delay: Initial delay between retries (seconds)

        Returns:
            JSON response data (empty dict on failure)
        """
        url = f"{self.base_url}{endpoint}"
        headers = {**self.headers, "Content-Type": "application/json"}

        for attempt in range(max_retries):
            try:
                response = requests.put(
                    url, headers=headers, json=payload, timeout=30
                )
                response.raise_for_status()
                return response.json()
            except requests.exceptions.HTTPError as e:
                status_code = e.response.status_code if e.response else None

                # Retry on server errors (5xx) or rate limiting (429)
                if status_code in [429, 500, 502, 503, 504]:
                    if attempt < max_retries - 1:
                        wait_time = retry_delay * (2**attempt)
                        print(
                            f"  Server error ({status_code}), "
                            f"retrying in {wait_time}s... "
                            f"(attempt {attempt + 1}/{max_retries})"
                        )
                        time.sleep(wait_time)
                        continue

                print(
                    f"Error putting to {endpoint}: HTTP {status_code}",
                    file=sys.stderr,
                )
                if e.response:
                    try:
                        error_detail = e.response.json()
                        print(f"  Detail: {error_detail}", file=sys.stderr)
                    except Exception:
                        print(
                            f"  Response: {e.response.text[:200]}",
                            file=sys.stderr,
                        )
                return {}
            except requests.exceptions.Timeout:
                if attempt < max_retries - 1:
                    wait_time = retry_delay * (2**attempt)
                    print(
                        f"  Request timeout, retrying in {wait_time}s... "
                        f"(attempt {attempt + 1}/{max_retries})"
                    )
                    time.sleep(wait_time)
                    continue
                print(f"Error: Request timeout on {endpoint}", file=sys.stderr)
                return {}
            except requests.exceptions.RequestException as e:
                print(f"Error putting to {endpoint}: {e}", file=sys.stderr)
                return {}

        return {}


def ask_confirmation(prompt: str) -> bool:
    """
    Ask user for yes/no confirmation.

    Args:
        prompt: Question to ask the user

    Returns:
        True if user confirms (y/yes), False otherwise (n/no)
    """
    while True:
        response = input(f"{prompt} (y/n): ").lower().strip()
        if response in ["y", "yes"]:
            return True
        elif response in ["n", "no"]:
            return False
        else:
            print("Please answer 'y' or 'n'")


class CommandContext:
    """Context object for command execution with common options."""

    def __init__(self, dry_run: bool = False, no_confirm: bool = False):
        """
        Initialize command context.

        Args:
            dry_run: If True, show changes without applying them
            no_confirm: If True, skip interactive confirmations
        """
        self.dry_run = dry_run
        self.no_confirm = no_confirm


def print_separator(char: str = "=", width: int = 80) -> None:
    """Print a separator line."""
    print(char * width)


def print_section_header(title: str) -> None:
    """Print a section header with separators."""
    print("\n" + "=" * 80)
    print(title)
    print("=" * 80)


def print_item_list(
    items: List[str], prefix: str, max_display: int = 5
) -> None:
    """
    Print a list of items with optional truncation.

    Args:
        items: List of item names to display
        prefix: Prefix message to show before the list
        max_display: Maximum number of items to show before truncating
    """
    if not items:
        return

    count = len(items)
    print(f"\n{prefix} ({count} items):")
    for item in items[:max_display]:
        print(f"  - {item}")
    if len(items) > max_display:
        remaining = len(items) - max_display
        print(f"  ... and {remaining} more")


def get_confirmation_decision(ctx: CommandContext, prompt: str) -> bool:
    """
    Determine whether to proceed based on dry-run, no-confirm, or user input.

    Args:
        ctx: Command context with dry_run and no_confirm flags
        prompt: Confirmation prompt to show user

    Returns:
        True if should proceed, False otherwise
    """
    if ctx.dry_run:
        print("\n[DRY RUN] Skipping actual operation")
        return False
    elif ctx.no_confirm:
        print("\n[NO CONFIRM] Proceeding with operation...")
        return True
    else:
        return ask_confirmation(prompt)


def print_final_summary(
    total: int,
    processed: int,
    skipped: int,
    operation: str,
    queue_note: bool = True,
) -> None:
    """
    Print final summary of operations.

    Args:
        total: Total items that needed processing
        processed: Number of items successfully processed
        skipped: Number of items skipped
        operation: Name of the operation (e.g., "Renamed", "Retagged")
        queue_note: Whether to show the queue check note
    """
    print_section_header("FINAL SUMMARY")
    print(f"\nItems processed: {total}")
    print(f"  - {operation}: {processed}")
    print(f"  - Skipped: {skipped}")

    if processed > 0 and queue_note:
        print(
            f"\nNote: {operation} operations are queued. "
            "Check the service's queue for progress."
        )


def select_with_fzf(
    items: List[Dict[str, str]], display_format: str, multi: bool = True
) -> List[str]:
    """
    Use fzf to interactively select items.

    Args:
        items: List of dictionaries containing item data
        display_format: Format string for displaying items (e.g.,
                       "{name} ({owner}, {tracks_total} tracks)")
        multi: Allow multiple selection if True

    Returns:
        List of selected item IDs (empty list if cancelled)
    """
    if not items:
        return []

    # Create lookup table: display text -> item id
    lookup = {}
    lines = []
    for item in items:
        display = display_format.format(**item)
        lines.append(display)
        lookup[display] = item.get("id")

    # Prepare fzf input
    fzf_input = "\n".join(lines)

    # Run fzf
    fzf_args = ["fzf", "--ansi", "--prompt=Select playlists: "]
    if multi:
        fzf_args.append("--multi")

    try:
        result = subprocess.run(
            fzf_args,
            input=fzf_input,
            text=True,
            capture_output=True,
            check=True,
        )
        # Parse selected lines
        selected_lines = result.stdout.strip().split("\n")
        return [lookup[line] for line in selected_lines if line in lookup]
    except subprocess.CalledProcessError:
        # User cancelled or fzf not found
        return []
    except FileNotFoundError:
        print("Error: fzf not found. Please install fzf:", file=sys.stderr)
        print("  On NixOS: nix-env -iA nixpkgs.fzf", file=sys.stderr)
        print("  On other systems: see https://github.com/junegunn/fzf")
        sys.exit(1)


class JellyfinClient:
    """Client for Jellyfin API interactions."""

    def __init__(
        self, base_url: str, api_token: str, user_id: str, debug: bool = False
    ):
        """
        Initialize the Jellyfin API client.

        Args:
            base_url: Base URL of the Jellyfin service
                      (e.g., http://localhost:8096)
            api_token: API token for authentication
            user_id: User ID or username for playlist ownership
            debug: Enable debug output
        """
        self.base_url = base_url.rstrip("/")
        self.api_token = api_token
        self.debug = debug
        self.headers = {
            "Authorization": f'MediaBrowser Token="{api_token}"',
            "Content-Type": "application/json",
        }

        # Resolve username to user ID if needed
        self.user_id = self._resolve_user_id(user_id)

    def _resolve_user_id(self, user_identifier: str) -> str:
        """
        Resolve a username or user ID to a proper user ID.

        If the identifier looks like a GUID, use it as-is.
        Otherwise, look up the user by username.

        Args:
            user_identifier: Username or user ID

        Returns:
            Resolved user ID (GUID)
        """
        # Check if it's already a GUID (basic check for 8-4-4-4-12 format)
        if len(user_identifier) == 32 or (
            len(user_identifier) == 36 and user_identifier.count("-") == 4
        ):
            return user_identifier

        # Otherwise, look up by username
        try:
            response = self.get("/Users")
            if isinstance(response, list):
                for user in response:
                    user_name = user.get("Name", "").lower()
                    if user_name == user_identifier.lower():
                        return user.get("Id")
        except Exception:
            # If lookup fails, return the original identifier
            # and let subsequent API calls fail with a clearer error
            pass

        # If not found, return original (might be unrecognized ID)
        return user_identifier

    def get(
        self, endpoint: str, params: Optional[Dict[str, Any]] = None
    ) -> List[Dict[str, Any]] | Dict[str, Any]:
        """
        Make a GET request to the Jellyfin API.

        Args:
            endpoint: API endpoint path (e.g., /Items)
            params: Optional query parameters

        Returns:
            JSON response data

        Raises:
            SystemExit: If the request fails
        """
        url = f"{self.base_url}{endpoint}"
        try:
            response = requests.get(
                url, headers=self.headers, params=params, timeout=30
            )
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(
                f"Error fetching from Jellyfin {endpoint}: {e}",
                file=sys.stderr,
            )
            sys.exit(1)

    def post(self, endpoint: str, payload: Dict[str, Any]) -> Dict[str, Any]:
        """
        Make a POST request to the Jellyfin API.

        Args:
            endpoint: API endpoint path (e.g., /Playlists)
            payload: JSON payload to send

        Returns:
            JSON response data

        Raises:
            SystemExit: If the request fails
        """
        url = f"{self.base_url}{endpoint}"
        try:
            response = requests.post(
                url, headers=self.headers, json=payload, timeout=30
            )
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(
                f"Error posting to Jellyfin {endpoint}: {e}",
                file=sys.stderr,
            )
            if hasattr(e, "response") and e.response is not None:
                try:
                    error_detail = e.response.json()
                    print(f"  Detail: {error_detail}", file=sys.stderr)
                except Exception:
                    print(
                        f"  Response: {e.response.text[:200]}",
                        file=sys.stderr,
                    )
            sys.exit(1)

    def search_tracks(
        self,
        query: str,
        limit: int = 50,
        artist_name: str = None,
        track_name: str = None,
    ) -> List[Dict[str, Any]]:
        """
        Search for tracks in Jellyfin library.

        Args:
            query: Legacy search query string (for backward compatibility)
            limit: Maximum number of results
            artist_name: Artist name to search by (preferred)
            track_name: Track name to search by

        Returns:
            List of track items
        """
        # Prefer searching by artist name when available
        if artist_name:
            # Find artist using NameStartsWith (more reliable)
            artist_words = artist_name.split()
            artist_first_word = (
                artist_words[0] if artist_words else artist_name
            )

            artist_params = {
                "NameStartsWith": artist_first_word,
                "IncludeItemTypes": "MusicArtist",
                "Limit": 50,
                "Recursive": True,
            }
            artist_result = self.get(
                f"/Users/{self.user_id}/Items", params=artist_params
            )

            artists = artist_result.get("Items", [])

            if self.debug:
                print(
                    f"DEBUG: Searching for artist starting with "
                    f"'{artist_first_word}' - Found {len(artists)} artists"
                )
                if artists:
                    for idx, artist in enumerate(artists[:5], 1):
                        print(f"  {idx}. {artist.get('Name')}")

            if artists:
                # Find exact or best match
                artist_name_lower = artist_name.lower()
                matched_artist = None

                for artist in artists:
                    if artist.get("Name", "").lower() == artist_name_lower:
                        matched_artist = artist
                        break

                # If no exact match, use first result
                if not matched_artist:
                    matched_artist = artists[0]

                artist_id = matched_artist.get("Id")

                if self.debug:
                    artist_name_str = matched_artist.get("Name")
                    print(
                        f"DEBUG: Using artist: {artist_name_str} "
                        f"(ID: {artist_id})"
                    )

                track_params = {
                    "ArtistIds": artist_id,
                    "IncludeItemTypes": "Audio",
                    "Recursive": True,
                    "Limit": limit,
                    "Fields": (
                        "Artists,Album,AlbumArtist,AlbumArtists,ArtistItems"
                    ),
                }
                result = self.get(
                    f"/Users/{self.user_id}/Items", params=track_params
                )
                items = result.get("Items", [])

                if self.debug:
                    print(f"DEBUG: Found {len(items)} tracks by this artist")

                return items

        # Fallback: search by track name
        search_term = track_name or query
        words = search_term.split()
        first_word = words[0] if words else search_term

        params = {
            "NameStartsWith": first_word,
            "IncludeItemTypes": "Audio",
            "Recursive": True,
            "Limit": 200,  # Larger limit: filtering client-side
            "Fields": ("Artists,Album,AlbumArtist,AlbumArtists,ArtistItems"),
            "EnableUserData": False,  # Skip user data for speed
        }

        result = self.get(f"/Users/{self.user_id}/Items", params=params)

        items = result.get("Items", [])

        # Enrich items with album artist data if track artists are missing
        for item in items:
            if not item.get("Artists") and item.get("AlbumId"):
                try:
                    album_id = item["AlbumId"]
                    album = self.get(
                        f"/Users/{self.user_id}/Items/{album_id}",
                        params={"Fields": "Artists,AlbumArtists"},
                    )
                    # Use album artists as track artists
                    item["Artists"] = album.get("AlbumArtists", [])
                    item["Album"] = album.get("Name", "")
                except Exception:
                    pass  # Continue even if album fetch fails

        return items

    def get_playlists(self) -> List[Dict[str, Any]]:
        """
        Get all playlists for the user.

        Returns:
            List of playlist items
        """
        params = {
            "IncludeItemTypes": "Playlist",
            "Recursive": "true",
        }
        result = self.get(f"/Users/{self.user_id}/Items", params=params)
        return result.get("Items", [])

    def create_playlist(
        self, name: str, item_ids: List[str], is_public: bool = False
    ) -> Dict[str, Any]:
        """
        Create a new playlist in Jellyfin.

        Args:
            name: Playlist name
            item_ids: List of Jellyfin item IDs to add
            is_public: Whether the playlist is public

        Returns:
            Created playlist data
        """
        payload = {
            "Name": name,
            "Ids": item_ids,
            "UserId": self.user_id,
            "IsPublic": is_public,
        }
        return self.post("/Playlists", payload)

    def add_to_playlist(
        self, playlist_id: str, item_ids: List[str]
    ) -> Dict[str, Any]:
        """
        Add items to an existing playlist.

        Args:
            playlist_id: Jellyfin playlist ID
            item_ids: List of item IDs to add

        Returns:
            Response data
        """
        params = {"ids": ",".join(item_ids), "userId": self.user_id}
        url = f"{self.base_url}/Playlists/{playlist_id}/Items"
        try:
            response = requests.post(
                url, headers=self.headers, params=params, timeout=30
            )
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(
                f"Error adding items to playlist: {e}",
                file=sys.stderr,
            )
            return {}

    def get_playlist_items(self, playlist_id: str) -> List[str]:
        """
        Get all item IDs in a playlist.

        Args:
            playlist_id: Jellyfin playlist ID

        Returns:
            List of item IDs in the playlist
        """
        params = {
            "ParentId": playlist_id,
            "Fields": "Id",
        }
        result = self.get(f"/Users/{self.user_id}/Items", params=params)
        items = result.get("Items", [])
        return [item.get("Id") for item in items if item.get("Id")]

    def clear_playlist(self, playlist_id: str) -> bool:
        """
        Remove all items from a playlist.

        Args:
            playlist_id: Jellyfin playlist ID

        Returns:
            True if successful
        """
        # Get current items
        item_ids = self.get_playlist_items(playlist_id)

        if not item_ids:
            return True  # Already empty

        # Remove all items
        try:
            url = (
                f"{self.base_url}/Playlists/{playlist_id}/Items"
                f"?EntryIds={','.join(item_ids)}"
            )
            response = requests.delete(url, headers=self.headers, timeout=30)
            response.raise_for_status()
            return True
        except requests.exceptions.RequestException as e:
            print(
                f"Error clearing playlist: {e}",
                file=sys.stderr,
            )
            return False


class SpotifyClient:
    """Client for Spotify API interactions using client credentials flow."""

    def __init__(self, client_id: str, client_secret: str):
        """
        Initialize the Spotify API client with client credentials.

        This uses the client credentials flow which can access public
        playlists but not private user data.

        Args:
            client_id: Spotify application client ID
            client_secret: Spotify application client secret
        """
        try:
            import spotipy
            from spotipy.oauth2 import SpotifyClientCredentials
        except ImportError:
            print(
                "Error: spotipy library not found. Install it with:",
                file=sys.stderr,
            )
            print("  pip install spotipy", file=sys.stderr)
            sys.exit(1)

        # Use client credentials flow (no OAuth required)
        auth_manager = SpotifyClientCredentials(
            client_id=client_id, client_secret=client_secret
        )
        self.sp = spotipy.Spotify(auth_manager=auth_manager)

    def get_playlist_tracks(self, playlist_id: str) -> List[Dict[str, Any]]:
        """
        Fetch all tracks from a Spotify playlist.

        Args:
            playlist_id: Spotify playlist ID or URI

        Returns:
            List of track information dictionaries
        """
        tracks = []
        results = self.sp.playlist_tracks(playlist_id)

        while results:
            for item in results.get("items", []):
                if item and item.get("track"):
                    track = item["track"]
                    album_obj = track.get("album", {})
                    tracks.append(
                        {
                            "name": track.get("name"),
                            "artists": [
                                {
                                    "name": artist.get("name"),
                                    "id": artist.get("id"),
                                }
                                for artist in track.get("artists", [])
                            ],
                            "album": {
                                "name": album_obj.get("name"),
                                "id": album_obj.get("id"),
                                "artists": [
                                    {
                                        "name": artist.get("name"),
                                        "id": artist.get("id"),
                                    }
                                    for artist in album_obj.get("artists", [])
                                ],
                            },
                        }
                    )

            # Handle pagination
            if results.get("next"):
                results = self.sp.next(results)
            else:
                results = None

        return tracks

    def get_playlist_info(self, playlist_id: str) -> Dict[str, Any]:
        """
        Get information about a Spotify playlist.

        Args:
            playlist_id: Spotify playlist ID or URI

        Returns:
            Playlist information dictionary
        """
        playlist = self.sp.playlist(playlist_id)
        return {
            "name": playlist.get("name"),
            "description": playlist.get("description"),
            "owner": playlist.get("owner", {}).get("display_name"),
            "tracks_total": playlist.get("tracks", {}).get("total", 0),
        }

    def get_user_playlists(self, username: str) -> List[Dict[str, Any]]:
        """
        Fetch all public playlists for a specific user.

        Args:
            username: Spotify username (user ID)

        Returns:
            List of playlist information dictionaries
        """
        playlists = []
        try:
            results = self.sp.user_playlists(username)

            while results:
                for item in results.get("items", []):
                    if item:
                        playlists.append(
                            {
                                "id": item.get("id"),
                                "name": item.get("name"),
                                "owner": item.get("owner", {}).get(
                                    "display_name"
                                ),
                                "tracks_total": item.get("tracks", {}).get(
                                    "total", 0
                                ),
                                "public": item.get("public", False),
                            }
                        )

                # Handle pagination
                if results.get("next"):
                    results = self.sp.next(results)
                else:
                    results = None

        except Exception as e:
            print(
                f"Error fetching playlists for user '{username}': {e}",
                file=sys.stderr,
            )
            print(
                "Make sure the username is correct and the user has "
                "public playlists.",
                file=sys.stderr,
            )

        return playlists
