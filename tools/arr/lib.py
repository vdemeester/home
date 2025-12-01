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
        print(
            f"Error: Failed after {max_retries} attempts", file=sys.stderr
        )
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

                print(
                    f"Error posting to {endpoint}: HTTP {status_code}",
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
                print(f"Error posting to {endpoint}: {e}", file=sys.stderr)
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


def get_confirmation_decision(
    ctx: CommandContext, prompt: str
) -> bool:
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
        print(
            "Error: fzf not found. Please install fzf:", file=sys.stderr
        )
        print(
            "  On NixOS: nix-env -iA nixpkgs.fzf", file=sys.stderr
        )
        print("  On other systems: see https://github.com/junegunn/fzf")
        sys.exit(1)


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
                            "album": track.get("album", {}).get("name"),
                            "album_id": track.get("album", {}).get("id"),
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
