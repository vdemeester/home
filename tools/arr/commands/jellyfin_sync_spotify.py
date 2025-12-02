"""
Sync Spotify playlists to Jellyfin.

This script:
1. Fetches tracks from specified Spotify playlists
2. Searches for matching tracks in Jellyfin library
3. Creates corresponding playlists in Jellyfin with matched tracks
4. Reports on matching success rate and missing tracks
"""

import time
from typing import List, Tuple

from lib import (
    CommandContext,
    JellyfinClient,
    SpotifyClient,
    print_section_header,
    select_with_fzf,
)


def normalize_string(s: str) -> str:
    """
    Normalize a string for comparison.

    Converts to lowercase, removes common punctuation, and extra spaces.

    Args:
        s: String to normalize

    Returns:
        Normalized string
    """
    import re

    # Convert to lowercase
    s = s.lower()
    # Remove common punctuation
    s = re.sub(r"['\",.:;!?()[\]{}]", "", s)
    # Normalize whitespace
    s = re.sub(r"\s+", " ", s).strip()
    return s


def match_track_in_jellyfin(
    jellyfin: JellyfinClient,
    track_name: str,
    artist_names: List[str],
    album_name: str,
) -> Tuple[str | None, float]:
    """
    Search for a track in Jellyfin and find the best match.

    Args:
        jellyfin: Jellyfin API client
        track_name: Track name from Spotify
        artist_names: List of artist names from Spotify
        album_name: Album name from Spotify

    Returns:
        Tuple of (jellyfin_item_id, confidence_score)
        Returns (None, 0.0) if no match found
    """
    # Build search query with track and primary artist
    primary_artist = artist_names[0] if artist_names else ""
    query = f"{track_name} {primary_artist}"

    # Search Jellyfin
    results = jellyfin.search_tracks(query, limit=20)

    if not results:
        return None, 0.0

    # Normalize search terms
    norm_track = normalize_string(track_name)
    norm_artists = {normalize_string(a) for a in artist_names}
    norm_album = normalize_string(album_name)

    best_match = None
    best_score = 0.0

    for item in results:
        score = 0.0

        # Check track name (weight: 40%)
        item_name = normalize_string(item.get("Name", ""))
        if item_name == norm_track:
            score += 0.4
        elif norm_track in item_name or item_name in norm_track:
            score += 0.2

        # Check artist names (weight: 40%)
        item_artists = item.get("Artists", [])
        item_artist_names = {normalize_string(a) for a in item_artists}
        if norm_artists & item_artist_names:  # Intersection
            score += 0.4
        elif any(
            any(ia in na or na in ia for ia in item_artist_names)
            for na in norm_artists
        ):
            score += 0.2

        # Check album name (weight: 20%)
        item_album = normalize_string(item.get("Album", ""))
        if item_album == norm_album:
            score += 0.2
        elif norm_album in item_album or item_album in norm_album:
            score += 0.1

        if score > best_score:
            best_score = score
            best_match = item.get("Id")

    return best_match, best_score


def run(
    jellyfin_url: str,
    jellyfin_api_token: str,
    jellyfin_user_id: str,
    spotify_client_id: str,
    spotify_client_secret: str,
    spotify_username: str,
    playlist_ids: List[str],
    match_threshold: float,
    dry_run: bool,
    no_confirm: bool,
    all_playlists: bool,
    public: bool,
):
    """Execute the jellyfin sync-spotify command."""
    # Create clients and context
    jellyfin = JellyfinClient(
        jellyfin_url, jellyfin_api_token, jellyfin_user_id
    )
    ctx = CommandContext(dry_run, no_confirm)

    # Determine if we need interactive mode
    use_interactive = not playlist_ids and spotify_username

    # Initialize Spotify client
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
            "  arr jellyfin sync-spotify http://localhost:8096 "
            "-u your-username"
        )
        print(
            "  arr jellyfin sync-spotify http://localhost:8096 "
            "PLAYLIST_ID_1 PLAYLIST_ID_2"
        )
        return

    # Check existing Jellyfin playlists
    print("Fetching existing Jellyfin playlists...")
    existing_playlists = jellyfin.get_playlists()
    existing_playlist_names = {
        normalize_string(p.get("Name", "")) for p in existing_playlists
    }

    print_section_header("SYNCING SPOTIFY PLAYLISTS TO JELLYFIN")

    playlists_created = 0
    playlists_skipped = 0
    total_tracks = 0
    matched_tracks = 0
    failed_matches = []

    for playlist_id in playlist_ids:
        try:
            # Get playlist info
            info = spotify.get_playlist_info(playlist_id)
            playlist_name = info["name"]
            print(
                f"\n\nPlaylist: {playlist_name} "
                f"(by {info['owner']}, {info['tracks_total']} tracks)"
            )

            # Check if playlist already exists in Jellyfin
            if normalize_string(playlist_name) in existing_playlist_names:
                print(
                    f"  ⚠ Playlist '{playlist_name}' already exists "
                    "in Jellyfin, skipping..."
                )
                playlists_skipped += 1
                continue

            # Get tracks
            tracks = spotify.get_playlist_tracks(playlist_id)
            print(f"  Retrieved {len(tracks)} tracks from Spotify")

            # Match tracks in Jellyfin
            print("  Matching tracks in Jellyfin library...")
            jellyfin_item_ids = []
            local_failed = []

            for idx, track in enumerate(tracks, 1):
                track_name = track.get("name", "")
                artist_names = [
                    a.get("name", "") for a in track.get("artists", [])
                ]
                album_name = track.get("album", "")

                if not track_name or not artist_names:
                    continue

                total_tracks += 1
                print(
                    f"    [{idx}/{len(tracks)}] Searching: "
                    f"{track_name} - {', '.join(artist_names)}"
                )

                item_id, score = match_track_in_jellyfin(
                    jellyfin, track_name, artist_names, album_name
                )

                if item_id and score >= match_threshold:
                    jellyfin_item_ids.append(item_id)
                    matched_tracks += 1
                    print(f"      ✓ Matched (confidence: {score:.2f})")
                else:
                    local_failed.append(
                        {
                            "track": track_name,
                            "artists": ", ".join(artist_names),
                            "album": album_name,
                            "score": score,
                            "playlist": playlist_name,
                        }
                    )
                    print(
                        f"      ✗ No match (best score: {score:.2f}, "
                        f"threshold: {match_threshold:.2f})"
                    )

                # Small delay to avoid hammering Jellyfin
                time.sleep(0.1)

            failed_matches.extend(local_failed)

            # Create playlist in Jellyfin
            if jellyfin_item_ids:
                print(
                    f"\n  Matched {len(jellyfin_item_ids)}/{len(tracks)} "
                    f"tracks ({len(jellyfin_item_ids)/len(tracks)*100:.1f}%)"
                )

                if not ctx.dry_run:
                    try:
                        result = jellyfin.create_playlist(
                            playlist_name, jellyfin_item_ids, public
                        )
                        if result and result.get("Id"):
                            print(
                                f"  ✓ Created playlist in Jellyfin "
                                f"(ID: {result['Id']})"
                            )
                            playlists_created += 1
                        else:
                            print("  ✗ Failed to create playlist")
                            playlists_skipped += 1
                    except Exception as e:
                        print(f"  ✗ Error creating playlist: {e}")
                        playlists_skipped += 1
                else:
                    print(
                        f"  [DRY RUN] Would create playlist "
                        f"'{playlist_name}' with {len(jellyfin_item_ids)} "
                        "tracks"
                    )
                    playlists_created += 1
            else:
                print(
                    "\n  ✗ No tracks matched - playlist not created"
                )
                playlists_skipped += 1

        except Exception as e:
            print(f"\nError processing playlist {playlist_id}: {e}")
            playlists_skipped += 1
            continue

    # Final summary
    print_section_header("FINAL SUMMARY")
    print(f"\nTotal playlists processed: {len(playlist_ids)}")
    print(f"  - Created: {playlists_created}")
    print(f"  - Skipped: {playlists_skipped}")
    print(f"\nTotal tracks processed: {total_tracks}")
    print(f"  - Matched: {matched_tracks}")
    print(f"  - Failed to match: {len(failed_matches)}")
    if total_tracks > 0:
        print(
            f"  - Match rate: "
            f"{matched_tracks/total_tracks*100:.1f}%"
        )

    if failed_matches:
        print_section_header("FAILED MATCHES")
        print(
            f"\nThe following {len(failed_matches)} tracks could not be "
            f"matched (threshold: {match_threshold:.2f}):\n"
        )
        for item in failed_matches[:20]:  # Show first 20
            print(
                f"  • {item['track']} - {item['artists']} "
                f"(from '{item['playlist']}')"
            )
            print(
                f"    Album: {item['album']}, "
                f"Best score: {item['score']:.2f}"
            )
        if len(failed_matches) > 20:
            print(f"\n  ... and {len(failed_matches) - 20} more")
        print(
            "\nTip: Lower --match-threshold if too many false negatives. "
            "Default is 0.6."
        )

    if ctx.dry_run:
        print(
            "\n[DRY RUN] No changes were made. "
            "Remove --dry-run to create playlists."
        )
    elif playlists_created > 0:
        print(
            f"\n✓ Successfully created {playlists_created} playlist(s) "
            "in Jellyfin!"
        )
