#!/usr/bin/env python3
"""
Music Playlist Downloader
Downloads DJ podcasts/radio shows from Mixcloud and SoundCloud,
and generates M3U playlists.
"""

import argparse
import logging
import os
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import List

import yaml


@dataclass
class MixcloudShow:
    """Configuration for a Mixcloud show."""

    handle: str
    artist: str
    show: str


@dataclass
class SoundcloudShow:
    """Configuration for a SoundCloud show."""

    url: str
    artist: str
    show: str


@dataclass
class Config:
    """Main configuration."""

    base_dir: Path
    mixcloud_shows: List[MixcloudShow]
    soundcloud_shows: List[SoundcloudShow]
    yt_dlp_options: dict


def load_config(config_path: Path) -> Config:
    """Load configuration from YAML file."""
    with open(config_path) as f:
        data = yaml.safe_load(f)

    mixcloud_shows = [
        MixcloudShow(**show) for show in data.get("mixcloud_shows", [])
    ]
    soundcloud_shows = [
        SoundcloudShow(**show) for show in data.get("soundcloud_shows", [])
    ]

    return Config(
        base_dir=Path(data.get("base_dir", "/neo/music")),
        mixcloud_shows=mixcloud_shows,
        soundcloud_shows=soundcloud_shows,
        yt_dlp_options=data.get("yt_dlp_options", {}),
    )


def build_yt_dlp_command(
    url: str,
    output_template: str,
    artist: str,
    archive_file: Path,
    yt_dlp_options: dict,
) -> List[str]:
    """Build yt-dlp command with options."""
    cmd = ["yt-dlp"]

    # Add configured options
    if yt_dlp_options.get("ignore_errors", True):
        cmd.append("-i")
    if yt_dlp_options.get("continue", True):
        cmd.append("-c")

    format_opt = yt_dlp_options.get("format", "best")
    cmd.extend(["-f", format_opt])

    if yt_dlp_options.get("add_metadata", True):
        cmd.append("--add-metadata")
    if yt_dlp_options.get("embed_thumbnail", True):
        cmd.append("--embed-thumbnail")

    # Download archive for deduplication
    cmd.extend(["--download-archive", str(archive_file)])

    # Parse metadata
    cmd.extend(
        [
            "--parse-metadata",
            "title:%(album)s",
            "--parse-metadata",
            f"{artist}:%(artist)s",
        ]
    )

    # Output template
    cmd.extend(["--output", output_template, url])

    return cmd


def download_mixcloud_show(
    show: MixcloudShow, library_dir: Path, yt_dlp_options: dict
) -> bool:
    """Download a Mixcloud show."""
    url = f"https://www.mixcloud.com/{show.handle}/"
    output_dir = library_dir / show.artist / show.show
    output_dir.mkdir(parents=True, exist_ok=True)

    output_template = str(output_dir / "%(title)s-%(id)s.%(ext)s")
    archive_file = output_dir / ".downloaded.txt"

    logging.info(f"Downloading {show.show} by {show.artist}...")
    cmd = build_yt_dlp_command(
        url, output_template, show.artist, archive_file, yt_dlp_options
    )

    try:
        subprocess.run(cmd, check=True, capture_output=False)
        return True
    except subprocess.CalledProcessError as e:
        logging.warning(f"Failed to download {show.show}: {e}")
        return False


def download_soundcloud_show(
    show: SoundcloudShow, library_dir: Path, yt_dlp_options: dict
) -> bool:
    """Download a SoundCloud show."""
    output_dir = library_dir / show.artist / show.show
    output_dir.mkdir(parents=True, exist_ok=True)

    output_template = str(output_dir / "%(title)s-%(id)s.%(ext)s")
    archive_file = output_dir / ".downloaded.txt"

    logging.info(f"Downloading {show.show} by {show.artist}...")
    cmd = build_yt_dlp_command(
        show.url, output_template, show.artist, archive_file, yt_dlp_options
    )

    try:
        subprocess.run(cmd, check=True, capture_output=False)
        return True
    except subprocess.CalledProcessError as e:
        logging.warning(f"Failed to download {show.show}: {e}")
        return False


def generate_playlist(
    artist: str, show: str, library_dir: Path, playlist_dir: Path
):
    """Generate M3U playlist for a show."""
    show_dir = library_dir / artist / show
    if not show_dir.exists():
        logging.warning(f"Show directory does not exist: {show_dir}")
        return

    # Find all audio files (exclude archive file)
    audio_extensions = {".m4a", ".mp3", ".opus", ".ogg", ".flac"}
    audio_files = sorted(
        [
            f
            for f in show_dir.iterdir()
            if f.is_file()
            and f.suffix.lower() in audio_extensions
            and not f.name.startswith(".")  # Exclude hidden files
        ]
    )

    if not audio_files:
        logging.warning(f"No audio files found in {show_dir}")
        return

    # Generate playlist filename
    playlist_name = f"{artist} - {show}.m3u"
    playlist_path = playlist_dir / playlist_name

    logging.info(
        f"Generating playlist: {playlist_name} ({len(audio_files)} files)"
    )

    # Write M3U playlist with relative paths
    with open(playlist_path, "w", encoding="utf-8") as f:
        f.write("#EXTM3U\n")
        for audio_file in audio_files:
            # Use relative path from playlist to audio file
            relative_path = os.path.relpath(audio_file, playlist_dir)
            f.write(f"{relative_path}\n")


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Download music podcasts and generate playlists"
    )
    parser.add_argument(
        "--config",
        type=Path,
        default="/neo/music/music-playlist-dl.yaml",
        help="Path to configuration file",
    )
    parser.add_argument(
        "--verbose", "-v", action="store_true", help="Enable verbose logging"
    )
    args = parser.parse_args()

    # Setup logging
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s - %(levelname)s - %(message)s",
    )

    # Load configuration
    try:
        config = load_config(args.config)
    except Exception as e:
        logging.error(f"Failed to load configuration: {e}")
        sys.exit(1)

    # Setup directories
    library_dir = config.base_dir / "library"
    playlist_dir = config.base_dir / "playlist"
    library_dir.mkdir(parents=True, exist_ok=True)
    playlist_dir.mkdir(parents=True, exist_ok=True)

    logging.info(
        f"Starting music podcast downloads to: {library_dir}"
    )
    logging.info("=" * 60)

    # Download Mixcloud shows
    for show in config.mixcloud_shows:
        download_mixcloud_show(show, library_dir, config.yt_dlp_options)

    # Download SoundCloud shows
    for show in config.soundcloud_shows:
        download_soundcloud_show(show, library_dir, config.yt_dlp_options)

    logging.info("=" * 60)
    logging.info("Generating playlists...")

    # Generate playlists for all shows
    for show in config.mixcloud_shows:
        generate_playlist(show.artist, show.show, library_dir, playlist_dir)

    for show in config.soundcloud_shows:
        generate_playlist(show.artist, show.show, library_dir, playlist_dir)

    logging.info("=" * 60)
    logging.info("Download complete!")


if __name__ == "__main__":
    main()
