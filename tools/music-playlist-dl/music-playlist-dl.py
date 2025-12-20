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
    beets_tags: dict = None  # Optional per-show metadata

    def __post_init__(self):
        if self.beets_tags is None:
            self.beets_tags = {}


@dataclass
class SoundcloudShow:
    """Configuration for a SoundCloud show."""

    url: str
    artist: str
    show: str
    beets_tags: dict = None  # Optional per-show metadata

    def __post_init__(self):
        if self.beets_tags is None:
            self.beets_tags = {}


@dataclass
class BeetsConfig:
    """Beets integration configuration."""

    enable: bool = False
    import_after_download: bool = True
    write_tags: bool = True
    default_tags: dict = None

    def __post_init__(self):
        if self.default_tags is None:
            self.default_tags = {}


@dataclass
class Config:
    """Main configuration."""

    base_dir: Path
    mixcloud_shows: List[MixcloudShow]
    soundcloud_shows: List[SoundcloudShow]
    yt_dlp_options: dict
    beets: BeetsConfig


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

    # Load beets config if present
    beets_data = data.get("beets", {})
    beets_config = BeetsConfig(
        enable=beets_data.get("enable", False),
        import_after_download=beets_data.get("import_after_download", True),
        write_tags=beets_data.get("write_tags", True),
        default_tags=beets_data.get("default_tags", {}),
    )

    return Config(
        base_dir=Path(data.get("base_dir", "/neo/music")),
        mixcloud_shows=mixcloud_shows,
        soundcloud_shows=soundcloud_shows,
        yt_dlp_options=data.get("yt_dlp_options", {}),
        beets=beets_config,
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
    show: MixcloudShow, base_dir: Path, yt_dlp_options: dict
) -> bool:
    """Download a Mixcloud show."""
    url = f"https://www.mixcloud.com/{show.handle}/"
    output_dir = base_dir / show.show
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
    show: SoundcloudShow, base_dir: Path, yt_dlp_options: dict
) -> bool:
    """Download a SoundCloud show."""
    output_dir = base_dir / show.show
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
    artist: str, show: str, base_dir: Path, playlist_dir: Path
):
    """Generate M3U playlist for a show."""
    show_dir = base_dir / show
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


def import_to_beets(
    base_dir: Path,
    artist: str,
    show: str,
    show_beets_tags: dict,
    beets_config: BeetsConfig,
) -> bool:
    """Import show to beets database with merged metadata."""
    if not beets_config.enable:
        return True  # Skip if disabled

    show_dir = base_dir / show
    if not show_dir.exists():
        logging.warning(f"Show directory does not exist: {show_dir}")
        return False

    # Merge tags: default_tags < show_beets_tags
    merged_tags = {**beets_config.default_tags, **show_beets_tags}

    # Always set artist and album from show config
    merged_tags["artist"] = artist
    merged_tags["album"] = show

    # Build beets import command
    cmd = [
        "beet",
        "import",
        "-C",  # Don't move files (keep in place)
        "-A",  # Don't autotag (skip MusicBrainz)
        "-q",  # Quiet mode
    ]

    # Add all merged tags
    for key, value in merged_tags.items():
        cmd.extend(["--set", f"{key}={value}"])

    cmd.append(str(show_dir))

    try:
        result = subprocess.run(
            cmd, check=True, capture_output=True, text=True
        )
        logging.debug(f"Beets import output: {result.stdout}")

        # Write metadata to file tags if enabled
        if beets_config.write_tags:
            write_cmd = ["beet", "write", "-q", f"album:{show}"]
            subprocess.run(write_cmd, check=False, capture_output=True)

        logging.info(f"✓ Imported {show} to beets")
        return True
    except subprocess.CalledProcessError as e:
        logging.warning(f"Failed to import {show} to beets: {e.stderr}")
        return False


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
    parser.add_argument(
        "--import-existing",
        action="store_true",
        help="Import all existing files to beets (run once after enabling)",
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
    base_dir = config.base_dir
    playlist_dir = base_dir.parent / "playlists"
    base_dir.mkdir(parents=True, exist_ok=True)
    playlist_dir.mkdir(parents=True, exist_ok=True)

    logging.info(
        f"Starting music podcast downloads to: {base_dir}"
    )
    logging.info("=" * 60)

    # Download Mixcloud shows
    for show in config.mixcloud_shows:
        download_mixcloud_show(show, base_dir, config.yt_dlp_options)

    # Download SoundCloud shows
    for show in config.soundcloud_shows:
        download_soundcloud_show(show, base_dir, config.yt_dlp_options)

    logging.info("=" * 60)
    logging.info("Generating playlists...")

    # Generate playlists for all shows
    for show in config.mixcloud_shows:
        generate_playlist(show.artist, show.show, base_dir, playlist_dir)

    for show in config.soundcloud_shows:
        generate_playlist(show.artist, show.show, base_dir, playlist_dir)

    # Import to beets if enabled
    if config.beets.enable:
        if args.import_existing:
            logging.info("=" * 60)
            logging.info("Importing all existing files to beets...")
            for show in config.mixcloud_shows:
                import_to_beets(
                    base_dir,
                    show.artist,
                    show.show,
                    show.beets_tags,
                    config.beets,
                )
            for show in config.soundcloud_shows:
                import_to_beets(
                    base_dir,
                    show.artist,
                    show.show,
                    show.beets_tags,
                    config.beets,
                )
            logging.info("Beets import complete!")
            sys.exit(0)

        elif config.beets.import_after_download:
            logging.info("=" * 60)
            logging.info("Importing new downloads to beets...")
            for show in config.mixcloud_shows:
                import_to_beets(
                    base_dir,
                    show.artist,
                    show.show,
                    show.beets_tags,
                    config.beets,
                )
            for show in config.soundcloud_shows:
                import_to_beets(
                    base_dir,
                    show.artist,
                    show.show,
                    show.beets_tags,
                    config.beets,
                )

    logging.info("=" * 60)
    logging.info("Download complete!")


if __name__ == "__main__":
    main()
