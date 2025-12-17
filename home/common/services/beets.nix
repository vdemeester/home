{ config, pkgs, ... }:
let
  # Override beets with our custom plugins using the official pluginOverrides mechanism
  beetsWithPlugins = pkgs.beets.override {
    python3 = pkgs.python3.override {
      packageOverrides = _self: super: {
        beets = super.beets.override {
          pluginOverrides = {
            lidarrfields = {
              enable = true;
              propagatedBuildInputs = [ pkgs.beets-lidarr-fields ];
            };
            filetote = {
              enable = true;
              propagatedBuildInputs = [ pkgs.beets-filetote ];
            };
          };
        };
      };
    };
  };
in
{
  programs.beets = {
    enable = true;
    package = beetsWithPlugins;

    settings = {
      # Library paths
      directory = "${config.home.homeDirectory}/desktop/music/test";
      library = "${config.home.homeDirectory}/desktop/music/test/musiclibrary.db";

      # Import settings
      import = {
        move = true;
        incremental = true;
        quiet_fallback = "asis"; # Handle non-MusicBrainz content
        log = "${config.home.homeDirectory}/desktop/music/import.log";
      };

      # Essential plugins
      plugins = [
        "fetchart"
        "embedart"
        "lyrics"
        "lastgenre"
        "replaygain"
        "duplicates"
        "discogs"
        "fromfilename"
        "edit"
        "musicbrainz"
        "smartplaylist"
        "lidarrfields" # Nix-packaged plugin for Lidarr-compatible paths
        "filetote" # Manage non-music files (art, cue, logs, lyrics)
      ];

      # Path formats using Lidarr-compatible fields
      paths = {
        # Regular albums - using lidarr fields plugin format
        # Format: Artist/Album (Year)/[Disc-]Track.Title
        default = "library/$releasegroupartist/$lidarralbum%aunique{}/%if{$audiodisctotal,$disc-}$track.$title";

        # Soundtracks - using lidarr fields
        "albumtype:soundtrack" =
          "soundtrack/$lidarralbum%aunique{}/%if{$audiodisctotal,$disc-}$track.$title";

        # Singletons (podcasts, DJ sets, etc)
        singleton = "single/$artist - $title";

        # Compilations - using lidarr fields
        comp = "compilation/$lidarralbum%aunique{}/%if{$audiodisctotal,$disc-}$track.$title";

        # Podcasts get their own path (not using Lidarr fields since not from Lidarr)
        "albumtype:podcast" = "podcasts/$album/$track - $title";
      };

      # Enable per-disc track numbering for multi-disc albums
      per_disc_numbering = true;

      # Alternative metadata sources
      discogs.data_source_mismatch_penalty = 0.5;
      musicbrainz.data_source_mismatch_penalty = 0.0;

      # Artwork
      fetchart = {
        auto = true;
        sources = "coverart itunes amazon albumart";
      };

      embedart = {
        auto = true;
        maxwidth = 1000;
      };

      # Lyrics fetching
      lyrics = {
        auto = true;
        sources = [
          "lrclib"
          "genius"
          "musixmatch"
        ];
        # Prefer synced lyrics from lrclib
        synced = true;
        # Fall back to plain text if synced not available
        fallback = "";
      };

      # ReplayGain for volume normalization
      replaygain = {
        auto = true;
        backend = "ffmpeg";
      };

      # Smart playlists
      smartplaylist = {
        relative_to = "${config.home.homeDirectory}/desktop/music/test";
        playlist_dir = "${config.home.homeDirectory}/desktop/music/test/playlists";
        playlists = [
          {
            name = "recent.m3u";
            query = "added:-1m..";
          }
          {
            name = "podcasts.m3u";
            query = "albumtype:podcast";
          }
        ];
      };

      # Filetote - manage non-music files during import
      filetote = {
        # File extensions to move/copy along with music
        # Covers: album art, scans, booklets
        # Audio related: cue sheets, ripping logs, accuracy logs
        # Text: lyrics, metadata files
        extensions = [
          ".cue"
          ".log"
          ".txt"
          ".jpg"
          ".jpeg"
          ".png"
          ".webp"
          ".gif"
          ".pdf"
          ".nfo"
          ".m3u"
          ".sfv"
          ".md5"
        ];

        # Pairing configuration - for lyrics files
        pairing = {
          enabled = true;
          pairing_only = false; # Also process non-paired files
          extensions = [
            ".lrc"
            ".txt"
          ]; # Paired lyrics files
        };

        # Exclude unwanted files
        exclude = {
          # Common junk files to skip
          filenames = [
            "Thumbs.db"
            ".DS_Store"
            "desktop.ini"
            ".directory"
          ];
          # Download artifacts
          extensions = [
            ".torrent"
            ".url"
            ".htm"
            ".html"
          ];
        };

        # Path configuration for different file types
        # Using lidarr fields for consistency with main path format
        paths = {
          # Album art goes to album directory with standard name
          # $albumpath automatically resolves to the lidarr-formatted directory
          "ext:.jpg" = "$albumpath/cover";
          "ext:.jpeg" = "$albumpath/cover";
          "ext:.png" = "$albumpath/cover";
          "ext:.webp" = "$albumpath/cover";

          # Cue sheets and logs in album directory (using lidarr fields)
          "ext:.cue" = "$albumpath/$releasegroupartist - $lidarralbum";
          "ext:.log" = "$albumpath/$releasegroupartist - $lidarralbum";
          "ext:.txt" = "$albumpath/$releasegroupartist - $lidarralbum";

          # PDFs (booklets/scans) in album directory (using lidarr fields)
          "ext:.pdf" = "$albumpath/$releasegroupartist - $lidarralbum";

          # Paired lyrics files next to tracks
          # $medianame_new is the new filename of the paired track
          "paired_ext:.lrc" = "$albumpath/$medianame_new";
          "paired_ext:.txt" = "$albumpath/$medianame_new";

          # NFO files in album directory (using lidarr fields)
          "ext:.nfo" = "$albumpath/$releasegroupartist - $lidarralbum";
        };

        # Print ignored files for debugging
        print_ignored = false;
      };
    };
  };

  # Create music directories
  home.file."desktop/music/test/library/.keep".text = "";
  home.file."desktop/music/test/soundtrack/.keep".text = "";
  home.file."desktop/music/test/compilation/.keep".text = "";
  home.file."desktop/music/test/single/.keep".text = "";
  home.file."desktop/music/test/podcasts/.keep".text = "";
  home.file."desktop/music/test/playlists/.keep".text = "";
}
