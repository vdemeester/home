{ config, pkgs, ... }:
{
  programs.beets = {
    enable = true;

    # Add extra Python packages to beets' environment
    package = pkgs.beets.override {
      extraPackages = [
        pkgs.beets-lidarr-fields
        pkgs.python3Packages.beets-filetote
      ];
    };

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
        "smartplaylist"
        "lidarrfields" # Nix-packaged plugin for Lidarr-compatible paths
        "filetote" # Manage non-music files (art, cue, logs, lyrics)
      ];

      # Path formats using Lidarr-compatible fields
      paths = {
        # Regular albums - using Lidarr fields for compatibility
        default = "library/$lidarr_albumartist/$lidarr_album_title/$lidarr_track_title";

        # Soundtracks - using Lidarr fields
        "albumtype:soundtrack" = "soundtrack/$lidarr_album_title/$lidarr_track_title";

        # Singletons (podcasts, DJ sets, etc)
        singleton = "single/$lidarr_artist - $lidarr_title";

        # Compilations - using Lidarr fields
        comp = "compilation/$lidarr_album_title/$lidarr_track_title";

        # Podcasts get their own path (not using Lidarr fields since not from Lidarr)
        "albumtype:podcast" = "podcasts/$album/$track - $title";
      };

      # Alternative metadata sources
      discogs.source_weight = 0.0;
      musicbrainz.source_weight = 0.5;

      # Artwork
      fetchart = {
        auto = true;
        sources = "coverart itunes amazon albumart";
      };

      embedart = {
        auto = true;
        maxwidth = 1000;
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
        extensions = ".cue .log .txt .jpg .jpeg .png .webp .gif .pdf .nfo .m3u .sfv .md5";

        # Pairing configuration - for lyrics files
        pairing = {
          enabled = true;
          pairing_only = false; # Also process non-paired files
          extensions = ".lrc .txt"; # Paired lyrics files
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
          extensions = ".torrent .url .htm .html"; # Download artifacts
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
          "ext:.cue" = "$albumpath/$lidarr_albumartist - $lidarr_album_title";
          "ext:.log" = "$albumpath/$lidarr_albumartist - $lidarr_album_title";
          "ext:.txt" = "$albumpath/$lidarr_albumartist - $lidarr_album_title";

          # PDFs (booklets/scans) in album directory (using lidarr fields)
          "ext:.pdf" = "$albumpath/$lidarr_albumartist - $lidarr_album_title";

          # Paired lyrics files next to tracks
          # $medianame_new is the new filename of the paired track (already using lidarr fields)
          "paired_ext:.lrc" = "$albumpath/$medianame_new";
          "paired_ext:.txt" = "$albumpath/$medianame_new";

          # NFO files in album directory (using lidarr fields)
          "ext:.nfo" = "$albumpath/$lidarr_albumartist - $lidarr_album_title";
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
