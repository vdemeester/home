{
  baseDir ? null,
}:
{
  config,
  pkgs,
  lib,
  ...
}:
let
  # Use provided baseDir or default to ~/desktop/music/test
  actualBaseDir =
    if baseDir != null then baseDir else "${config.home.homeDirectory}/desktop/music/test";

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

  # Check if baseDir is within home directory
  isRelativeToHome = lib.hasPrefix config.home.homeDirectory actualBaseDir;

  # Get relative path from home directory if applicable
  relativePath =
    if isRelativeToHome then lib.removePrefix "${config.home.homeDirectory}/" actualBaseDir else null;

  # Directories to create .keep files for (only if baseDir is in home)
  musicDirs = [
    "library"
    "soundtrack"
    "compilation"
    "single"
    "podcasts"
    "mixes"
    "playlists"
  ];

  # Generate .keep file declarations
  keepFiles = lib.optionalAttrs isRelativeToHome (
    builtins.listToAttrs (
      map (dir: {
        name = "${relativePath}/${dir}/.keep";
        value.text = "";
      }) musicDirs
    )
  );
in
{
  programs.beets = {
    enable = true;
    package = beetsWithPlugins;

    settings = {
      # Library paths
      directory = actualBaseDir;
      library = "${actualBaseDir}/musiclibrary.db";

      # Import settings
      import = {
        move = true;
        incremental = true;
        quiet_fallback = "asis"; # Handle non-MusicBrainz content
        log = "${actualBaseDir}/import.log";
        write = true; # Always write tags to files during import
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

        # DJ mixes and mixtapes
        "albumtype:mixtape" = "mixes/$artist - $album";
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
      # NOTE: auto=false prevents lyrics from being removed during 'beet update'
      # Use 'beet lyrics' manually to fetch lyrics when needed
      lyrics = {
        auto = false;
        force = false; # Don't overwrite existing lyrics
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
        auto = false; # Disable auto-updates during import to avoid concurrency issues
        relative_to = "${actualBaseDir}/playlists";
        playlist_dir = "${actualBaseDir}/playlists";
        playlists = [
          {
            name = "podcasts.m3u";
            query = "albumtype:podcast";
          }
          # Language-based playlists
          {
            name = "english.m3u";
            query = "language:eng";
          }
          {
            name = "french.m3u";
            query = "language:fra";
          }
          {
            name = "japanese.m3u";
            query = "language:jpn";
          }
          {
            name = "korean.m3u";
            query = "language:kor";
          }
          {
            name = "spanish.m3u";
            query = "language:spa";
          }
          {
            name = "german.m3u";
            query = "language:deu";
          }
          {
            name = "italian.m3u";
            query = "language:ita";
          }
          {
            name = "instrumental.m3u";
            query = "language:zxx";
          }
          # Genre-based playlists
          {
            name = "rock.m3u";
            query = "genre:Rock";
          }
          {
            name = "electronic.m3u";
            query = "genre:Electronic";
          }
          {
            name = "trance.m3u";
            query = "genre:Trance";
          }
          {
            name = "jazz.m3u";
            query = "genre:Jazz";
          }
          {
            name = "classical.m3u";
            query = "genre:Classical";
          }
          {
            name = "hiphop.m3u";
            query = "genre:'Hip Hop'";
          }
          {
            name = "metal.m3u";
            query = "genre:Metal";
          }
          {
            name = "pop.m3u";
            query = "genre:Pop";
          }
          {
            name = "ambient.m3u";
            query = "genre:Ambient";
          }
          {
            name = "folk.m3u";
            query = "genre:Folk";
          }
          # Time-based playlists - Decades
          {
            name = "1960s.m3u";
            query = "year:1960..1969";
          }
          {
            name = "1970s.m3u";
            query = "year:1970..1979";
          }
          {
            name = "1980s.m3u";
            query = "year:1980..1989";
          }
          {
            name = "1990s.m3u";
            query = "year:1990..1999";
          }
          {
            name = "2000s.m3u";
            query = "year:2000..2009";
          }
          {
            name = "2010s.m3u";
            query = "year:2010..2019";
          }
          {
            name = "2020s.m3u";
            query = "year:2020..2029";
          }
          # Time-based - Recent additions
          {
            name = "last-week.m3u";
            query = "added:-1w..";
          }
          {
            name = "this-year.m3u";
            query = "added:-1y..";
          }
          # Discovery & Rediscovery
          {
            name = "unplayed.m3u";
            query = "play_count:0";
          }
          {
            name = "rarely-played.m3u";
            query = "play_count:..3 added:..2024-01-01";
          }
          # Album types
          {
            name = "compilations.m3u";
            query = "albumtype:compilation";
          }
          {
            name = "live-albums.m3u";
            query = "albumtype:live";
          }
          {
            name = "eps.m3u";
            query = "albumtype:ep";
          }
          {
            name = "singles.m3u";
            query = "albumtype:single";
          }
          {
            name = "remixes.m3u";
            query = "albumtype:remix";
          }
          # Multi-language cultural groups
          {
            name = "asian.m3u";
            query = "language:jpn,kor,cmn,zho";
          }
          {
            name = "european.m3u";
            query = "language:fra,deu,ita,spa";
          }
          # Energy & Mood
          {
            name = "workout.m3u";
            query = "genre:Electronic,Rock,'Hip Hop' bpm:120..";
          }
          {
            name = "chill.m3u";
            query = "genre:Ambient,Jazz,Folk bpm:..100";
          }
          # Length-based playlists
          {
            name = "short-tracks.m3u";
            query = "length:..3:00";
          }
          {
            name = "interludes.m3u";
            query = "length:..1:30";
          }
          {
            name = "long-tracks.m3u";
            query = "length:6:00..";
          }
          {
            name = "epics.m3u";
            query = "length:10:00..";
          }
          {
            name = "standard-length.m3u";
            query = "length:3:00..5:00";
          }
          # Seasonal & Special Occasions
          {
            name = "christmas.m3u";
            query = "genre:Christmas,Holiday";
          }
          {
            name = "summer.m3u";
            query = "genre:Reggae,Ska,'Surf Rock'";
          }
          # Specific use cases
          {
            name = "focus.m3u";
            query = "genre:Classical,Ambient,'Post-Rock' bpm:..90";
          }
          {
            name = "party.m3u";
            query = "genre:Pop,Dance,Electronic,'Hip Hop' bpm:110..140 year:2015..";
          }
          {
            name = "driving.m3u";
            query = "genre:Rock,Electronic,'Indie Rock' bpm:100..130";
          }
          # Favorites & Ratings
          {
            name = "5-stars.m3u";
            query = "rating:5";
          }
          {
            name = "favorites.m3u";
            query = "rating:4..";
          }
          {
            name = "hidden-gems.m3u";
            query = "rating:4.. play_count:..5";
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

  # Create music directories with .keep files (only if baseDir is within home)
  home.file = keepFiles;
}
