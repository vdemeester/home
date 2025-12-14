{
  lib,
  writeShellApplication,
  audible-cli,
  aaxtomp3,
  ffmpeg,
  mediainfo,
  jq,
}:

writeShellApplication {
  name = "audible-converter";

  runtimeInputs = [
    audible-cli
    aaxtomp3
    ffmpeg
    mediainfo
    jq
  ];

  text = builtins.readFile ./convert.sh;

  meta = {
    description = "Download and convert Audible audiobooks to Audiobookshelf-compatible formats";
    longDescription = ''
      A wrapper tool that combines audible-cli and AAXtoMP3 to download
      audiobooks from Audible and convert them to M4B format for use with
      Audiobookshelf or other audiobook players.

      Features:
      - Download entire Audible library or specific books
      - Convert AAX to M4B/MP3/M4A
      - Preserve chapter markers and metadata
      - Organize output for Audiobookshelf
    '';
    homepage = "https://github.com/vdemeester/home";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
    maintainers = [ ];
  };
}
