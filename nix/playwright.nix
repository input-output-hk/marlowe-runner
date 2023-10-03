{ pkgs, ... }:

let
  # extracted from "node_modules/playwright-core/browsers.json"
  chromiumRevision = "1060";
  ffmpegRevision = "1008";
in

pkgs.runCommand "playwright-browsers" { } ''
  mkdir -p $out/chromium-${chromiumRevision}/chrome-linux
  ln -sv ${pkgs.chromium}/bin/chromium $out/chromium-${chromiumRevision}/chrome-linux/chrome
  mkdir -p $out/ffmpeg-${ffmpegRevision}
  ln -sv ${pkgs.ffmpeg}/bin/ffmpeg $out/ffmpeg-${ffmpegRevision}/ffmpeg-linux
''

