#!/usr/bin/env bash
# fetch_backup.sh: stream an EcoTaxa project backup zip from Google Drive to the node host, then unzip it
# under the node's file_srv so /api/file_import can read it. Run on the LAPTOP: the Drive token stays in
# the laptop's rclone config and never lands on the server (the bytes pass through the laptop).
#
#   scripts/fetch_backup.sh <drive_file_id> <dest_dir_under_file_srv>
#   scripts/fetch_backup.sh 1dXWk_uKw-C9eahfdDKfIndI1gLv9SiEv bak_11310      # project 4, 2026-11-09
#
# needs: rclone remote `gdrive-ecoquants` (Drive, with access to the shared file), ssh alias `msens`,
# and the msens key at $MSENS_KEY. --drive-acknowledge-abuse is needed for large shared zips Drive
# will not virus-scan.
set -euo pipefail
ID=${1:?drive file id}; DEST=${2:?destination folder under file_srv, e.g. bak_11310}
FILE_SRV=/share/ecotaxa/file_srv
export RCLONE_SFTP_HOST=${RCLONE_SFTP_HOST:-msens1.marinesensitivity.org} RCLONE_SFTP_USER=${RCLONE_SFTP_USER:-ubuntu}
export RCLONE_SFTP_KEY_FILE=${MSENS_KEY:-"$HOME/My Drive/private/msens_key_pair.pem"}
ssh msens "mkdir -p $FILE_SRV/$DEST && df -h /share | tail -1"
caffeinate -i rclone backend copyid gdrive-ecoquants: "$ID" ":sftp:$FILE_SRV/$DEST/" \
  --stats 60s --stats-one-line -v --drive-acknowledge-abuse
# unzip on the host; the zip and its copy both stay until the import is verified (then delete both)
ssh msens "cd $FILE_SRV/$DEST && ls -la && mkdir -p unzipped && unzip -q -o *.zip -d unzipped && \
  find unzipped -name '*.tsv' | wc -l && du -sh . && df -h /share | tail -1"
