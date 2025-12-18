#!/usr/bin/env bash
# Acknowledge (delete) the last ntfy notification
# This script is called when right-clicking a notification in mako

set -euo pipefail

STATEFILE="/tmp/ntfy-last-message"
LOGFILE="/tmp/ntfy-actions.log"

# Log the action
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Acknowledge action triggered" >> "$LOGFILE"

# Check if we have a message to acknowledge
if [[ ! -f "$STATEFILE" ]]; then
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: No message to acknowledge" >> "$LOGFILE"
    notify-send -a ntfy "Error" "No message to acknowledge"
    exit 1
fi

# Read message details
# shellcheck source=/dev/null
source "$STATEFILE"

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Acknowledging message: $TOPIC/$ID" >> "$LOGFILE"

# Delete the message from ntfy server
if curl -s -X DELETE "https://ntfy.sbr.pm/$TOPIC/$ID" \
    -H "Authorization: Bearer $(passage show home/ntfy/token)" \
    >> "$LOGFILE" 2>&1; then
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Successfully deleted message" >> "$LOGFILE"
    rm -f "$STATEFILE"
else
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Failed to delete message" >> "$LOGFILE"
fi
