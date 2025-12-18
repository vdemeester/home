#!/usr/bin/env bash
# Handle incoming ntfy notification
# This script is called by ntfy for each incoming message
# It stores the message details and displays the notification

set -euo pipefail

# Store message details for later acknowledgment
STATEFILE="/tmp/ntfy-last-message"

# These variables (topic, id, title, message) are provided by ntfy as environment variables
# shellcheck disable=SC2154
cat >"$STATEFILE" <<EOF
TOPIC=$topic
ID=$id
TITLE=$title
MESSAGE=$message
EOF

# Display notification via mako
# shellcheck disable=SC2154
notify-send -a ntfy "$title" "$message"
