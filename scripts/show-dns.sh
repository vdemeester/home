#!/usr/bin/env bash
# Display DNS zone entries generated from Nix configuration

set -euo pipefail

# Color codes for output
BOLD='\033[1m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
RESET='\033[0m'

# Default host to use for zone generation (must have bind configured)
HOST="${1:-demeter}"

echo -e "${BOLD}${BLUE}Generating DNS zones for ${CYAN}${HOST}${RESET}${BLUE}...${RESET}"
echo

# Define zones
ZONES=(
    "sbr.pm"
    "home"
    "vpn"
    "192.168.1.in-addr.arpa"
    "10.100.0.in-addr.arpa"
)

# Process each zone
for zone_name in "${ZONES[@]}"; do
    echo -e "${CYAN}Evaluating zone: ${zone_name}...${RESET}"

    # Get the zone file content directly
    zone_content=$(nix eval --raw ".#nixosConfigurations.${HOST}.config.services.bind.zones.\"${zone_name}\".file" 2>&1 | grep -v "^warning:" | grep -v "^Using saved setting")

    if [[ -n "$zone_content" ]] && [[ "$zone_content" != *"error"* ]]; then
        echo -e "${BOLD}${GREEN}=== Zone: ${zone_name} ===${RESET}"
        echo
        echo "$zone_content"
        echo
    else
        echo -e "${YELLOW}Could not generate zone ${zone_name}${RESET}"
        echo
    fi
done

echo -e "${BOLD}${GREEN}Done!${RESET}"
echo
echo -e "${CYAN}Usage: $0 [hostname]${RESET}"
echo -e "${CYAN}Example: make dns-show  # uses demeter by default${RESET}"
