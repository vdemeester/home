#!/usr/bin/env bash
# Display DNS zone entries generated from Nix configuration

set -euo pipefail

# Color codes for output
BOLD='\033[1m'
DIM='\033[2m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
RED='\033[0;31m'
RESET='\033[0m'

# Parse arguments
VERBOSE=0
HOST="${1:-demeter}"

if [[ "${1:-}" == "--verbose" ]] || [[ "${2:-}" == "--verbose" ]]; then
    VERBOSE=1
    HOST="${2:-${1:-demeter}}"
    if [[ "$HOST" == "--verbose" ]]; then
        HOST="demeter"
    fi
fi

# Define zones
ZONES=(
    "sbr.pm"
    "home"
    "vpn"
    "192.168.1.in-addr.arpa"
    "10.100.0.in-addr.arpa"
)

# Function to colorize DNS record type
colorize_type() {
    local type="$1"
    case "$type" in
        SOA) echo -e "${MAGENTA}${type}${RESET}" ;;
        NS) echo -e "${CYAN}${type}${RESET}" ;;
        A) echo -e "${GREEN}${type}${RESET}" ;;
        AAAA) echo -e "${GREEN}${type}${RESET}" ;;
        CNAME) echo -e "${YELLOW}${type}${RESET}" ;;
        MX) echo -e "${BLUE}${type}${RESET}" ;;
        TXT) echo -e "${DIM}${type}${RESET}" ;;
        PTR) echo -e "${CYAN}${type}${RESET}" ;;
        *) echo -e "${type}" ;;
    esac
}

# Function to parse and display zone in compact format
show_zone_compact() {
    local zone_name="$1"
    local zone_content="$2"

    echo -e "${BOLD}${BLUE}━━━ ${zone_name} ━━━${RESET}"

    # Count records by type
    local soa_count
    local ns_count
    local a_count
    local cname_count
    local ptr_count
    soa_count=$(echo "$zone_content" | grep -c " IN SOA " || true)
    ns_count=$(echo "$zone_content" | grep -c " IN NS " || true)
    a_count=$(echo "$zone_content" | grep -c " IN A " || true)
    cname_count=$(echo "$zone_content" | grep -c " IN CNAME " || true)
    ptr_count=$(echo "$zone_content" | grep -c " IN PTR " || true)

    # Show summary
    echo -e "${DIM}Records: SOA:${soa_count} NS:${ns_count} A:${a_count} CNAME:${cname_count} PTR:${ptr_count}${RESET}"
    echo

    # Parse and display records in organized format
    local record_type=""
    local prev_type=""

    while IFS= read -r line; do
        # Skip empty lines and TTL
        [[ -z "$line" || "$line" =~ ^\$TTL ]] && continue

        # Parse record type
        if [[ "$line" =~ IN\ (SOA|NS|A|AAAA|CNAME|MX|TXT|PTR) ]]; then
            record_type="${BASH_REMATCH[1]}"

            # Add spacing between different record types
            if [[ -n "$prev_type" && "$prev_type" != "$record_type" ]]; then
                echo
            fi
            prev_type="$record_type"

            # Format the line based on type
            if [[ "$record_type" == "SOA" ]]; then
                # SOA records - just show they exist
                echo -e "  $(colorize_type SOA) ${DIM}${line}${RESET}"
                continue
            elif [[ "$record_type" == "NS" ]]; then
                # NS records
                local name
                local target
                name=$(echo "$line" | awk '{print $1}')
                target=$(echo "$line" | awk '{print $NF}')
                printf "  %-30s $(colorize_type NS)  → %s\n" "$name" "$target"
            elif [[ "$record_type" == "A" ]]; then
                # A records - show name and IP
                local name
                local ip
                local ttl
                name=$(echo "$line" | awk '{print $1}')
                ip=$(echo "$line" | awk '{print $NF}')
                ttl=""

                # Check if there's a TTL
                if [[ "$line" =~ [0-9]+\ IN\ A ]]; then
                    ttl=$(echo "$line" | awk '{print $2}')
                    printf "  %-30s $(colorize_type A)    %s ${DIM}(TTL: %s)${RESET}\n" "$name" "$ip" "$ttl"
                else
                    printf "  %-30s $(colorize_type A)    %s\n" "$name" "$ip"
                fi
            elif [[ "$record_type" == "PTR" ]]; then
                # PTR records - reverse lookup
                local name
                local target
                name=$(echo "$line" | awk '{print $1}')
                target=$(echo "$line" | awk '{print $NF}')
                printf "  %-30s $(colorize_type PTR) → %s\n" "$name" "$target"
            elif [[ "$record_type" == "CNAME" ]]; then
                # CNAME records
                local name
                local target
                name=$(echo "$line" | awk '{print $1}')
                target=$(echo "$line" | awk '{print $NF}')
                printf "  %-30s $(colorize_type CNAME) → %s\n" "$name" "$target"
            fi
        fi
    done <<< "$zone_content"

    echo
}

# Function to show full zone
show_zone_verbose() {
    local zone_name="$1"
    local zone_content="$2"

    echo -e "${BOLD}${GREEN}═══ Zone: ${zone_name} ═══${RESET}"
    echo
    echo "$zone_content" | while IFS= read -r line; do
        if [[ "$line" =~ IN\ SOA ]]; then
            echo -e "${MAGENTA}${line}${RESET}"
        elif [[ "$line" =~ IN\ NS ]]; then
            echo -e "${CYAN}${line}${RESET}"
        elif [[ "$line" =~ IN\ A ]]; then
            echo -e "${GREEN}${line}${RESET}"
        elif [[ "$line" =~ IN\ CNAME ]]; then
            echo -e "${YELLOW}${line}${RESET}"
        elif [[ "$line" =~ IN\ PTR ]]; then
            echo -e "${CYAN}${line}${RESET}"
        elif [[ "$line" =~ IN\ (SRV|MX|TXT) ]]; then
            echo -e "${BLUE}${line}${RESET}"
        elif [[ "$line" =~ ^\$TTL ]]; then
            echo -e "${DIM}${line}${RESET}"
        else
            echo "$line"
        fi
    done
    echo
}

# Header
echo -e "${BOLD}${CYAN}DNS Zones for ${HOST}${RESET}"
echo -e "${DIM}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"
echo

# Process each zone
total_zones=0
failed_zones=0

for zone_name in "${ZONES[@]}"; do
    # Get the zone file content directly
    zone_content=$(nix eval --raw ".#nixosConfigurations.${HOST}.config.services.bind.zones.\"${zone_name}\".file" --apply 'path: builtins.readFile path' 2>&1 | grep -v "^warning:" | grep -v "^Using saved setting" | grep -v "^building " || true)

    if [[ -n "$zone_content" ]] && [[ "$zone_content" != *"error"* ]]; then
        total_zones=$((total_zones + 1))

        if [[ $VERBOSE -eq 1 ]]; then
            show_zone_verbose "$zone_name" "$zone_content"
        else
            show_zone_compact "$zone_name" "$zone_content"
        fi
    else
        failed_zones=$((failed_zones + 1))
        echo -e "${YELLOW}⚠ Could not generate zone ${zone_name}${RESET}"
        echo
    fi
done

# Summary
echo -e "${DIM}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"
echo -e "${BOLD}Summary:${RESET} Generated ${GREEN}${total_zones}${RESET} zones successfully"
[[ $failed_zones -gt 0 ]] && echo -e "         Failed to generate ${RED}${failed_zones}${RESET} zones"
echo

# Usage info
if [[ $VERBOSE -eq 0 ]]; then
    echo -e "${DIM}Tip: Use --verbose flag to see full zone files${RESET}"
fi
echo -e "${DIM}Usage: $0 [--verbose] [hostname]${RESET}"
