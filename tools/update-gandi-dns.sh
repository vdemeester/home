#!/usr/bin/env bash
# Update Gandi DNS records from NixOS DNS configuration
# Usage: ./scripts/update-gandi-dns.sh [--dry-run]

set -euo pipefail

# Colors
BOLD='\033[1m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
CYAN='\033[0;36m'
RESET='\033[0m'

DOMAIN="sbr.pm"
API_URL="https://api.gandi.net/v5/livedns/domains/$DOMAIN/records"
DRY_RUN=false

# Parse arguments
for arg in "$@"; do
    case $arg in
        --dry-run)
            DRY_RUN=true
            shift
            ;;
    esac
done

# Check for API key (using lego's environment variable name)
if [[ -z "${GANDIV5_PERSONAL_TOKEN:-}" ]]; then
    echo -e "${RED}Error: GANDIV5_PERSONAL_TOKEN environment variable not set${RESET}"
    echo -e "${YELLOW}Please set it with: export GANDIV5_PERSONAL_TOKEN=your-api-key${RESET}"
    echo -e "${CYAN}Or source it from the agenix secret on rhea:${RESET}"
    echo -e "${CYAN}  source /run/agenix/gandi.env${RESET}"
    exit 1
fi

echo -e "${BOLD}${BLUE}Updating Gandi DNS records for $DOMAIN...${RESET}"
if [[ "$DRY_RUN" == "true" ]]; then
    echo -e "${YELLOW}DRY RUN MODE - No changes will be made${RESET}"
fi
echo

# Get the DNS zone file content from Nix
echo -e "${CYAN}Extracting DNS records from Nix configuration...${RESET}"
ZONE_FILE=$(nix eval --raw '.#nixosConfigurations.demeter.config.services.bind.zones."sbr.pm".file' --apply 'path: builtins.readFile path' 2>&1 | \
           grep -v "^warning:" | grep -v "^Using saved setting" | grep -v "^building ")

if [[ -z "$ZONE_FILE" ]]; then
    echo -e "${RED}Error: Could not generate zone file${RESET}"
    exit 1
fi

echo -e "${GREEN}DNS records extracted${RESET}"
echo

# Create a temporary file for the zone
TEMP_ZONE=$(mktemp)
echo "$ZONE_FILE" > "$TEMP_ZONE"

# Extract A records (excluding SOA, NS, and comments)
# Format examples:
#   name.domain. IN A ip
#   name.domain. TTL IN A ip
#   *.name.domain. IN A ip
RECORDS=$(grep -E "^\S+\s+(([0-9]+\s+)?IN\s+)?A\s+" "$TEMP_ZONE" | \
          grep -v "SOA" | \
          grep -v "^;" || true)

rm -f "$TEMP_ZONE"

if [[ -z "$RECORDS" ]]; then
    echo -e "${YELLOW}No A records found in zone file${RESET}"
    exit 0
fi

echo -e "${GREEN}Found $(echo "$RECORDS" | wc -l) A records to process${RESET}"
echo

# Get current DNS records from Gandi
if [[ "$DRY_RUN" == "false" ]]; then
    echo -e "${CYAN}Fetching current DNS records from Gandi...${RESET}"
    CURRENT_RECORDS=$(curl -s \
      -H "Authorization: Bearer $GANDIV5_PERSONAL_TOKEN" \
      "$API_URL" || echo "[]")

    echo -e "${GREEN}Current records fetched${RESET}"
    echo
fi

# Process each record
UPDATED=0
SKIPPED=0
FAILED=0

while IFS= read -r line; do
    # Parse the line to extract: name, TTL, and value
    # Handle various formats:
    #   name.sbr.pm. IN A value
    #   name.sbr.pm. TTL IN A value
    #   *.name.sbr.pm. TTL IN A value

    # Extract components
    FULL_NAME=$(echo "$line" | awk '{print $1}')

    # Check if second field is a number (TTL) or "IN"
    SECOND_FIELD=$(echo "$line" | awk '{print $2}')
    if [[ "$SECOND_FIELD" =~ ^[0-9]+$ ]]; then
        TTL="$SECOND_FIELD"
        VALUE=$(echo "$line" | awk '{print $5}')
    else
        TTL=10800
        VALUE=$(echo "$line" | awk '{print $4}')
    fi

    # Remove .sbr.pm. suffix and convert to Gandi format
    NAME="${FULL_NAME%.sbr.pm.}"

    # Convert wildcard format: *.name -> *.name (Gandi uses this format)
    # Convert root wildcard: * -> @ (Gandi's root wildcard)
    if [[ "$NAME" == "*" ]]; then
        NAME="@"
    fi

    echo -e "${BLUE}Processing: ${RESET}$NAME.$DOMAIN A $VALUE (TTL: $TTL)"

    if [[ "$DRY_RUN" == "true" ]]; then
        echo -e "  ${CYAN}[DRY RUN] Would update/create record${RESET}"
        UPDATED=$((UPDATED + 1))
    else
        # Check if record exists and has same value
        CURRENT_VALUE=$(echo "$CURRENT_RECORDS" | jq -r \
          --arg name "$NAME" \
          --arg type "A" \
          '.[] | select(.rrset_name == $name and .rrset_type == $type) | .rrset_values[0]' \
          2>/dev/null || echo "")

        if [[ "$CURRENT_VALUE" == "$VALUE" ]]; then
            echo -e "  ${GREEN}✓ Record unchanged, skipping${RESET}"
            SKIPPED=$((SKIPPED + 1))
        else
            if [[ -z "$CURRENT_VALUE" ]]; then
                echo -e "  ${YELLOW}Creating new record...${RESET}"
            else
                echo -e "  ${YELLOW}Updating record (was: $CURRENT_VALUE)...${RESET}"
            fi

            # Update/create the record
            RESPONSE=$(curl -s -w "\n%{http_code}" \
              -X PUT \
              -H "Authorization: Bearer $GANDIV5_PERSONAL_TOKEN" \
              -H "Content-Type: application/json" \
              -d "{\"rrset_values\": [\"$VALUE\"], \"rrset_ttl\": $TTL}" \
              "$API_URL/$NAME/A")

            HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
            BODY=$(echo "$RESPONSE" | sed '$d')

            if [[ "$HTTP_CODE" == "201" ]] || [[ "$HTTP_CODE" == "200" ]]; then
                echo -e "  ${GREEN}✓ Record updated successfully${RESET}"
                UPDATED=$((UPDATED + 1))
            else
                echo -e "  ${RED}✗ Failed to update record (HTTP $HTTP_CODE)${RESET}"
                echo -e "  ${RED}Response: $BODY${RESET}"
                FAILED=$((FAILED + 1))
            fi
        fi
    fi

    echo
done <<< "$RECORDS"

echo -e "${BOLD}${GREEN}DNS update complete!${RESET}"
echo
echo -e "${CYAN}Summary:${RESET}"
echo -e "  Updated: $UPDATED"
echo -e "  Skipped (unchanged): $SKIPPED"
if [[ $FAILED -gt 0 ]]; then
    echo -e "  ${RED}Failed: $FAILED${RESET}"
fi
