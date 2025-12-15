#!/usr/bin/env -S uv run
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "websockets",
# ]
# ///
"""
Get Home Assistant entities via WebSocket API.
Usage: ./get-ha-entities.py <token>
"""

import asyncio
import websockets
import json
import sys
from collections import defaultdict

HA_WS = "ws://hass.home:8123/api/websocket"


async def get_entities(token):
    try:
        async with websockets.connect(HA_WS) as ws:
            # Receive auth_required
            auth_req = json.loads(await ws.recv())
            version = auth_req.get('ha_version', 'unknown')
            print(f"Connected to Home Assistant {version}")

            # Send authentication
            await ws.send(json.dumps({
                "type": "auth",
                "access_token": token
            }))

            # Receive auth response
            auth_resp = json.loads(await ws.recv())

            if auth_resp["type"] != "auth_ok":
                msg = auth_resp.get('message', 'Unknown error')
                print(f"❌ Authentication failed: {msg}")
                return

            print("✅ Authenticated successfully!\n")

            # Get all states
            await ws.send(json.dumps({
                "id": 1,
                "type": "get_states"
            }))

            response = json.loads(await ws.recv())

            if not response.get("success"):
                print(f"❌ Error getting states: {response}")
                return

            states = response["result"]

            # Group by domain
            by_domain = defaultdict(list)
            for state in states:
                domain = state["entity_id"].split(".")[0]
                by_domain[domain].append(state)

            # Display summary
            print(f"📊 Total entities: {len(states)}")
            print(f"📁 Domains: {len(by_domain)}\n")

            # Show each domain
            for domain in sorted(by_domain.keys()):
                entities = by_domain[domain]
                print(f"=== {domain} ({len(entities)} entities) ===")

                for entity in sorted(entities, key=lambda x: x["entity_id"]):
                    entity_id = entity["entity_id"]
                    state = entity["state"]
                    attrs = entity.get("attributes", {})
                    friendly_name = attrs.get("friendly_name", entity_id)
                    unit = attrs.get("unit_of_measurement", "")

                    state_display = f"{state} {unit}".strip()
                    name = friendly_name
                    print(f"  • {name} ({entity_id}): {state_display}")

                print()

    except websockets.exceptions.WebSocketException as e:
        print(f"❌ WebSocket error: {e}")
        print(f"   Make sure Home Assistant is accessible at {HA_WS}")
    except Exception as e:
        print(f"❌ Error: {e}")


def main():
    if len(sys.argv) < 2:
        print("Usage: ./get-ha-entities.py <long-lived-access-token>")
        print("")
        print("Create a token in Home Assistant:")
        print("  Profile → Long-Lived Access Tokens → Create Token")
        sys.exit(1)

    token = sys.argv[1]
    asyncio.run(get_entities(token))


if __name__ == "__main__":
    main()
