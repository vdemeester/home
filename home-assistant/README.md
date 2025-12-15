# Home Assistant Dashboard Configuration

This directory contains declarative YAML dashboard configurations for Home Assistant.

## Structure

```
home-assistant/
├── ui-lovelace.yaml           # Main dashboard file (entry point)
├── views/                     # Dashboard views (pages)
│   ├── overview.yaml          # Home overview
│   ├── media.yaml             # Media services
│   ├── downloads.yaml         # Download management
│   ├── infrastructure.yaml    # Infrastructure monitoring
│   └── automation.yaml        # Automation & devices
├── cards/                     # Reusable card components
│   ├── media-cards.yaml
│   ├── download-cards.yaml
│   └── infrastructure-cards.yaml
├── configuration-example.yaml # Example sensor configurations
├── secrets-example.yaml       # Example secrets file
└── get-entities.py            # Script to retrieve entities via API
```

## Installation

### 1. Enable YAML Mode in Home Assistant

Add to your Home Assistant `configuration.yaml`:

```yaml
lovelace:
  mode: yaml
  resources:
    # Add any custom Lovelace cards here
    - url: /local/custom-cards/mini-graph-card-bundle.js
      type: module
```

### 2. Link Dashboard Files

Copy or symlink the `ui-lovelace.yaml` file to your Home Assistant config directory:

```bash
# If Home Assistant is running on hass (192.168.1.181)
ssh hass.home
cd /config  # or wherever your HA config lives
ln -s /path/to/this/repo/home-assistant/ui-lovelace.yaml .
```

Or copy the entire `home-assistant/` directory into your Home Assistant config.

### 3. Reload Dashboard

In Home Assistant:
- Go to Settings → Dashboards
- Click the three dots → "Reload resources"
- Or restart Home Assistant

## Customization

### Adding Services

Edit the relevant view file in `views/` to add new services.

### Modifying Layout

The dashboard uses a grid layout with responsive columns. Adjust `grid_columns` and `grid_min_rows` in view files.

### Custom Cards

Install custom cards via HACS (Home Assistant Community Store) and add them to the `resources` section in `configuration.yaml`.

## Recommended Custom Cards

- **mini-graph-card**: Beautiful graphs for sensors
- **button-card**: Highly customizable buttons
- **layout-card**: Advanced layout control
- **auto-entities**: Dynamic card generation

## Version Control

These files are version controlled in the main homelab repository. Changes should be:

1. Made in this repository
2. Tested locally
3. Deployed to Home Assistant
4. Committed with descriptive messages

## API Access

### Retrieving Entities

Use the `get-entities.py` script to retrieve all entities from Home Assistant via the WebSocket API:

```bash
# Create a long-lived access token in Home Assistant:
# Profile → Long-Lived Access Tokens → Create Token

# Run the script
./get-entities.py YOUR_TOKEN_HERE
```

The script will:
- Connect to Home Assistant at `hass.home:8123`
- Authenticate with your token
- Retrieve all entities grouped by domain
- Display entity states, friendly names, and units

This is useful for:
- Discovering available entities for dashboard configuration
- Debugging sensor configurations
- Monitoring entity states
- Generating entity lists for automations

See the [Home Assistant API documentation](https://developers.home-assistant.io/docs/api/websocket/) for more details.

## Notes

- Service URLs use the `sbr.pm` domain (accessible via Traefik)
- All services are HTTPS via Traefik reverse proxy
- Monitor availability with `ping` sensors
- Add API keys for service widgets where needed
