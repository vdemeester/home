"""
Manage Lidarr queue items with interactive selection.

This script:
1. Fetches all items from the Lidarr queue
2. Filters for items that need manual import or have errors
3. Displays them in an interactive selector (fzf)
4. Allows removal of selected queue items
"""

import json
import subprocess
import sys
from typing import Any, Dict, List

from lib import (
    ArrClient,
    CommandContext,
    get_confirmation_decision,
    print_section_header,
)


def get_queue_items(
    client: ArrClient,
    include_unknown_artist: bool = True,
    page_size: int = 1000,
) -> List[Dict[str, Any]]:
    """
    Fetch all items from the Lidarr queue.

    Args:
        client: ArrClient instance
        include_unknown_artist: Include items without matched artists
        page_size: Number of items per page

    Returns:
        List of queue item dictionaries
    """
    params = {
        "page": 1,
        "pageSize": page_size,
        "includeUnknownArtistItems": include_unknown_artist,
        "includeAlbum": True,
        "includeArtist": True,
    }
    response = client.get("/api/v1/queue", params=params)

    # Handle paginated response
    if isinstance(response, dict):
        return response.get("records", [])
    return response


def delete_queue_item(
    client: ArrClient,
    queue_id: int,
    remove_from_client: bool = True,
    blocklist: bool = False,
    skip_redownload: bool = False,
) -> bool:
    """
    Delete a queue item.

    Args:
        client: ArrClient instance
        queue_id: Queue item ID to delete
        remove_from_client: Remove from download client
        blocklist: Add to blocklist
        skip_redownload: Skip automatic redownload

    Returns:
        True if successful
    """
    params = {
        "removeFromClient": str(remove_from_client).lower(),
        "blocklist": str(blocklist).lower(),
        "skipRedownload": str(skip_redownload).lower(),
    }

    # Use requests directly for DELETE with query params
    import requests

    url = f"{client.base_url}/api/v1/queue/{queue_id}"
    try:
        response = requests.delete(
            url, headers=client.headers, params=params, timeout=30
        )
        response.raise_for_status()
        return True
    except requests.exceptions.RequestException as e:
        print(f"Error deleting queue item {queue_id}: {e}")
        return False


def format_queue_item(item: Dict[str, Any]) -> str:
    """
    Format a queue item for display.

    Args:
        item: Queue item dictionary

    Returns:
        Formatted string for display
    """
    queue_id = item.get("id", "?")
    status = item.get("status", "unknown")
    error_message = item.get("errorMessage", "")

    # Get artist and album info if available
    artist_name = "Unknown Artist"
    album_title = "Unknown Album"

    if "artist" in item and item["artist"]:
        artist_name = item["artist"].get("artistName", artist_name)

    if "album" in item and item["album"]:
        album_title = item["album"].get("title", album_title)

    # Get quality info
    quality = "Unknown"
    if "quality" in item and item["quality"]:
        quality_profile = item["quality"].get("quality", {})
        quality = quality_profile.get("name", "Unknown")

    # Get size info
    size_str = ""
    size = item.get("size", 0)
    sizeleft = item.get("sizeleft", 0)
    if size > 0:
        size_mb = size / (1024 * 1024)
        if sizeleft > 0:
            percent = ((size - sizeleft) / size) * 100
            size_str = f"{size_mb:.1f}MB ({percent:.0f}%)"
        else:
            size_str = f"{size_mb:.1f}MB"

    # Get protocol and download client
    protocol = item.get("protocol", "")
    download_client = item.get("downloadClient", "")

    # Get tracked download info
    tracked_status = item.get("trackedDownloadStatus", "")

    # Determine status indicator
    status_icon = "⚠️"
    if status == "warning":
        status_icon = "⚠️"
    elif status == "error":
        status_icon = "❌"
    elif "completed" in status.lower():
        status_icon = "✓"
    elif "downloading" in status.lower():
        status_icon = "⬇"
    elif "manual" in status.lower():
        status_icon = "🔧"

    # Build display string
    parts = [
        f"[{queue_id}]",
        status_icon,
        f"{artist_name} - {album_title}",
    ]

    # Add quality and size
    details = []
    if quality != "Unknown":
        details.append(quality)
    if size_str:
        details.append(size_str)
    if protocol:
        details.append(protocol.upper())
    if download_client:
        details.append(f"via {download_client}")

    if details:
        parts.append(f"[{' | '.join(details)}]")

    # Add status
    status_parts = [status]
    if tracked_status and tracked_status != status:
        status_parts.append(tracked_status)
    parts.append(f"({', '.join(status_parts)})")

    if error_message:
        # Truncate long error messages
        error_short = (
            error_message[:60] + "..."
            if len(error_message) > 60
            else error_message
        )
        parts.append(f"- {error_short}")

    return " ".join(parts)


def format_queue_item_preview(item: Dict[str, Any]) -> str:
    """
    Format a detailed preview of a queue item.

    Args:
        item: Queue item dictionary

    Returns:
        Formatted preview string with full details
    """
    lines = []

    # Header
    queue_id = item.get("id", "?")
    title = item.get("title", "Unknown")
    lines.append("=" * 80)
    lines.append(f"QUEUE ITEM #{queue_id}")
    lines.append("=" * 80)
    lines.append("")

    # Basic Info
    lines.append("BASIC INFO:")
    lines.append(f"  Title: {title}")

    if "artist" in item and item["artist"]:
        artist_name = item["artist"].get("artistName", "Unknown")
        lines.append(f"  Artist: {artist_name}")

    if "album" in item and item["album"]:
        album_title = item["album"].get("title", "Unknown")
        release_date = item["album"].get("releaseDate", "Unknown")
        lines.append(f"  Album: {album_title}")
        lines.append(f"  Release Date: {release_date}")

    lines.append("")

    # Download Info
    lines.append("DOWNLOAD INFO:")
    status = item.get("status", "unknown")
    lines.append(f"  Status: {status}")

    tracked_status = item.get("trackedDownloadStatus", "")
    if tracked_status:
        lines.append(f"  Tracked Status: {tracked_status}")

    tracked_state = item.get("trackedDownloadState", "")
    if tracked_state:
        lines.append(f"  Tracked State: {tracked_state}")

    protocol = item.get("protocol", "")
    if protocol:
        lines.append(f"  Protocol: {protocol.upper()}")

    download_client = item.get("downloadClient", "")
    if download_client:
        lines.append(f"  Download Client: {download_client}")

    # Size info
    size = item.get("size", 0)
    sizeleft = item.get("sizeleft", 0)
    if size > 0:
        size_mb = size / (1024 * 1024)
        lines.append(f"  Total Size: {size_mb:.2f} MB")
        if sizeleft > 0:
            sizeleft_mb = sizeleft / (1024 * 1024)
            percent = ((size - sizeleft) / size) * 100
            downloaded_mb = size_mb - sizeleft_mb
            lines.append(
                f"  Downloaded: {downloaded_mb:.2f} MB ({percent:.1f}%)"
            )
            lines.append(f"  Remaining: {sizeleft_mb:.2f} MB")

    lines.append("")

    # Quality Info
    if "quality" in item and item["quality"]:
        lines.append("QUALITY:")
        quality_profile = item["quality"].get("quality", {})
        quality_name = quality_profile.get("name", "Unknown")
        lines.append(f"  Quality: {quality_name}")
        lines.append("")

    # Output Path
    output_path = item.get("outputPath", "")
    if output_path:
        lines.append("OUTPUT PATH:")
        lines.append(f"  {output_path}")
        lines.append("")

    # Error/Warning Messages
    error_message = item.get("errorMessage", "")
    if error_message:
        lines.append("ERROR MESSAGE:")
        lines.append(f"  {error_message}")
        lines.append("")

    status_messages = item.get("statusMessages", [])
    if status_messages:
        lines.append("STATUS MESSAGES:")
        for msg in status_messages:
            msg_title = msg.get("title", "")
            msg_messages = msg.get("messages", [])
            if msg_title:
                lines.append(f"  • {msg_title}")
            for m in msg_messages:
                lines.append(f"    - {m}")
        lines.append("")

    # Download ID
    download_id = item.get("downloadId", "")
    if download_id:
        lines.append("DOWNLOAD ID:")
        lines.append(f"  {download_id}")
        lines.append("")

    # Timestamps
    lines.append("TIMESTAMPS:")
    added = item.get("added", "")
    if added:
        lines.append(f"  Added: {added}")

    estimated_completion = item.get("estimatedCompletionTime", "")
    if estimated_completion:
        lines.append(f"  Estimated Completion: {estimated_completion}")

    return "\n".join(lines)


def select_queue_items_with_preview(
    items: List[Dict[str, Any]]
) -> List[str]:
    """
    Use fzf with preview to interactively select queue items.

    Args:
        items: List of queue item dictionaries

    Returns:
        List of selected item IDs (empty list if cancelled)
    """
    if not items:
        return []

    # Create a temporary mapping file for preview
    import tempfile

    # Create lookup table: display text -> item data
    lookup = {}
    lines = []

    for item in items:
        item_id = str(item.get("id"))
        display = format_queue_item(item)
        lines.append(display)
        lookup[display] = {
            "id": item_id,
            "preview": format_queue_item_preview(item)
        }

    # Prepare fzf input
    fzf_input = "\n".join(lines)

    # Create a temporary script for preview
    with tempfile.NamedTemporaryFile(
        mode='w', suffix='.json', delete=False
    ) as f:
        # Store the lookup data
        preview_data = {
            display: data["preview"] for display, data in lookup.items()
        }
        json.dump(preview_data, f)
        preview_file = f.name

    try:
        # Run fzf with preview
        # We'll use a simple approach: pass the preview text directly
        fzf_args = [
            "fzf",
            "--ansi",
            "--multi",
            "--prompt=Select queue items (TAB to select, ENTER to confirm): ",
            "--preview=echo {}",
            "--preview-window=right:60%:wrap",
            "--bind=ctrl-/:toggle-preview",
        ]

        # Create a Python script for preview
        import os
        preview_script = tempfile.NamedTemporaryFile(
            mode='w', suffix='.py', delete=False
        )
        preview_script.write(f"""#!/usr/bin/env python3
import json
import sys

preview_file = {repr(preview_file)}
line = sys.argv[1] if len(sys.argv) > 1 else ""

try:
    with open(preview_file, 'r') as f:
        data = json.load(f)
    print(data.get(line, "No preview available"))
except Exception as e:
    print(f"Error: {{e}}")
""")
        preview_script.close()
        os.chmod(preview_script.name, 0o755)

        prompt = (
            "Select queue items "
            "(TAB to select, ENTER to confirm, Ctrl-/ to toggle preview): "
        )
        header = (
            "TAB: select | ENTER: confirm | Ctrl-/: toggle preview | "
            "Ctrl-↑/↓: scroll preview | ESC: cancel"
        )
        fzf_args = [
            "fzf",
            "--ansi",
            "--multi",
            f"--prompt={prompt}",
            f"--preview={preview_script.name} {{}}",
            "--preview-window=right:60%:wrap",
            "--bind=ctrl-/:toggle-preview",
            "--bind=ctrl-up:preview-page-up",
            "--bind=ctrl-down:preview-page-down",
            "--bind=ctrl-u:preview-half-page-up",
            "--bind=ctrl-d:preview-half-page-down",
            f"--header={header}"
        ]

        result = subprocess.run(
            fzf_args,
            input=fzf_input,
            text=True,
            capture_output=True,
            check=True,
        )

        # Parse selected lines
        selected_lines = result.stdout.strip().split("\n")
        selected_ids = [
            lookup[line]["id"]
            for line in selected_lines
            if line in lookup
        ]

        return selected_ids

    except subprocess.CalledProcessError:
        # User cancelled or fzf not found
        return []
    except FileNotFoundError:
        print("Error: fzf not found. Please install fzf:", file=sys.stderr)
        print(
            "  On NixOS: nix-env -iA nixpkgs.fzf",
            file=sys.stderr
        )
        print(
            "  On other systems: see https://github.com/junegunn/fzf",
            file=sys.stderr
        )
        sys.exit(1)
    finally:
        # Clean up temporary files
        import os
        try:
            os.unlink(preview_file)
            os.unlink(preview_script.name)
        except Exception:
            pass


def filter_queue_items(
    items: List[Dict[str, Any]],
    filter_type: str = "all",
    tracked_state: str = None
) -> List[Dict[str, Any]]:
    """
    Filter queue items by type and tracked state.

    Args:
        items: List of queue items
        filter_type: Filter type - 'all', 'manual', 'warning',
                     'error', 'completed'
        tracked_state: Specific tracked download state to filter by
                      (e.g., 'importFailed', 'imported', 'importing')

    Returns:
        Filtered list of queue items
    """
    filtered = items if filter_type == "all" else []

    # First apply the filter_type filter
    if filter_type != "all":
        for item in items:
            status = item.get("status", "").lower()
            error_message = item.get("errorMessage", "").lower()
            tracked_download_status = item.get("trackedDownloadStatus", "")
            tracked_download_status = tracked_download_status.lower()

            if filter_type == "manual":
                # Items that need manual import
                if (
                    "warning" in status
                    or "manual" in tracked_download_status
                    or "manual import" in error_message
                ):
                    filtered.append(item)
            elif filter_type == "warning":
                if "warning" in status:
                    filtered.append(item)
            elif filter_type == "error":
                if "error" in status or item.get("errorMessage"):
                    filtered.append(item)
            elif filter_type == "completed":
                if (
                    "completed" in status
                    or "completed" in tracked_download_status
                ):
                    filtered.append(item)

    # Then apply the tracked_state filter if specified
    if tracked_state:
        tracked_state_lower = tracked_state.lower()
        filtered = [
            item
            for item in filtered
            if item.get("trackedDownloadState", "").lower()
            == tracked_state_lower
        ]

    return filtered


def run(
    url: str,
    api_key: str,
    filter_type: str,
    tracked_state: str,
    remove_from_client: bool,
    blocklist: bool,
    skip_redownload: bool,
    dry_run: bool,
    no_confirm: bool,
):
    """Execute the lidarr manage-queue command."""
    # Create client and context
    client = ArrClient(url, api_key)
    ctx = CommandContext(dry_run, no_confirm)

    print_section_header("FETCHING LIDARR QUEUE")
    print(f"Connecting to {client.base_url}...")

    all_items = get_queue_items(client)
    print(f"Found {len(all_items)} total queue items")

    if not all_items:
        print("\nQueue is empty!")
        return

    # Filter items based on filter type and tracked state
    filtered_items = filter_queue_items(all_items, filter_type, tracked_state)

    # Build filter description
    filter_desc_parts = []
    if filter_type != "all":
        filter_desc_parts.append(f"type: {filter_type}")
    if tracked_state:
        filter_desc_parts.append(f"tracked state: {tracked_state}")

    if filter_desc_parts:
        filter_desc = ", ".join(filter_desc_parts)
        print(
            f"Filtered to {len(filtered_items)} items "
            f"({filter_desc})"
        )

    if not filtered_items:
        filter_msg = f"filter '{filter_type}'"
        if tracked_state:
            filter_msg = (
                f"filters (type: {filter_type}, "
                f"tracked state: {tracked_state})"
            )
        print(f"\nNo items matching {filter_msg}")
        return

    print_section_header("QUEUE ITEMS")

    # Display summary by status
    status_counts = {}
    for item in filtered_items:
        status = item.get("status", "unknown")
        status_counts[status] = status_counts.get(status, 0) + 1

    print("\nItems by status:")
    for status, count in sorted(status_counts.items()):
        print(f"  - {status}: {count}")

    # Interactive selection with preview
    print("\nOpening interactive selector with preview...")
    print(
        "Controls: TAB=select | Ctrl-/=toggle preview | "
        "Ctrl-↑/↓=scroll preview | ENTER=confirm"
    )

    selected_ids = select_queue_items_with_preview(filtered_items)

    if not selected_ids:
        print("\nNo items selected. Exiting.")
        return

    print(f"\n{len(selected_ids)} item(s) selected for removal")

    # Show what will be removed
    print("\nItems to remove:")
    for item_id in selected_ids:
        item = next(
            (i for i in filtered_items if str(i.get("id")) == item_id), None
        )
        if item:
            print(f"  - {format_queue_item(item)}")

    # Show removal options
    print("\nRemoval options:")
    print(f"  - Remove from download client: {remove_from_client}")
    print(f"  - Add to blocklist: {blocklist}")
    print(f"  - Skip automatic redownload: {skip_redownload}")

    # Confirm deletion
    prompt = f"\nRemove {len(selected_ids)} item(s) from queue?"
    should_proceed = get_confirmation_decision(ctx, prompt)

    if not should_proceed:
        print("Cancelled.")
        return

    # Delete selected items
    print_section_header("REMOVING QUEUE ITEMS")

    success_count = 0
    failed_count = 0

    for item_id in selected_ids:
        queue_id = int(item_id)
        item = next(
            (i for i in filtered_items if i.get("id") == queue_id), None
        )
        item_title = item.get("title", f"ID {queue_id}") if item else queue_id

        print(f"\nRemoving: {item_title}")

        if delete_queue_item(
            client,
            queue_id,
            remove_from_client,
            blocklist,
            skip_redownload,
        ):
            print("  ✓ Removed successfully")
            success_count += 1
        else:
            print("  ✗ Failed to remove")
            failed_count += 1

    # Final summary
    print_section_header("FINAL SUMMARY")
    print(f"\nItems selected: {len(selected_ids)}")
    print(f"  - Successfully removed: {success_count}")
    print(f"  - Failed: {failed_count}")

    if success_count > 0:
        print("\n✓ Queue items removed successfully")
