#!/usr/bin/env python3
"""
Shared library for *arr (Sonarr, Radarr, Lidarr) automation scripts.

Provides common functionality for API interaction, user confirmation,
and output formatting across all *arr stack scripts.
"""

import argparse
import sys
from typing import Any, Dict, List, Optional

import requests


class ArrClient:
    """Base client for *arr API interactions."""

    def __init__(self, base_url: str, api_key: str):
        """
        Initialize the *arr API client.

        Args:
            base_url: Base URL of the *arr service
                      (e.g., http://localhost:8989)
            api_key: API key for authentication
        """
        self.base_url = base_url.rstrip("/")
        self.api_key = api_key
        self.headers = {"X-Api-Key": api_key}

    def get(
        self, endpoint: str, params: Optional[Dict[str, Any]] = None
    ) -> List[Dict[str, Any]] | Dict[str, Any]:
        """
        Make a GET request to the *arr API.

        Args:
            endpoint: API endpoint path (e.g., /api/v3/series)
            params: Optional query parameters

        Returns:
            JSON response data

        Raises:
            SystemExit: If the request fails
        """
        url = f"{self.base_url}{endpoint}"

        try:
            response = requests.get(url, headers=self.headers, params=params)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(f"Error fetching from {endpoint}: {e}", file=sys.stderr)
            if params:
                print(f"  Params: {params}", file=sys.stderr)
            sys.exit(1)

    def post(
        self, endpoint: str, payload: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Make a POST request to the *arr API.

        Args:
            endpoint: API endpoint path (e.g., /api/v3/command)
            payload: JSON payload to send

        Returns:
            JSON response data (empty dict on failure)
        """
        url = f"{self.base_url}{endpoint}"
        headers = {**self.headers, "Content-Type": "application/json"}

        try:
            response = requests.post(url, headers=headers, json=payload)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(f"Error posting to {endpoint}: {e}", file=sys.stderr)
            return {}


def ask_confirmation(prompt: str) -> bool:
    """
    Ask user for yes/no confirmation.

    Args:
        prompt: Question to ask the user

    Returns:
        True if user confirms (y/yes), False otherwise (n/no)
    """
    while True:
        response = input(f"{prompt} (y/n): ").lower().strip()
        if response in ["y", "yes"]:
            return True
        elif response in ["n", "no"]:
            return False
        else:
            print("Please answer 'y' or 'n'")


def create_arr_parser(
    service_name: str, description: str, default_port: int
) -> argparse.ArgumentParser:
    """
    Create a standard argument parser for *arr scripts.

    Args:
        service_name: Name of the service (e.g., "Sonarr", "Radarr")
        description: Description for the script
        default_port: Default port for the service

    Returns:
        Configured ArgumentParser instance
    """
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument(
        f"{service_name.lower()}_url",
        metavar="url",
        help=(
            f"{service_name} base URL "
            f"(e.g., http://localhost:{default_port})"
        ),
    )
    parser.add_argument("api_key", help=f"{service_name} API key")
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be changed without making changes",
    )
    parser.add_argument(
        "--no-confirm",
        "--yolo",
        action="store_true",
        dest="no_confirm",
        help="Skip interactive confirmation (use with caution)",
    )
    return parser


def print_separator(char: str = "=", width: int = 80) -> None:
    """Print a separator line."""
    print(char * width)


def print_section_header(title: str) -> None:
    """Print a section header with separators."""
    print("\n" + "=" * 80)
    print(title)
    print("=" * 80)


def print_item_list(
    items: List[str], prefix: str, max_display: int = 5
) -> None:
    """
    Print a list of items with optional truncation.

    Args:
        items: List of item names to display
        prefix: Prefix message to show before the list
        max_display: Maximum number of items to show before truncating
    """
    if not items:
        return

    count = len(items)
    print(f"\n{prefix} ({count} items):")
    for item in items[:max_display]:
        print(f"  - {item}")
    if len(items) > max_display:
        remaining = len(items) - max_display
        print(f"  ... and {remaining} more")


def get_confirmation_decision(
    args: argparse.Namespace, prompt: str
) -> bool:
    """
    Determine whether to proceed based on dry-run, no-confirm, or user input.

    Args:
        args: Parsed command-line arguments
        prompt: Confirmation prompt to show user

    Returns:
        True if should proceed, False otherwise
    """
    if args.dry_run:
        print("\n[DRY RUN] Skipping actual operation")
        return False
    elif args.no_confirm:
        print("\n[NO CONFIRM] Proceeding with operation...")
        return True
    else:
        return ask_confirmation(prompt)


def print_final_summary(
    total: int,
    processed: int,
    skipped: int,
    operation: str,
    queue_note: bool = True,
) -> None:
    """
    Print final summary of operations.

    Args:
        total: Total items that needed processing
        processed: Number of items successfully processed
        skipped: Number of items skipped
        operation: Name of the operation (e.g., "Renamed", "Retagged")
        queue_note: Whether to show the queue check note
    """
    print_section_header("FINAL SUMMARY")
    print(f"\nItems processed: {total}")
    print(f"  - {operation}: {processed}")
    print(f"  - Skipped: {skipped}")

    if processed > 0 and queue_note:
        print(
            f"\nNote: {operation} operations are queued. "
            "Check the service's queue for progress."
        )
