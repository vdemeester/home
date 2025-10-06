#!/usr/bin/env -S uv run --script
# /// script
# dependencies = [
#     "requests",
#     "PyYAML",
#     "google-generativeai",
#
# ]
# ///

import os.path
import subprocess
import socket
import sys
from typing import Any, Dict, List, Optional

import requests
import yaml  # pip install pyyaml types-pyyaml
import urllib.parse as urlparse
import google.generativeai as genai


def debug(msg: str):
    print(f"[DEBUG] {msg}", file=sys.stderr)


def check_running(api_base, timeout=0.5) -> bool:
    """Quickly check if Ollama is accessible at the given host and port."""
    url = urlparse.urlparse(api_base)
    port = url.port or (80 if url.scheme == "http" else 443)
    debug(f"Checking if {url.hostname}:{port} is running (api_base={api_base})")
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(timeout)
        result = sock.connect_ex((url.hostname, port))
        sock.close()
        debug(f"Socket connect_ex result for {url.hostname}:{port}: {result}")
        return result == 0
    except Exception as e:
        debug(f"Exception in check_running: {e}")
        raise e


def load_config(config_path: str) -> Dict[str, Any]:
    debug(f"Loading config from {config_path}")
    with open(config_path, "r") as file:
        config = yaml.safe_load(file)
        debug(f"Loaded config: {config}")
        return config


def get_models(api_base: str, api_key_config: str | None) -> List[Dict[str, str]]:
    """Query the models endpoint and return a list of model data"""
    debug(
        f"get_models called with api_base={api_base}, api_key_config={api_key_config}"
    )
    actual_api_key = None
    if api_key_config:
        if api_key_config.startswith("passage::"):
            passage_path = api_key_config.split("::", 1)[1]
            debug(f"Retrieving API key from passage at {passage_path}")
            actual_api_key = get_passageword(passage_path)
            if not actual_api_key:
                print(
                    f"Could not retrieve API key from passage for path: {passage_path}",
                    file=sys.stderr,
                )
                debug(f"Failed to retrieve API key from passage for {passage_path}")
                # Decide how to handle failure: skip, return empty, etc.
                # Here we'll proceed without a key, which might fail later.
        else:
            actual_api_key = api_key_config  # Use the key directly if not a passage path
            debug("Using API key directly from config")

    headers = {}
    if actual_api_key:
        headers["Authorization"] = f"Bearer {actual_api_key}"
        debug("Authorization header set")

    # Ensure the URL is properly formatted
    if not api_base.endswith("/"):
        api_base = api_base + "/"
        debug(f"api_base adjusted to {api_base}")

    models_url = f"{api_base}models"
    debug(f"Querying models endpoint: {models_url}")

    try:
        response = requests.get(models_url, headers=headers, timeout=10)
        debug(f"HTTP GET {models_url} status_code={response.status_code}")
        response.raise_for_status()
        data = response.json()
        debug(f"Response JSON: {data}")

        # Extract models from response
        models = data.get("data", [])
        debug(f"Extracted models: {models}")
        return [
            {"name": model.get("id"), "description": model.get("id")}
            for model in models
        ]
    except Exception as e:
        print(f"Error querying {api_base}: {str(e)}", file=sys.stderr)
        debug(f"Exception in get_models: {e}")
        return []


def get_gemini_models(api_key: Optional[str]) -> List[Dict[str, str]]:
    """Query Google's Gemini API and return a list of available models"""
    debug(f"get_gemini_models called with api_key={'***' if api_key else None}")
    if not api_key:
        print("Error: API key is required for Google Gemini API", file=sys.stderr)
        debug("No API key provided to get_gemini_models")
        return []

    try:
        # Configure the Gemini API with the provided key
        debug("Configuring genai with provided API key")
        genai.configure(api_key=api_key)

        # Get list of available models
        debug("Listing models from genai")
        models_list = genai.list_models()
        debug(f"Models list: {models_list}")

        # Filter for Gemini models
        gemini_models = [
            {"name": model.name.split("/")[-1], "description": model.description}
            for model in models_list
            if "gemini" in model.name.lower()
        ]
        debug(f"Filtered Gemini models: {gemini_models}")

        return gemini_models
    except Exception as e:
        print(f"Error querying Google Gemini API: {str(e)}", file=sys.stderr)
        debug(f"Exception in get_gemini_models: {e}")
        return []


def get_passageword(passage_path: str) -> str | None:
    """Retrieve passageword from passage using the given path."""
    debug(f"get_passageword called for passage_path={passage_path}")
    try:
        result = subprocess.run(
            ["passage", "show", passage_path], capture_output=True, text=True, check=True
        )
        # Return the first line of the output, stripping newline
        passageword = result.stdout.splitlines()[0]
        debug(f"Passageword retrieved from passage for {passage_path}")
        return passageword
    except FileNotFoundError:
        print(
            "Error: 'passage' command not found. Is passage installed and in your PATH?",
            file=sys.stderr,
        )
        debug("'passage' command not found")
        return None
    except subprocess.CalledProcessError as e:
        print(f"Error running passage show {passage_path}: {e.stderr}", file=sys.stderr)
        debug(f"subprocess.CalledProcessError in get_passageword: {e}")
        return None
    except IndexError:
        print(f"Error: 'passage show {passage_path}' returned empty output.", file=sys.stderr)
        debug(f"IndexError: passage show {passage_path} returned empty output")
        return None


def main():
    config_path = os.path.expanduser("~/.config/aichat/config.yaml.in")
    debug(f"main: config_path={config_path}")
    config_data = load_config(config_path)

    if "clients" in config_data:
        updated_clients = []
        debug(f"main: found {len(config_data.get('clients', []))} clients")
        for client in config_data.get("clients", []):
            # Make a copy to avoid modifying the original dict during iteration if needed elsewhere
            updated_client = client.copy()
            debug(f"Processing client: {updated_client}")

            actual_api_key = None
            api_key_config = updated_client.get("api_key")
            if api_key_config and api_key_config.startswith("passage::"):
                passage_path = api_key_config.split("::", 1)[1]
                debug(f"main: retrieving api_key from passage for {passage_path}")
                actual_api_key = get_passageword(passage_path)
            else:
                actual_api_key = api_key_config
            updated_client["api_key"] = actual_api_key
            debug("main: actual_api_key set for client")

            # For OpenAI-compatible clients, query and potentially add models
            if updated_client.get("type") == "openai-compatible":
                # Check if models are NOT already defined in config or are empty
                if not updated_client.get("models"):
                    api_base = updated_client.get("api_base")
                    api_key = updated_client.get("api_key")
                    debug(
                        f"main: openai-compatible client, api_base={api_base}, api_key={'***' if api_key else None}"
                    )

                    # Skip ollama explicitly if needed, or handle based on your logic
                    if api_base:
                        if not check_running(api_base):
                            debug(f"main: {api_base} not running, skipping client")
                            continue
                        # Try to fetch models from API
                        fetched_models = get_models(api_base, api_key)
                        if fetched_models:
                            updated_client["models"] = fetched_models
                            debug("main: models fetched and set for client")
                        else:
                            # Keep models empty/undefined or add an empty list
                            updated_client["models"] = []
                            debug("main: no models fetched, set empty list")
                    else:
                        # Handle cases where type is openai-compatible but no api_base
                        updated_client["models"] = []
                        debug(
                            "main: openai-compatible client with no api_base, set empty models"
                        )

            # For Google Gemini clients, query and potentially add models
            elif updated_client.get("type") == "gemini":
                # Check if models are NOT already defined in config or are empty
                if not updated_client.get("models"):
                    api_key = updated_client.get("api_key")
                    debug(f"main: gemini client, api_key={'***' if api_key else None}")
                    fetched_models = get_gemini_models(api_key)
                    if fetched_models:
                        updated_client["models"] = fetched_models
                        debug("main: gemini models fetched and set for client")
                    else:
                        updated_client["models"] = []
                        debug("main: no gemini models fetched, set empty list")

            updated_clients.append(updated_client)
            debug("main: client processed and added to updated_clients")

        # Replace the original clients list with the updated one
        config_data["clients"] = updated_clients
        debug("main: updated_clients set in config_data")

    # Print the entire (potentially updated) configuration as YAML
    debug("main: dumping config_data as YAML")
    print(yaml.dump(config_data, default_flow_style=False, sort_keys=False))


if __name__ == "__main__":
    main()
