#!/usr/bin/env python3
#
# A command line tool to be used as an AWS `credential_process` that retrieves
# NASA Earthdata login credentials from 1Password, uses them to obtain
# temporary AWS credentials, and provides them to the AWS CLI/SDK.
#
# For first-time use, run this script interactively in your terminal:
# python3 /path/to/aws-earthdata-creds.py

import base64
import configparser
import json
import os
import shutil
import subprocess
import sys
from datetime import datetime, timedelta, timezone
from http.cookiejar import CookieJar
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.request import HTTPCookieProcessor, Request, build_opener

# The NASA endpoint that provides temporary AWS credentials.
S3_CREDENTIALS_ENDPOINT = "https://data.lpdaac.earthdatacloud.nasa.gov/s3credentials"

# A directory to cache the temporary credentials, mimicking AWS SSO's cache location.
CACHE_DIR = Path.home() / ".aws" / "sso" / "cache"


def die(message: str) -> None:
    """Print an error message to stderr and exits with a non-zero status."""
    print(message, file=sys.stderr)
    sys.exit(1)


def get_op_credential(item_id, field):
    """Retrieve a specific field for an item from the 1Password CLI."""
    command: list[str] = ["op", "item", "get", item_id, f"--fields={field}"]
    if field == "password":
        command.append("--reveal")

    try:
        result = subprocess.run(
            command, capture_output=True, text=True, check=True, encoding="utf-8"
        )
        return result.stdout.strip()
    except FileNotFoundError:
        die(
            "Error: 'op' command not found. Is the 1Password CLI installed and in your PATH?"
        )
    except subprocess.CalledProcessError as e:
        die(f"Error retrieving '{field}' from 1Password: {e.stderr.strip()}")


def fetch_earthdata_credentials(username, password):
    """Authenticate with Earthdata Login to get temporary AWS credentials."""
    cookie_jar = CookieJar()
    opener = build_opener(HTTPCookieProcessor(cookie_jar))

    try:
        # This multi-step process uses a shared cookie jar. The opener will
        # automatically follow redirects and attach the necessary cookies.
        # 1. First, access the endpoint to get redirected to the Earthdata login page.
        login_page = opener.open(S3_CREDENTIALS_ENDPOINT)

        # 2. Post the base64-encoded credentials to the login page URL.
        auth_str = f"{username}:{password}".encode("ascii")
        encoded_auth = base64.b64encode(auth_str).decode("ascii")
        post_data = f"credentials={encoded_auth}".encode("utf-8")
        auth_req = Request(
            login_page.url, data=post_data, headers={"Origin": S3_CREDENTIALS_ENDPOINT}
        )
        opener.open(auth_req)

        # 3. Finally, access the original endpoint again. The cookie jar now has the
        # necessary session token, which will grant access to the credentials.
        final_resp = opener.open(S3_CREDENTIALS_ENDPOINT)
        return json.loads(final_resp.read())

    except (URLError, HTTPError) as e:
        die(f"Error during network request to Earthdata: {e}")
    except json.JSONDecodeError:
        die("Error: Failed to parse JSON response from Earthdata.")


def format_credentials_for_aws(earthdata_creds):
    """Convert Earthdata credential format to the AWS credential_process format."""
    # Earthdata provides an expiration like: "2021-01-27 00:50:09+00:00"
    # AWS credential_process expects a strict ISO8601 format: "YYYY-MM-DDTHH:MM:SSZ"
    dt_obj = datetime.fromisoformat(earthdata_creds["expiration"])
    expiration_iso = dt_obj.astimezone(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    return {
        "Version": 1,
        "AccessKeyId": earthdata_creds["accessKeyId"],
        "SecretAccessKey": earthdata_creds["secretAccessKey"],
        "SessionToken": earthdata_creds["sessionToken"],
        "Expiration": expiration_iso,
    }


def run_credential_process(op_item_id):
    """Handles the core logic when called by the AWS CLI."""
    cache_file = CACHE_DIR / f"nasa-earthdata-creds-{op_item_id}.json"

    if cache_file.exists():
        try:
            with open(cache_file, "r", encoding="utf-8") as f:
                cached_creds = json.load(f)

            # AWS expects expiration in ISO 8601 format with a 'Z' for UTC.
            # Python's fromisoformat needs this replaced to parse correctly.
            expiration_str = cached_creds["Expiration"].replace("Z", "+00:00")
            expiration = datetime.fromisoformat(expiration_str)

            # Check if credentials expire in more than 5 minutes to avoid race conditions.
            if expiration > datetime.now(timezone.utc) + timedelta(minutes=5):
                print(json.dumps(cached_creds))
                sys.exit(0)
        except (json.JSONDecodeError, KeyError, OSError):
            # If cache is invalid or unreadable, just proceed to fetch new credentials.
            pass

    username = get_op_credential(op_item_id, "username")
    password = get_op_credential(op_item_id, "password")

    earthdata_creds = fetch_earthdata_credentials(username, password)
    aws_creds = format_credentials_for_aws(earthdata_creds)

    try:
        CACHE_DIR.mkdir(parents=True, exist_ok=True)
        # Write to a private file (readable/writable only by the user).
        with open(cache_file, "w", encoding="utf-8") as f:
            json.dump(aws_creds, f)
        os.chmod(cache_file, 0o600)
    except OSError as e:
        # A failed cache write is not a fatal error; we can still provide the credentials.
        print(
            f"Warning: Could not write to cache file {cache_file}: {e}", file=sys.stderr
        )

    print(json.dumps(aws_creds))


def run_interactive_setup() -> None:
    """Guide a user through creating the necessary AWS profile configuration."""
    print("--- NASA Earthdata AWS Credential Helper Setup ---")
    print("This script will configure an AWS profile to use your Earthdata Login")
    print("credentials, retrieved from 1Password, to get temporary AWS access.")
    print("-" * 50)

    try:
        item_id = input(
            "Enter the 1Password item ID for your Earthdata login: "
        )
        profile_name = input(
            "Enter a name for the new AWS profile (default: 'earthdata'): "
        )
        if not profile_name:
            profile_name = 'earthdata'
    except KeyboardInterrupt:
        print("\nSetup cancelled.")
        sys.exit(1)

    if not item_id or not profile_name:
        die("Error: Both 1Password item ID and profile name are required.")

    aws_config_path = Path.home() / ".aws" / "config"
    aws_config_path.parent.mkdir(exist_ok=True)

    if aws_config_path.exists():
        timestamp = datetime.now().strftime("%Y-%m-%dT%H%M%S")
        backup_path = aws_config_path.with_suffix(f".{timestamp}.bak")
        try:
            shutil.copy2(aws_config_path, backup_path)
            print(f"Backed up existing config to {backup_path}")
        except OSError as e:
            die(f"Error creating backup of AWS config: {e}")

    config = configparser.ConfigParser()
    if aws_config_path.exists():
        config.read(aws_config_path)

    section_name = f"profile {profile_name}"
    if config.has_section(section_name):
        print(
            f"Warning: Profile '{profile_name}' already exists and will be overwritten."
        )

    script_path = Path(__file__).resolve()
    # Per AWS docs, quote the path and arguments to handle spaces safely.
    credential_process_command = f'"{script_path}" "{item_id}"'

    config[section_name] = {
        "region": "us-west-1",
        "credential_process": credential_process_command,
    }

    try:
        with aws_config_path.open("w", encoding="utf-8") as configfile:
            config.write(configfile)
        print("\nSuccess! Your AWS config file has been updated.")
        print("You can now use this profile with the AWS CLI, for example:")
        print(f"  aws s3 ls --profile {profile_name}")
    except OSError as e:
        die(f"Error writing to AWS config file: {e}")


if __name__ == "__main__":
    # If called with an argument, act as a credential_process.
    if len(sys.argv) > 1:
        if len(sys.argv) != 2:
            die(f"Usage as credential_process: {sys.argv[0]} <1password_item_id>")
        op_item_id = sys.argv[1]
        run_credential_process(op_item_id)
    # If called with no arguments in an interactive terminal, run setup.
    elif sys.stdout.isatty():
        run_interactive_setup()
    # If piped or called non-interactively with no arguments, show an error.
    else:
        die(
            "This script is meant to be run interactively for setup or invoked by the AWS CLI."
        )
