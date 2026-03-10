#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_BASE="${1:-/var/www/ua-plus}"
ENABLE_NOW="${2:-}"

if [[ "$(id -u)" -ne 0 ]]; then
  echo "Run as root (use sudo)." >&2
  exit 1
fi

if [[ ! -d "$APP_BASE" ]]; then
  echo "App base directory does not exist: $APP_BASE" >&2
  exit 1
fi

mkdir -p /etc/ua-shiny
mkdir -p /etc/systemd/system
mkdir -p /etc/httpd/conf.d

TMP_UNIT="$(mktemp)"
trap 'rm -f "$TMP_UNIT"' EXIT

sed "s|__APP_BASE__|$APP_BASE|g" \
  "$SCRIPT_DIR/systemd/ua-shiny@.service" > "$TMP_UNIT"

install -m 0644 "$TMP_UNIT" /etc/systemd/system/ua-shiny@.service
RUNNER_SRC="$SCRIPT_DIR/run_shiny_app.sh"
RUNNER_DST="$APP_BASE/deploy/bayoupal/run_shiny_app.sh"
if [[ "$RUNNER_SRC" != "$RUNNER_DST" ]]; then
  install -m 0755 "$RUNNER_SRC" "$RUNNER_DST"
fi

VALIDATOR_SRC="$SCRIPT_DIR/validate_routes.sh"
VALIDATOR_DST="$APP_BASE/deploy/bayoupal/validate_routes.sh"
if [[ "$VALIDATOR_SRC" != "$VALIDATOR_DST" ]]; then
  install -m 0755 "$VALIDATOR_SRC" "$VALIDATOR_DST"
fi

HOME_SRC="$SCRIPT_DIR/homepage"
HOME_DST="$APP_BASE/deploy/bayoupal/homepage"
if [[ -d "$HOME_SRC" ]]; then
  mkdir -p "$HOME_DST"
  if [[ "$HOME_SRC" != "$HOME_DST" ]]; then
    cp -a "$HOME_SRC"/. "$HOME_DST"/
  fi
fi

for env_file in "$SCRIPT_DIR"/systemd/env/*.env; do
  install -m 0644 "$env_file" "/etc/ua-shiny/$(basename "$env_file")"
done

TMP_APACHE="$(mktemp)"
trap 'rm -f "$TMP_UNIT" "$TMP_APACHE"' EXIT
sed "s|__APP_BASE__|$APP_BASE|g" \
  "$SCRIPT_DIR/apache/ua-shiny.conf" > "$TMP_APACHE"
install -m 0644 "$TMP_APACHE" /etc/httpd/conf.d/ua-shiny.conf

systemctl daemon-reload

SERVICES=(
  ua-shiny@training
  ua-shiny@staffing
  ua-shiny@staffing-fixed
  ua-shiny@job-crafting
  ua-shiny@fisher-2017
  ua-shiny@fisher-2020
  ua-shiny@sturman
)

if [[ "$ENABLE_NOW" == "--enable" ]]; then
  systemctl enable --now "${SERVICES[@]}"
  systemctl restart httpd
  echo "Services enabled and started."
else
  echo "Installed configs."
  echo "Next steps:"
  echo "  systemctl enable --now ${SERVICES[*]}"
  echo "  systemctl restart httpd"
fi
