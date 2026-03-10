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

for env_file in "$SCRIPT_DIR"/systemd/env/*.env; do
  install -m 0644 "$env_file" "/etc/ua-shiny/$(basename "$env_file")"
done

install -m 0644 "$SCRIPT_DIR/apache/ua-shiny.conf" /etc/httpd/conf.d/ua-shiny.conf

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
