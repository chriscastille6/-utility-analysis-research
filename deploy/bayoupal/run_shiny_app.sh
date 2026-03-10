#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 <absolute_app_script_path> <port>" >&2
  exit 64
fi

APP_SCRIPT="$1"
APP_PORT="$2"

if [[ ! -f "$APP_SCRIPT" ]]; then
  echo "App script not found: $APP_SCRIPT" >&2
  exit 66
fi

if [[ ! "$APP_PORT" =~ ^[0-9]+$ ]]; then
  echo "Port must be numeric: $APP_PORT" >&2
  exit 65
fi

exec /usr/bin/Rscript -e "options(shiny.host='0.0.0.0', shiny.port=${APP_PORT}, shiny.launch.browser=FALSE); shiny::runApp('${APP_SCRIPT}')"
