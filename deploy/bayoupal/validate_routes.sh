#!/usr/bin/env bash
set -euo pipefail

BASE_URL="${1:-https://bayoupal.nicholls.edu}"

ROUTES=(
  "/ua/home/"
  "/ua/training/"
  "/ua/staffing/"
  "/ua/job-crafting/"
  "/ua/fisher-2017/"
  "/ua/fisher-2020/"
  "/ua/sturman/"
  "/ua/portfolio/"
)

echo "Validating routes against: ${BASE_URL}"

if command -v rg >/dev/null 2>&1; then
  HINT_CMD='rg -o "<title>[^<]*</title>|<h1>[^<]*</h1>|Application Error|Service Unavailable|Not Found"'
else
  HINT_CMD='grep -Eo "<title>[^<]*</title>|<h1>[^<]*</h1>|Application Error|Service Unavailable|Not Found"'
fi

for route in "${ROUTES[@]}"; do
  url="${BASE_URL}${route}"
  code="$(curl -L -m 20 -s -o /tmp/ua_route_body.html -w "%{http_code}" "$url" || true)"
  hint="$(eval "$HINT_CMD /tmp/ua_route_body.html" | head -n 1 || true)"
  printf "%s | %s | %s\n" "$code" "$url" "$hint"
done
