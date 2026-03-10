#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_BASE="/var/www/ua-plus"
ENABLE_NOW=false
SKIP_LATEX_INSTALL=false

usage() {
  cat <<'USAGE'
Usage: install.sh [APP_BASE] [--enable] [--skip-latex-install]

Examples:
  sudo bash deploy/bayoupal/install.sh /var/www/ua-plus
  sudo bash deploy/bayoupal/install.sh /var/www/ua-plus --enable
USAGE
}

APP_BASE_SET=false
for arg in "$@"; do
  case "$arg" in
    --enable)
      ENABLE_NOW=true
      ;;
    --skip-latex-install)
      SKIP_LATEX_INSTALL=true
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --*)
      echo "Unknown option: $arg" >&2
      usage >&2
      exit 64
      ;;
    *)
      if [[ "$APP_BASE_SET" == true ]]; then
        echo "Unexpected extra argument: $arg" >&2
        usage >&2
        exit 64
      fi
      APP_BASE="$arg"
      APP_BASE_SET=true
      ;;
  esac
done

if [[ "$(id -u)" -ne 0 ]]; then
  echo "Run as root (use sudo)." >&2
  exit 1
fi

if [[ ! -d "$APP_BASE" ]]; then
  echo "App base directory does not exist: $APP_BASE" >&2
  exit 1
fi

ensure_latex_runtime() {
  local missing=()
  local still_missing=()

  command -v pdflatex >/dev/null 2>&1 || missing+=("pdflatex")
  command -v xelatex >/dev/null 2>&1 || missing+=("xelatex")

  if [[ ${#missing[@]} -eq 0 ]]; then
    echo "LaTeX runtime check: OK (pdflatex and xelatex found)."
    return 0
  fi

  echo "LaTeX runtime check: missing ${missing[*]}."

  if [[ "$SKIP_LATEX_INSTALL" == true ]]; then
    echo "Skipping automatic LaTeX install (--skip-latex-install)."
  else
    if command -v dnf >/dev/null 2>&1; then
      echo "Attempting LaTeX install with dnf (texlive + texlive-xetex)..."
      if dnf install -y texlive texlive-xetex; then
        echo "LaTeX packages installed via dnf."
      else
        echo "WARNING: dnf install failed; continuing with deployment." >&2
      fi
    elif command -v yum >/dev/null 2>&1; then
      echo "Attempting LaTeX install with yum (texlive + texlive-xetex)..."
      if yum install -y texlive texlive-xetex; then
        echo "LaTeX packages installed via yum."
      else
        echo "WARNING: yum install failed; continuing with deployment." >&2
      fi
    elif command -v apt-get >/dev/null 2>&1; then
      echo "Attempting LaTeX install with apt-get (texlive-xetex)..."
      if apt-get update && apt-get install -y texlive-xetex; then
        echo "LaTeX packages installed via apt-get."
      else
        echo "WARNING: apt-get install failed; continuing with deployment." >&2
      fi
    else
      echo "WARNING: no supported package manager found for automatic LaTeX install." >&2
    fi
  fi

  command -v pdflatex >/dev/null 2>&1 || still_missing+=("pdflatex")
  command -v xelatex >/dev/null 2>&1 || still_missing+=("xelatex")
  if [[ ${#still_missing[@]} -gt 0 ]]; then
    echo "WARNING: still missing ${still_missing[*]}." >&2
    echo "PDF report downloads will fail until LaTeX is installed." >&2
    echo "Manual fix on RHEL: sudo dnf install -y texlive texlive-xetex" >&2
  else
    echo "LaTeX runtime check: OK after installation."
  fi
}

ensure_latex_runtime

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
  ua-shiny@job-crafting
  ua-shiny@fisher-2017
  ua-shiny@fisher-2020
  ua-shiny@sturman
)

if [[ "$ENABLE_NOW" == true ]]; then
  systemctl enable --now "${SERVICES[@]}"
  # One-time migration: retire the legacy staffing-fixed service.
  if systemctl list-unit-files | grep -q '^ua-shiny@staffing-fixed\.service'; then
    systemctl disable --now ua-shiny@staffing-fixed >/dev/null 2>&1 || true
  fi
  systemctl restart httpd
  echo "Services enabled and started."
else
  echo "Installed configs."
  echo "Next steps:"
  echo "  systemctl enable --now ${SERVICES[*]}"
  echo "  systemctl restart httpd"
fi
