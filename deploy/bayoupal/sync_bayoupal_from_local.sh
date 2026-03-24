#!/usr/bin/env bash
# Push local main to GitHub, then SSH to BayouPAL: git pull + restart UA Shiny services (+ optional full install).
#
# Requirements:
#   - SSH key access to the server (e.g. ssh ccastille@bayoupal)
#   - sudo on the server for git pull in /var/www/ua-plus and systemctl
#
# Usage (from repo root):
#   bash deploy/bayoupal/sync_bayoupal_from_local.sh
#   bash deploy/bayoupal/sync_bayoupal_from_local.sh --install
#
# Environment (optional):
#   UA_DEPLOY_SSH   default: ccastille@bayoupal
#   UA_DEPLOY_PATH  default: /var/www/ua-plus

set -euo pipefail

UA_DEPLOY_SSH="${UA_DEPLOY_SSH:-ccastille@bayoupal}"
UA_DEPLOY_PATH="${UA_DEPLOY_PATH:-/var/www/ua-plus}"
DO_INSTALL=0
if [[ "${1:-}" == "--install" ]]; then
  DO_INSTALL=1
fi
if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  grep '^#' "$0" | head -n 22
  exit 0
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$REPO_ROOT"

echo "==> Repository: $REPO_ROOT"

if [[ -n "$(git status --porcelain 2>/dev/null)" ]]; then
  echo ""
  echo "WARNING: Uncommitted changes. This script only runs git push; commit first for a clean deploy."
  read -r -p "Continue with git push? [y/N] " ans
  if [[ ! "${ans:-}" =~ ^[yY]$ ]]; then
    exit 1
  fi
fi

current_branch="$(git branch --show-current)"
if [[ "$current_branch" != "main" ]]; then
  echo "WARNING: Current branch is '$current_branch', not main." >&2
  read -r -p "Push to origin main anyway? [y/N] " ans
  if [[ ! "${ans:-}" =~ ^[yY]$ ]]; then
    exit 1
  fi
fi

echo "==> git push origin main"
git push origin main

echo "==> Remote: $UA_DEPLOY_SSH  path: $UA_DEPLOY_PATH"
echo "    (sudo will prompt — use Terminal.app/iTerm if Cursor has no TTY)"
echo "    After git pull, restarting httpd + 7 Shiny apps often takes 1–2 minutes — wait for 'remote finished'."
echo ""

# Note: Do not pipe a heredoc into ssh; that steals stdin and breaks -t (sudo cannot prompt).
# -tt forces a PTY even in some IDE-integrated terminals.
# Restart instances one-by-one so the terminal shows progress (batch can look "hung" for 1–2 min).
RESTART_INSTANCES="training staffing job-crafting fisher-2017 fisher-2020 sturman portfolio"

if [[ "$DO_INSTALL" -eq 1 ]]; then
  ssh -tt "$UA_DEPLOY_SSH" \
    "set -euo pipefail; cd $(printf '%q' "$UA_DEPLOY_PATH"); echo '==> git pull'; sudo git pull origin main --no-rebase; echo '==> install.sh --enable (can take a while)'; sudo bash deploy/bayoupal/install.sh $(printf '%q' "$UA_DEPLOY_PATH") --enable; echo '==> restart httpd'; sudo systemctl restart httpd; for s in $RESTART_INSTANCES; do echo \"==> restart ua-shiny@\$s\"; sudo systemctl restart ua-shiny@\$s; done; echo '==> remote finished'"
else
  ssh -tt "$UA_DEPLOY_SSH" \
    "set -euo pipefail; cd $(printf '%q' "$UA_DEPLOY_PATH"); echo '==> git pull'; sudo git pull origin main --no-rebase; echo '==> restart httpd'; sudo systemctl restart httpd; for s in $RESTART_INSTANCES; do echo \"==> restart ua-shiny@\$s\"; sudo systemctl restart ua-shiny@\$s; done; echo '==> remote finished'"
fi

echo ""
echo "==> Done. Spot-check: https://bayoupal.nicholls.edu/ua/home/"
