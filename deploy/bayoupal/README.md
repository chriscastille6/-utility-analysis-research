# BayouPAL deployment pack (UA apps)

This folder contains a ready-to-apply deployment bundle for hosting the UA Shiny apps on `bayoupal.nicholls.edu` behind Apache.

## Canonical Shiny entrypoints (do not drift)

Production **`APP_SCRIPT`** paths are the source of truth. The repo also contains **`shiny_apps/`** copies of some apps for local experiments; **editing only `shiny_apps/...` will not change BayouPAL** unless you sync the same changes into the canonical file below (or change the env file—discouraged).

| systemd instance | Public route | Canonical `APP_SCRIPT` (under `APP_BASE`, e.g. `/var/www/ua-plus`) |
|------------------|--------------|---------------------------------------------------------------------|
| `ua-shiny@training` | `/ua/training/` | `training_utility_app.R` |
| `ua-shiny@staffing` | `/ua/staffing/` | `staffing_utility_app.R` |
| `ua-shiny@job-crafting` | `/ua/job-crafting/` | `job_crafting_utility_app.R` |
| `ua-shiny@fisher-2017` | `/ua/fisher-2017/` | `fisher_connelly_2017_app.R` |
| `ua-shiny@fisher-2020` | `/ua/fisher-2020/` | `fisher_connelly_2020_app.R` |
| `ua-shiny@sturman` | `/ua/sturman/` | `scripts/features/sturman_2003_app.R` |
| `ua-shiny@portfolio` | `/ua/portfolio/` | `simulation_portfolio_app.R` |

**Duplicate / mirror paths (not used by default systemd):** e.g. `shiny_apps/staffing_utility_full/app.R` vs root `staffing_utility_app.R` — treat as forks until consolidated.

## Deploy checklist (after code changes)

1. **Pull** on the server: `cd /var/www/ua-plus && sudo git pull origin main`
2. **Restart** only services you changed: `sudo systemctl restart ua-shiny@<name>` (see table above)
3. If you changed **Apache** or **added a route**: re-run `sudo bash deploy/bayoupal/install.sh /var/www/ua-plus --enable` (or install configs + `sudo systemctl restart httpd`) and ensure new `ua-shiny@*` is enabled
4. **Optional:** `bash deploy/bayoupal/validate_routes.sh`

Set **`UA_REPO_ROOT=/var/www/ua-plus`** in the environment if Shiny’s working directory is ever not the repo root (shared modules use it to find `scripts/shiny_modules/`).

## One-command push + restart (from your Mac)

Script: [`sync_bayoupal_from_local.sh`](sync_bayoupal_from_local.sh)

1. **Commit** your changes on `main` (the script does not commit for you).
2. From the **repo root**:

```bash
./deploy/bayoupal/sync_bayoupal_from_local.sh
```

This runs `git push origin main`, then SSHs to the server, `sudo git pull origin main --no-rebase`, restarts `httpd`, and restarts all `ua-shiny@*` instances listed in the script.

- **Apache / new systemd env / first-time portfolio:** use  
  `./deploy/bayoupal/sync_bayoupal_from_local.sh --install`  
  (runs `install.sh ... --enable` on the server).

- **Override SSH user/host or path:**  
  `UA_DEPLOY_SSH=you@yourhost UA_DEPLOY_PATH=/var/www/ua-plus ./deploy/bayoupal/sync_bayoupal_from_local.sh`

You still need **SSH access** and will usually be prompted for **sudo** on the server unless you configure passwordless sudo for those commands. Fully unattended deploy requires CI (e.g. GitHub Actions with a deploy key) or a pull-based hook on the server—not something this repo configures by default.

If you see **“Pseudo-terminal will not be allocated”** / **sudo: a terminal is required**, the script uses **`ssh -tt`** and an inline remote command (not a heredoc) so your **keyboard** stays the TTY for sudo. Run the script from **Terminal.app** or **iTerm** if your IDE terminal still cannot allocate a PTY.

## What this bundle deploys

Path-based URLs:

- `https://bayoupal.nicholls.edu/ua/` (redirects to homepage)
- `https://bayoupal.nicholls.edu/ua/home/` (homepage)
- `https://bayoupal.nicholls.edu/ua/training/`
- `https://bayoupal.nicholls.edu/ua/staffing/`
- `https://bayoupal.nicholls.edu/ua/job-crafting/`
- `https://bayoupal.nicholls.edu/ua/fisher-2017/`
- `https://bayoupal.nicholls.edu/ua/fisher-2020/`
- `https://bayoupal.nicholls.edu/ua/sturman/`
- `https://bayoupal.nicholls.edu/ua/portfolio/` (simulation portfolio — joint decisions over time)

Backed by systemd services:

- `ua-shiny@training.service`
- `ua-shiny@staffing.service`
- `ua-shiny@job-crafting.service`
- `ua-shiny@fisher-2017.service`
- `ua-shiny@fisher-2020.service`
- `ua-shiny@sturman.service`
- `ua-shiny@portfolio.service`

## Files in this folder

- `install.sh` - installs unit files/env files/apache config
- `sync_bayoupal_from_local.sh` - from your Mac: `git push` + SSH pull + restart services (optional `--install`)
- `run_shiny_app.sh` - service launcher used by systemd
- `validate_routes.sh` - checks all public UA route health
- `systemd/ua-shiny@.service` - templated service unit
- `systemd/env/*.env` - per-app script/port mapping
- `apache/ua-shiny.conf` - Apache reverse-proxy rules
- `homepage/index.html` - static landing page served at `/ua/home/`

## Server prerequisites (RHEL/Apache)

1. Install OS packages:

   - `httpd`
   - `mod_ssl`
   - `R`
   - `texlive`
   - `texlive-xetex`
   - Apache proxy modules (`mod_proxy`, `mod_proxy_http`, `mod_proxy_wstunnel`, `mod_headers`, `mod_rewrite`)

2. Install R packages:

   - `shiny`, `shinydashboard`, `shinyjs`, `ggplot2`, `dplyr`, `scales`, `DT`, `plotly`, `tidyr`, `rmarkdown`, `knitr`, `ggtext`, `gridExtra`, `mvtnorm`, `stringr`, `iopsych`, `lavaan`

3. Deploy the repository to a server path (default expected path):

   - `/var/www/ua-plus`

## Install and enable

From repository root on the server:

1. Install configs only:

   - `sudo bash deploy/bayoupal/install.sh /var/www/ua-plus`

2. Install and immediately enable services:

   - `sudo bash deploy/bayoupal/install.sh /var/www/ua-plus --enable`

Notes:

- `install.sh` now checks for `pdflatex` and `xelatex` (required for PDF report downloads).
- On supported systems, it attempts to install LaTeX automatically.
- To skip auto-install and only deploy configs/services:
  - `sudo bash deploy/bayoupal/install.sh /var/www/ua-plus --enable --skip-latex-install`

3. Restart Apache if needed:

   - `sudo systemctl restart httpd`

## Validate deployment

Local service checks:

- `sudo systemctl status ua-shiny@training`
- `sudo systemctl status ua-shiny@staffing`
- `sudo systemctl status ua-shiny@job-crafting`

Port checks:

- `curl -I http://127.0.0.1:3841/`
- `curl -I http://127.0.0.1:3844/`

Public URL checks:

- `curl -I https://bayoupal.nicholls.edu/ua/home/`
- `curl -I https://bayoupal.nicholls.edu/ua/training/`
- `curl -I https://bayoupal.nicholls.edu/ua/fisher-2017/`
- `bash deploy/bayoupal/validate_routes.sh`

If a route returns `503`:

- `sudo journalctl -u ua-shiny@staffing -n 120 --no-pager`
- `sudo systemctl restart ua-shiny@staffing`

Legacy note:

- `/ua/staffing-fixed/` now redirects to `/ua/staffing/`.
- Running `install.sh ... --enable` disables `ua-shiny@staffing-fixed` if it exists.

## Updating app code

1. Pull latest repo changes into your app directory.
2. Restart only affected services, for example:

   - `sudo systemctl restart ua-shiny@training`

## Notes

- `install.sh` writes env files to `/etc/ua-shiny`.
- Edit `/etc/ua-shiny/*.env` to change app script paths or ports.
- If your Apache setup uses custom vhosts, include `apache/ua-shiny.conf` in the active SSL vhost.
