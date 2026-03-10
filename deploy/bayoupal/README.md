# BayouPAL deployment pack (UA apps)

This folder contains a ready-to-apply deployment bundle for hosting the UA Shiny apps on `bayoupal.nicholls.edu` behind Apache.

## What this bundle deploys

Path-based URLs:

- `https://bayoupal.nicholls.edu/ua/training/`
- `https://bayoupal.nicholls.edu/ua/staffing/`
- `https://bayoupal.nicholls.edu/ua/staffing-fixed/`
- `https://bayoupal.nicholls.edu/ua/job-crafting/`
- `https://bayoupal.nicholls.edu/ua/fisher-2017/`
- `https://bayoupal.nicholls.edu/ua/fisher-2020/`
- `https://bayoupal.nicholls.edu/ua/sturman/`

Backed by systemd services:

- `ua-shiny@training.service`
- `ua-shiny@staffing.service`
- `ua-shiny@staffing-fixed.service`
- `ua-shiny@job-crafting.service`
- `ua-shiny@fisher-2017.service`
- `ua-shiny@fisher-2020.service`
- `ua-shiny@sturman.service`

## Files in this folder

- `install.sh` - installs unit files/env files/apache config
- `run_shiny_app.sh` - service launcher used by systemd
- `systemd/ua-shiny@.service` - templated service unit
- `systemd/env/*.env` - per-app script/port mapping
- `apache/ua-shiny.conf` - Apache reverse-proxy rules

## Server prerequisites (RHEL/Apache)

1. Install OS packages:

   - `httpd`
   - `mod_ssl`
   - `R`
   - Apache proxy modules (`mod_proxy`, `mod_proxy_http`, `mod_proxy_wstunnel`, `mod_headers`, `mod_rewrite`)

2. Install R packages:

   - `shiny`, `shinydashboard`, `ggplot2`, `dplyr`, `scales`, `DT`, `plotly`, `tidyr`, `rmarkdown`, `knitr`, `ggtext`, `gridExtra`

3. Deploy the repository to a server path (default expected path):

   - `/var/www/ua-plus`

## Install and enable

From repository root on the server:

1. Install configs only:

   - `sudo bash deploy/bayoupal/install.sh /var/www/ua-plus`

2. Install and immediately enable services:

   - `sudo bash deploy/bayoupal/install.sh /var/www/ua-plus --enable`

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

- `curl -I https://bayoupal.nicholls.edu/ua/training/`
- `curl -I https://bayoupal.nicholls.edu/ua/fisher-2017/`

## Updating app code

1. Pull latest repo changes into your app directory.
2. Restart only affected services, for example:

   - `sudo systemctl restart ua-shiny@training`

## Notes

- `install.sh` writes env files to `/etc/ua-shiny`.
- Edit `/etc/ua-shiny/*.env` to change app script paths or ports.
- If your Apache setup uses custom vhosts, include `apache/ua-shiny.conf` in the active SSL vhost.
