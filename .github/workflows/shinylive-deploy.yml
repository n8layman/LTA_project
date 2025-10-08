name: Deploy Shinylive App

on:
  push:
    branches:
      - main
  workflow_dispatch:

jobs:
  deploy:
    runs-on: ubuntu-latest
    # Using a minimal R image ensures we start with very few packages installed.
    container:
      image: rocker/r-ver:4.4.1 

    permissions:
      contents: write
      pages: write
      id-token: write

    steps:
      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Cache Docker layers
        uses: actions/cache@v4
        with:
          path: /tmp/.buildx-cache
          key: ${{ runner.os }}-docker-${{ github.sha }}
          restore-keys: |
            ${{ runner.os }}-docker-

      - name: Checkout repository
        uses: actions/checkout@v4

      - name: Install system dependencies
        run: |
          apt-get update
          # Keep only the system libraries required by your specific R packages.
          apt-get install -y --no-install-recommends \
            libcurl4-openssl-dev \
            libssl-dev \
            libxml2-dev \
            libgdal-dev \
            libproj-dev \
            libgeos-dev \
            libudunits2-dev \
            cmake \
            gdal-bin \
            libglpk-dev \
            libpoppler-cpp-dev

      # --- REMOVING renv: Installing required packages directly instead ---
      - name: Install required R packages
        run: |
          # The app.R uses: shiny, leaflet, jsonlite, dplyr, DT, lubridate, 
          # leaflet.providers, leaflet.extras, readxl, htmltools.
          # shinylive is required for the export function.
          R -e "install.packages(c('shiny', 'leaflet', 'jsonlite', 'dplyr', 'DT', 'lubridate', 'leaflet.providers', 'leaflet.extras', 'readxl', 'htmltools', 'shinylive'), repos = 'https://cloud.r-project.org/')"

      - name: Export to shinylive
        run: |
          R -e "shinylive::export('app', '_site')"

      - name: Upload artifact
        uses: actions/upload-pages-artifact@v4

      - name: Deploy to GitHub Pages
        id: deployment
        uses: actions/deploy-pages@v4
