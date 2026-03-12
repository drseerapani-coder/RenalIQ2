# Use the official R-Shiny base image
FROM rocker/shiny:latest

# 1. Install System Dependencies 
# Added libsodium-dev for the 'sodium' package
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libtesseract-dev \
    tesseract-ocr \
    tesseract-ocr-eng \ 
    libpoppler-cpp-dev \
    libfontconfig1 \
    mesa-common-dev \
    libsodium-dev \
    && rm -rf /var/lib/apt/lists/*

# 2. Install R packages in layers
# Core clinical + auth packages
RUN R -e "install.packages(c('shiny', 'bslib', 'pool', 'DBI', 'RPostgres', \
    'dplyr', 'tidyr', 'lubridate', 'stringr', 'DT', 'shinyjs', \
    'rhandsontable', 'jsonlite', 'glue', 'pdftools', 'tesseract', \
    'httr', 'base64enc', 'shinycssloaders', 'sodium'), repos='https://cran.rstudio.com/')"

# Prescription export: writexl for Excel + googleCloudStorageR for GCS upload
# Service-account JSON is NOT baked into the image — inject at runtime via env var:
#   GOOGLE_SERVICE_ACCOUNT_JSON_CONTENT = <full JSON as a single-line string>
#   GCS_BUCKET                          = renaliq-prescriptions
RUN R -e "install.packages(c('writexl', 'googleCloudStorageR', 'blastula'), repos='https://cran.rstudio.com/')"

# Note: 'openai' R package removed — AI calls now go through httr directly,
# supporting both Ollama (local, default) and OpenAI (cloud fallback).

# 3. App Setup
RUN rm -rf /srv/shiny-server/*
WORKDIR /srv/shiny-server/

# Copy all files from your repo
COPY . /srv/shiny-server/

# Set permissions so the 'shiny' user can read the DB certificate and data files
RUN chmod 644 /srv/shiny-server/ca-certificate.crt || true
RUN chown -R shiny:shiny /srv/shiny-server/

# 4. Port & Startup
# DigitalOcean looks for port 8080 based on your appspec
USER shiny
EXPOSE 8080

# Direct startup for better reliability and health check response
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=8080)"]