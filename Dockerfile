# Use the official R-Shiny base image
FROM rocker/shiny:latest

# 1. Install System Dependencies 
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
    && rm -rf /var/lib/apt/lists/*

# 2. Install R packages in layers
# Layer 1: Core Tidyverse and DB (Stable)
RUN R -e "install.packages(c('shiny', 'bslib', 'pool', 'DBI', 'RPostgres', \
    'dplyr', 'tidyr', 'lubridate', 'stringr', 'DT', 'shinyjs', \
    'rhandsontable', 'jsonlite', 'glue', 'pdftools', 'tesseract', \
    'openai', 'shinycssloaders'), repos='https://cran.rstudio.com/')"
# Layer 2: UI Components (rhandsontable can be picky about dependencies)

RUN R -e "install.packages(c('shiny', 'bslib', 'shinyjs', 'DT', 'rhandsontable', 'shinycssloaders'), repos='https://cran.rstudio.com/')"

# Layer 3: AI and OCR (Heavy lifting)
RUN R -e "install.packages(c('pdftools', 'tesseract', 'openai'), repos='https://cran.rstudio.com/')"


# 3. App Setup
RUN rm -rf /srv/shiny-server/*
WORKDIR /srv/shiny-server/

# Copy all files
COPY . /srv/shiny-server/

# --- NEW: Explicitly set permissions for the cert ---
# This ensures even if the file is copied as root, the 'shiny' user can see it
RUN chmod 644 /srv/shiny-server/ca-certificate.crt || true
RUN chown -R shiny:shiny /srv/shiny-server/

# 4. Port & Startup
USER shiny
EXPOSE 8080

# Ensure the shiny user can read the cert [cite: 3]
#RUN chmod 644 /srv/shiny-server/ca-certificate.crt || true
#RUN chown shiny:shiny /srv/shiny-server/ca-certificate.crt || true

# Direct startup for better reliability
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=8080)"]