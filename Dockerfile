# Use the official R-Shiny base image
FROM rocker/shiny:latest

# 1. Install System Dependencies 
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libsodium-dev \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

# 2. Install R Packages
RUN R -e "install.packages(c(\
    'shiny', 'bslib', 'pool', 'DBI', 'RPostgres', \
    'dplyr', 'tidyr', 'lubridate', 'stringr', 'sodium', \
    'DT', 'shinyjs', 'jsonlite', 'glue', 'rhandsontable', \
    'shinycssloaders', 'readxl' \
    ), repos='https://cran.rstudio.com/')"

# 3. App Setup
RUN rm -rf /srv/shiny-server/*
WORKDIR /srv/shiny-server/
COPY . /srv/shiny-server/

# Fix permissions
RUN chown -R shiny:shiny /srv/shiny-server/

# 4. Port & Startup
# DigitalOcean expects 8080 by default. 
EXPOSE 8080

# Direct startup bypasses shiny-server management for better health check reliability
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=8080)"]