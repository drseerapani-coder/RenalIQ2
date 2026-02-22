# Use the official R-Shiny base image
FROM rocker/shiny:4.3.0

# 1. Install System Dependencies 
# Added libsodium-dev for password hashing and postgresql-client for terminal debugging
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libsodium-dev \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

# 2. Install R Packages
# Included 'sodium' for password_verify and 'stringr' for str_trim
RUN R -e "install.packages(c(\
    'shiny', \
    'bslib', \
    'pool', \
    'DBI', \
    'RPostgres', \
    'dplyr', \
    'tidyr', \
    'lubridate', \
    'stringr', \
    'sodium', \
    'DT', \
    'shinyjs', \
    'jsonlite', \
    'glue', \
    'rhandsontable', \
    'shinycssloaders', \
    'readxl' \
    ), repos='https://cran.rstudio.com/')"

# 3. App Setup
# Copy all app files (ensure auth_module.R is in the same folder)
COPY . /srv/shiny-server/

# Fix permissions so the shiny user can read the Excel templates
RUN chown -R shiny:shiny /srv/shiny-server/

# 4. Port & Startup
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]