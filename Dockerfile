# Use the official R-Shiny base image
FROM rocker/shiny:4.3.0

# 1. Install System Dependencies 
# These are required for R packages to interact with Postgres, SSL, XML, and Excel files
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

# 2. Install R Packages
# This list combines all your requested libraries into a single installation layer
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
    'DT', \
    'shinyjs', \
    'jsonlite', \
    'glue', \
    'rhandsontable', \
    'shinycssloaders', \
    'readxl' \
    ), repos='https://cran.rstudio.com/')"

# 3. App Setup
# Copy all files from your local directory into the image
COPY . /srv/shiny-server/

# Ensure the 'shiny' user has permission to read the files (especially templates.xlsx)
RUN chown -R shiny:shiny /srv/shiny-server/

# 4. Port & Startup
EXPOSE 3838

# Automatically starts the Shiny server on launch
CMD ["/usr/bin/shiny-server"]