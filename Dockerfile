# Use the official R-Shiny base image
FROM rocker/shiny:4.3.0

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
# IMPORTANT: Delete default "Hello World" apps so they don't block your app
RUN rm -rf /srv/shiny-server/*

# Set the working directory so R finds your files automatically
WORKDIR /srv/shiny-server/

# Copy your local code into the image
COPY . /srv/shiny-server/

# Fix permissions
RUN chown -R shiny:shiny /srv/shiny-server/

# 4. Port & Startup
# DigitalOcean works best on 8080, but if you keep 3838, 
# you MUST match it in the DO Dashboard settings.
EXPOSE 3838

# Standard startup
CMD ["/usr/bin/shiny-server"]