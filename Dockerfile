FROM rocker/binder:latest

# Copy install.R to the expected location
COPY install.R /tmp/install.R

# Run package installation (important: use /tmp path!)
RUN Rscript /tmp/install.R

# Copy the rest of the repo
COPY . /home/rstudio/

# Set working directory
WORKDIR /home/rstudio
