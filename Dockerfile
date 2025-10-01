FROM rocker/binder:4.3.0

# Copy the entire repo
COPY . /home/rstudio/

# Run package installation
RUN Rscript /home/rstudio/install.R

# Set working directory
WORKDIR /home/rstudio