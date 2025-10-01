FROM rocker/binder:4.3.1

COPY install.R /tmp/install.R
RUN Rscript /tmp/install.R
