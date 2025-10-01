FROM rocker/binder:4.5.1

COPY install.R /tmp/install.R
RUN Rscript /tmp/install.R
