# This Dockerfile expands the rocker RStudio server 
# image for a tidymodels pipeline which should work
# for both ARM and X86 architectures.
FROM rocker/rstudio:latest

# Update system and install system dependencies
RUN apt-get update \
    && apt-get upgrade -y \
    && apt-get install -y --no-install-recommends \
      git \
      build-essential \
      git-crypt \
      libssl-dev \
      libxml2-dev \
      libglpk-dev \
      libgit2-dev \
      libfreetype6-dev \
      liblcms2-dev \
      libjpeg-dev \
      libpng-dev \
      libtiff5-dev \
      libgif-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libfontconfig1-dev \
      libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/* 
      
# Install additional R packages
RUN install2.r --error --skipinstalled --ncpus -1 \ 
    curl \
    tidyverse \
    targets \
    tarchetypes \
    tidymodels \
    Rcpp \ 
    RcppEigen \ 
    devtools \
    remotes \
    mgcv

RUN mkdir -p ~/.R && \
    echo "CFLAGS += -flax-vector-conversions" >> ~/.R/Makevars && \
    echo "CXXFLAGS += -flax-vector-conversions" >> ~/.R/Makevars

RUN R -e 'install.packages("dbarts")' 

# Expose port 8787 for RStudio Server
EXPOSE 8787

