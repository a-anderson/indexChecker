# Use a lightweight R base image
FROM rocker/r-ver:4.0.5

# Install necessary system dependencies
RUN apt-get update && \
    apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev

# Install required R packages
RUN R -e "install.packages(c('shiny', 'dplyr', 'rhandsontable'))"

# Set the working directory in the container
WORKDIR /usr/src/app

# Copy the R files and data folder into the container
COPY app/ /usr/src/app

# Expose port 3838
EXPOSE 3838

# Command to run the Shiny app when the container starts
CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=3838)"]