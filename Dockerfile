# Use the official R base image
FROM rocker/r-ver:4.3.1

# Install system libraries required by R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libpng-dev \
    libssl-dev \
    libxml2-dev \
    pandoc \
    && apt-get clean


# Install plumber and other R packages
RUN install2.r --error \
    plumber \
    tidymodels \
    ggplot2 \
    ranger

# Set the working directory in the container
WORKDIR /app

# Copy project files to the container
COPY API.R /app/API.R
COPY runAPI.R /app/runAPI.R
COPY processed_data.rds /app/processed_data.rds
COPY final_rf_model.rds /app/final_rf_model.rds

# Expose the port for the API
EXPOSE 8000

# Run the API when the container starts
CMD ["Rscript", "runAPI.R"]