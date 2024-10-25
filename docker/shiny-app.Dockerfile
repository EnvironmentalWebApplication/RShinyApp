# Use a specific version of the official R image with Shiny
FROM rocker/shiny:4.1.2

# Install R packages
RUN R -e "install.packages(c('shiny', 'ggplot2', 'colorRamps'), repos='https://cloud.r-project.org/')"

# Copy R Shiny app code from 'r' directory to the Shiny app directory inside the container
COPY ../r /srv/shiny-server/

# Set the correct permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose port 3838 for the Shiny app
EXPOSE 3838

# Start the Shiny app
CMD ["/usr/bin/shiny-server"]

#To test run:
#docker build --platform linux/amd64 -t shiny-app -f docker/shiny-app.Dockerfile .
#docker run -p 3838:3838 shiny-app