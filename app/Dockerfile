FROM rocker/shiny
RUN install2.r rsconnect
COPY . /home/shinyusr/
WORKDIR /home/shinyusr
CMD Rscript deploy.R
