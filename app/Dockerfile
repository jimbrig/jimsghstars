FROM rocker/shiny
RUN install2.r rsconnect automagic remotes pak
COPY . /home/shinyusr/
WORKDIR /home/shinyusr
CMD Rscript deploy.R
