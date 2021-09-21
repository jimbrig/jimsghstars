# FROM rocker:r-ver
# RUN installr.r automagic remotes pak gh purrr stringr dplyr qs
# COPY generate_stars.R /home/shinyusr/
# WORKDIR /home/shinyusr
# RUN Rscript generate_stars.R

FROM rocker/shiny
RUN install2.r rsconnect
COPY . /home/shinyusr/
WORKDIR /home/shinyusr
CMD Rscript -e "shiny::runApp()"
# CMD Rscript deploy.R
