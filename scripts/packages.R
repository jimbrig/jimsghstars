pak::pak(c(
  "pak",
  "remotes",
  "automagic",
  "shiny",
  "fs",
  "qs",
  "DT",
  "stringr",
  "dplyr",
  "purrr"
  ))

# packages
automagic::make_deps_file(directory = "../app")

# add string to Dockerfile
write("RUN Rscript -e \"automagic::install_deps_file('deps.yaml')\"",
      file = "../app/Dockerfile",
      append = TRUE)
