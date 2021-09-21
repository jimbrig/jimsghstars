# shinyappsio
library(rsconnect) # pak::pak("rsconnect")
library(config)

conf <- config::get("shinyappsio", file = "../secrets.yml")

purrr::walk(names(conf), function(account) {
  config <- conf[[account]]
  command <- config$`setup-command`
  eval(parse(text = command))
})

# update package deps
automagic::make_deps_file(directory = "../app")
