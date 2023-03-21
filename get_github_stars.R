library(magrittr)
library(gh)
library(purrr)
library(tibble)
library(dplyr)
library(qs)
library(fs)

mystars <- gh::gh("/users/:username/starred", username = "jimbrig", .limit = Inf)


repo_names <- purrr::map_depth(mystars, 1, purrr::pluck, "full_name") %>% purrr::flatten_chr()
repo_urls <- purrr::map_depth(mystars, 1, purrr::pluck, "url") %>% purrr::flatten_chr()
creation_dates <- purrr::map_depth(mystars, 1, purrr::pluck, "created_at") %>% purrr::flatten_chr() %>% stringr::str_sub(1, 10) %>% lubridate::ymd()
last_updated_dates <- purrr::map_depth(mystars, 1, purrr::pluck, "updated_at") %>% purrr::flatten() %>% stringr::str_sub(1, 10) %>% lubridate::ymd()
stargazers_count <- purrr::map_depth(mystars, 1, purrr::pluck, "stargazers_count") %>% purrr::flatten_int()
languages <- purrr::map_depth(mystars, 1, purrr::pluck, "language") %>% purrr::map(function(x) if (is.null(x)) return("Missing") else x) %>% purrr::flatten_chr() %>% tolower()

descriptions <- purrr::map_depth(mystars, 1, purrr::pluck, "description") %>% purrr::map(function(x) if (is.null(x)) return("Missing") else x) %>% purrr::flatten_chr() %>% tolower()

out <- tibble::tibble(
  repo = repo_names,
  url = repo_urls,
  description = descriptions,
  last_updated = last_updated_dates,
  created = creation_dates,
  stargazers = stargazers_count,
  language = languages
) %>%
  dplyr::mutate(dplyr::across(c("language", "description"), dplyr::na_if, "missing")) %>%
  dplyr::arrange(
    language,
    dplyr::desc(last_updated),
    dplyr::desc(stargazers)
  )

if (!fs::dir_exists("data")) { fs::dir_create("data") }

out_file <- paste0("data/", Sys.Date(), "-jimbrig-github-starred-repos.qs")
shiny_file <- "app/data/stars-latest.qs"

qs::qsave(out, out_file)
qs::qsave(out, shiny_file)
