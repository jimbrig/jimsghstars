
library(rsconnect)
library(automagic)

automagic::install_deps_file('deps.yaml')

error_on_missing_name <- function(name){
    var <- Sys.getenv(name, unset=NA)
    if(is.na(var)){
        stop(paste0("cannot find ",name, " !"),call. = FALSE)
    }
    gsub("\"", '',var)
}

# Authenticate
setAccountInfo(
  name   = error_on_missing_name("SHINY_ACC_NAME"),
  token  = error_on_missing_name("TOKEN"),
  secret = error_on_missing_name("SECRET")
)

# Deploy the application.
deployApp(
    appFiles = c("app.R"),
    appName = error_on_missing_name("MASTERNAME"),
    appTitle = "JimsGitHubStars"
)
