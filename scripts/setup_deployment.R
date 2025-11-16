#!/usr/bin/env Rscript

# Setup script for configuring shinyapps.io deployment
# This script helps configure rsconnect for deploying to shinyapps.io

cat("=== jimsghstars - Shiny App Deployment Setup ===\n\n")

# Check if rsconnect is installed
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  cat("Installing rsconnect package...\n")
  install.packages("rsconnect")
}

library(rsconnect)

cat("This script will help you configure shinyapps.io deployment.\n")
cat("You'll need your shinyapps.io account credentials.\n\n")

cat("To get your credentials:\n")
cat("1. Go to https://www.shinyapps.io/\n")
cat("2. Log in to your account\n")
cat("3. Click on your name in the top right\n")
cat("4. Go to 'Account' -> 'Tokens'\n")
cat("5. Click 'Show' or 'Add Token'\n\n")

# Prompt for credentials
account_name <- readline(prompt = "Enter your shinyapps.io account name: ")
token <- readline(prompt = "Enter your token: ")
secret <- readline(prompt = "Enter your secret: ")

# Configure rsconnect
tryCatch({
  rsconnect::setAccountInfo(
    name = account_name,
    token = token,
    secret = secret
  )
  
  cat("\n✅ Successfully configured shinyapps.io credentials!\n\n")
  
  # Verify by listing accounts
  accounts <- rsconnect::accounts()
  cat("Configured accounts:\n")
  print(accounts)
  
  cat("\n=== Next Steps ===\n")
  cat("1. Test deployment locally:\n")
  cat("   rsconnect::deployApp('app', appName = 'jimsghstars')\n\n")
  cat("2. For GitHub Actions automation, add these secrets to your repository:\n")
  cat("   - SHINY_ACCOUNT: ", account_name, "\n", sep = "")
  cat("   - SHINY_TOKEN: [your token]\n")
  cat("   - SHINY_SECRET: [your secret]\n\n")
  cat("See AUTOMATION.md for more details.\n")
  
}, error = function(e) {
  cat("\n❌ Error configuring credentials:\n")
  cat(conditionMessage(e), "\n")
  cat("\nPlease verify your credentials and try again.\n")
})
