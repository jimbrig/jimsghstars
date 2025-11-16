# jimsghstars Shiny App

Interactive R Shiny application for browsing and filtering GitHub starred repositories.

## Features

- 📊 **Interactive Table**: Browse all starred repositories in a sortable, filterable table
- 🔍 **Advanced Filters**: Filter by programming language, date ranges, and star count
- 🎨 **Dark Theme**: Sleek dark UI using the Cyborg bootswatch theme
- 🔗 **Direct Links**: Click repository names to open them in GitHub
- 📱 **Responsive**: Works on desktop and mobile devices

## Local Development

### Prerequisites

- R (>= 4.0)
- RStudio (recommended)

### Installation

1. Install required packages:

```r
install.packages(c(
  "shiny", "fs", "qs", "stringr", "dplyr", "purrr",
  "shinycustomloader", "reactable", "formattable", 
  "bslib", "htmltools"
))
```

2. Ensure data file exists:

```r
# From repository root
source("get_github_stars.R")
```

3. Run the app:

```r
# From repository root
shiny::runApp("app")

# Or from within the app directory
shiny::runApp()
```

## Deployment

### Deploy to shinyapps.io

```r
library(rsconnect)

# Configure account (first time only)
rsconnect::setAccountInfo(
  name = "your-account",
  token = "your-token",
  secret = "your-secret"
)

# Deploy
rsconnect::deployApp(
  appDir = "app",
  appName = "jimsghstars"
)
```

### Automated Deployment

The app is automatically deployed via GitHub Actions when:
- Data is updated (daily schedule)
- Changes are pushed to the `app/` directory
- Workflow is manually triggered

See [AUTOMATION.md](../AUTOMATION.md) for details.

## File Structure

```
app/
├── app.R              # Main Shiny application
├── data/              # Data directory
│   └── stars-latest.qs  # Latest stars data
├── deploy.R           # Deployment script
├── deps.yaml          # Package dependencies
└── www/               # Static assets (images, favicon, etc.)
```

## Configuration

### Theme Customization

The app uses `bslib::bs_theme()` with the "cyborg" bootswatch theme. To customize:

```r
theme = bslib::bs_theme(
  bootswatch = "cyborg",  # Change theme
  # Add custom overrides
  bg = "#000000",
  fg = "#FFFFFF"
)
```

### Table Theme

The `reactable` table uses a custom dark theme defined in `tbl_theme`. Modify colors in the `reactableTheme()` call.

## Data Structure

The app expects a data frame with these columns:

- `repo`: Repository name (owner/repo)
- `url`: GitHub URL
- `description`: Repository description  
- `last_updated`: Last update date
- `created`: Creation date
- `stargazers`: Star count
- `language`: Primary programming language

## Troubleshooting

**App won't start**:
- Ensure all dependencies are installed
- Check that data file exists in `app/data/stars-latest.qs`

**Data not loading**:
- Verify data file format (.qs)
- Run `get_github_stars.R` to regenerate data

**Deployment fails**:
- Check shinyapps.io credentials
- Verify all packages are available on CRAN
- Review deployment logs

## Performance

The app uses:
- `qs` format for fast data loading
- `reactable` for efficient table rendering
- Lazy loading with `shinycustomloader`

For large datasets (>5000 repos), consider:
- Pagination in reactable
- Server-side filtering
- Data caching strategies
