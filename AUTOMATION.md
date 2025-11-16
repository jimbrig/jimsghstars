# jimsghstars - Automation Documentation

This repository contains an automated system for tracking and displaying GitHub starred repositories.

## Overview

The system consists of:

1. **Data Collection**: Fetches starred repositories from GitHub API
2. **Shiny Application**: Interactive web app to browse and filter starred repos
3. **Automated Workflows**: GitHub Actions to keep everything up-to-date

## Components

### 1. Data Collection Script

**File**: `get_github_stars.R`

This R script:
- Fetches all starred repositories for the user `jimbrig`
- Extracts metadata (name, URL, description, dates, stars, language)
- Saves data in two locations:
  - `/data/YYYY-MM-DD-jimbrig-github-starred-repos.qs` (historical archive)
  - `/app/data/stars-latest.qs` (used by Shiny app)

### 2. Shiny Application

**Location**: `/app/`

An interactive R Shiny application that provides:
- Filterable table of starred repositories
- Search functionality
- Filters by: language, date range, star count
- Direct links to repositories
- Dark theme UI using `bslib` and `reactable`

### 3. Automated Workflow

**File**: `.github/workflows/update-and-deploy.yml`

This GitHub Actions workflow:
- **Triggers**:
  - Daily at 8:00 AM UTC (scheduled)
  - Manual trigger via GitHub Actions UI
  - On push to main branch (for app changes)
  
- **Steps**:
  1. Fetches latest starred repositories
  2. Commits updated data to the repository
  3. Deploys the Shiny app to shinyapps.io

## Setup Instructions

### Prerequisites

- GitHub account with starred repositories
- (Optional) shinyapps.io account for deployment

### Required Secrets

Configure these GitHub repository secrets for deployment:

- `SHINY_ACCOUNT`: Your shinyapps.io account name
- `SHINY_TOKEN`: Your shinyapps.io authentication token
- `SHINY_SECRET`: Your shinyapps.io secret key

### Local Development

1. **Clone the repository**:
   ```bash
   git clone https://github.com/jimbrig/jimsghstars.git
   cd jimsghstars
   ```

2. **Install R dependencies**:
   ```r
   install.packages(c(
     "rsconnect", "automagic", "gh", "magrittr", "purrr", 
     "tibble", "dplyr", "qs", "fs", "lubridate", "stringr",
     "shiny", "shinycustomloader", "reactable", "formattable", 
     "bslib", "htmltools"
   ))
   ```

3. **Fetch data locally**:
   ```r
   source("get_github_stars.R")
   ```

4. **Run the Shiny app**:
   ```r
   shiny::runApp("app")
   ```

## Manual Workflow Triggers

You can manually trigger the workflow:

1. Go to the [Actions tab](../../actions)
2. Select "Update Data and Deploy"
3. Click "Run workflow"
4. Choose the branch and click "Run workflow"

## Workflow Status

The workflow will:
- ✅ Always update data (requires `GITHUB_TOKEN` - automatically provided)
- ✅ Deploy to shinyapps.io (requires credentials in secrets)
- ⚠️ Skip deployment if credentials are not configured

## Viewing Results

- **Shiny App**: Visit your deployed app at `https://[account].shinyapps.io/jimsghstars/`
- **Static List**: View the generated markdown list in [README.md](README.md)
- **Raw Data**: Historical data files in `/data/` directory

## Troubleshooting

### Data not updating
- Check the [Actions tab](../../actions) for workflow status
- Verify `GITHUB_TOKEN` has appropriate permissions
- Ensure the workflow file is in the `main` branch

### Deployment failures
- Verify shinyapps.io credentials are correctly set in repository secrets
- Check that all R package dependencies are listed in the workflow
- Review deployment logs in the Actions tab

### Local development issues
- Ensure you have the latest R version (>= 4.0)
- Install all required packages from `deps.yaml`
- Set `GITHUB_PAT` environment variable for API rate limits

## Data Format

Data is stored in `.qs` format (quick serialization) for efficiency. Each record contains:

- `repo`: Full repository name (owner/repo)
- `url`: GitHub API URL
- `description`: Repository description
- `last_updated`: Last update date
- `created`: Repository creation date
- `stargazers`: Number of stars
- `language`: Primary programming language

## Contributing

To modify the automation:

1. Fork the repository
2. Make changes to workflows or scripts
3. Test locally before pushing
4. Submit a pull request with description

## License

See [LICENSE](LICENSE) file for details.
