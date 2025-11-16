# Quick Start Guide - Automated Workflow

This guide will help you get started with the automated GitHub Actions workflow for jimsghstars.

## Prerequisites

✅ GitHub account (you have this!)  
✅ Repository cloned (you have this!)  
⚠️ shinyapps.io account (optional, for deployment)

## Step 1: Configure Deployment (Optional)

If you want to deploy to shinyapps.io:

### Option A: Use the Setup Script

```bash
# From repository root
Rscript scripts/setup_deployment.R
```

This will:
- Prompt for your shinyapps.io credentials
- Configure rsconnect locally
- Show you the secrets to add to GitHub

### Option B: Manual Configuration

1. Get credentials from https://www.shinyapps.io/admin/#/tokens
2. Add these as repository secrets in GitHub:
   - Go to Settings → Secrets and variables → Actions
   - Add three secrets:
     - `SHINY_ACCOUNT`: Your account name
     - `SHINY_TOKEN`: Your token
     - `SHINY_SECRET`: Your secret

## Step 2: Enable the Workflow

The workflow is **already configured** and will:

- ✅ Run automatically every day at 8:00 AM UTC
- ✅ Update GitHub stars data
- ✅ Deploy to shinyapps.io (if credentials are set)
- ✅ Can be triggered manually anytime

## Step 3: Test the Workflow

### Manual Test

1. Go to: https://github.com/jimbrig/jimsghstars/actions
2. Click on "Update Data and Deploy"
3. Click "Run workflow"
4. Select the branch: `copilot/automate-deployment-process`
5. Click "Run workflow"
6. Watch the magic happen! ✨

### What to Expect

The workflow will:

1. **Update Data** (~2-3 minutes)
   - Fetch all your starred repos from GitHub
   - Save to `/data` and `/app/data`
   - Commit changes to the repository

2. **Deploy App** (~3-5 minutes, if credentials configured)
   - Deploy to shinyapps.io
   - App will be available at: `https://[your-account].shinyapps.io/jimsghstars/`

3. **Summary**
   - See deployment status in the workflow summary

## Step 4: Verify Everything Works

### Check the Data

```bash
# Check latest data file exists
ls -lh app/data/stars-latest.qs

# View in R
R
dat <- qs::qread("app/data/stars-latest.qs")
head(dat)
```

### Run the App Locally

```bash
# From repository root
Rscript -e "shiny::runApp('app')"
```

Then open http://localhost:XXXX in your browser (port will be shown in console).

### Check Deployment

If you configured shinyapps.io, visit:
- https://[your-account].shinyapps.io/jimsghstars/

## Troubleshooting

### "Workflow not running"

- Check the Actions tab for errors
- Ensure you're on the correct branch
- Verify the workflow file exists: `.github/workflows/update-and-deploy.yml`

### "Deployment failed"

- Verify secrets are correctly set in repository settings
- Check the workflow logs for specific errors
- Try running `scripts/setup_deployment.R` again

### "Data not updating"

- GitHub API has rate limits (5000 requests/hour for authenticated users)
- Check the workflow logs for API errors
- Ensure `GITHUB_TOKEN` has appropriate permissions

## What's Next?

### After Merging to Main

Once this PR is merged to main:

1. The workflow will start running automatically
2. Data will update daily
3. App will deploy automatically
4. You can monitor via the Actions tab

### Customization

Want to change something?

- **Schedule**: Edit `cron:` in `.github/workflows/update-and-deploy.yml`
- **App Theme**: Edit `app/app.R` theme configuration
- **Data Fields**: Edit `get_github_stars.R` to add more fields

See [AUTOMATION.md](AUTOMATION.md) for detailed customization options.

## Support

Need help?

- 📖 Read [AUTOMATION.md](AUTOMATION.md) for detailed documentation
- 📖 Read [app/README.md](app/README.md) for app-specific docs
- 🐛 Check the Actions tab for workflow logs
- 💬 Open an issue if you encounter problems

## Success Criteria

You'll know everything is working when:

- ✅ Workflow runs without errors in Actions tab
- ✅ Data files are being updated in `/data` and `/app/data`
- ✅ App is accessible at your shinyapps.io URL (if configured)
- ✅ Workflow status badge shows "passing" in README

---

**Congratulations!** 🎉 You've successfully automated your GitHub stars tracking and Shiny app deployment!
