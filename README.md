# Regional Tick Surveillance Data Explorer

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License for Code: MIT](<https://img.shields.io/badge/License%20(for%20code)-MIT-yellow.svg>)](https://opensource.org/licenses/MIT)
[![License: CC-BY-4.0](<https://img.shields.io/badge/License%20(for%20text)-CC_BY_4.0-blue.svg>)](https://creativecommons.org/licenses/by/4.0/)
<!-- badges: end -->

An interactive R Shiny application for exploring tick surveillance data from multiple field sites. Built with **shinylive** to run entirely in the browser—no R server required. Deployed freely on **GitHub Pages** with integrated **GraphComment** comments for community feedback.

## Live Application

Visit the live application at: [https://n8layman.github.io/LTA_project/](https://n8layman.github.io/LTA_project/)

## Running Locally

### Prerequisites

- R version 4.4.1 or higher
- Required R packages:
  - shiny
  - leaflet
  - leaflet.extras
  - DT
  - tidyr
  - dplyr
  - ggplot2
  - munsell
  - glue
  - jsonlite

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/n8layman/LTA_project.git
   cd LTA_project
   ```

2. Install required packages:
   ```r
   install.packages(c('shiny', 'leaflet', 'leaflet.extras', 'DT',
                      'tidyr', 'dplyr', 'ggplot2', 'munsell', 'glue', 'jsonlite'))
   ```

3. Run the app:
   ```r
   shiny::runApp('app')
   ```

## Deploying to GitHub Pages

This project includes automated deployment via GitHub Actions. When you push to the `main` branch, the app is automatically exported to a static site and deployed to GitHub Pages.

### Manual Export

To manually export the app with GraphComment comments:

```r
source("app/export_with_giscus.R")
export_with_giscus(app_dir = "app", output_dir = "docs", graphcomment_id = "Tick-dashboard-1")
```

This function:
- Exports the Shiny app using `shinylive::export()`
- Automatically injects GraphComment widget for user feedback
- Outputs a static site ready for GitHub Pages deployment

## Project Structure

```
LTA_project/
├── app/
│   ├── app.R                        # Main Shiny application (591 lines)
│   ├── helpers.R                    # Helper functions for data processing
│   ├── export_with_giscus.R         # Export script with GraphComment integration
│   ├── styles.css                   # Custom CSS styles
│   ├── tick_count_tooltip.html      # HTML template for tick count tooltips
│   ├── table_row.html               # HTML template for table rows
│   ├── tick_data_processed.csv      # Primary tick surveillance dataset
│   ├── mianus_polygons.geojson      # Mianus preserve boundary
│   ├── mianus_transects.geojson     # Mianus transect lines
│   ├── mianus_exclosures.geojson    # Mianus deer exclosure polygons
│   ├── mianus_trails.geojson        # Mianus trail network
│   ├── mianus_fireroads.geojson     # Mianus fire road network
│   └── mohonk_exclosures.csv        # Mohonk exclosure metadata
├── docs/                            # Generated static site (GitHub Pages)
├── scripts/                         # Utility scripts and archived files
└── .github/workflows/               # GitHub Actions for automated deployment
```

## Features

- **Interactive Maps**: Explore tick surveillance data on interactive leaflet maps with multiple basemap options (OpenTopoMap, Satellite, Default)
- **Multiple Sites**: View data from Mohonk Preserve (NY) and Mianus River Gorge Preserve (CT/NY)
- **Hierarchical Data Display**: Browse tick counts at transect, line, and segment levels
- **Spatial Visualization**: Color-coded transect lines and polygons showing tick abundance
- **Interactive Tooltips**: Hover over map features to see detailed tick counts by life stage
- **Data Tables**: Filter, sort, and download tick observation data with species information
- **Species Plots**: Visualize tick species distribution by life stage (adults vs. nymphs) at each site
- **Site Summaries**: Quick overview of total tick counts and species diversity
- **User Feedback**: Integrated GraphComment for discussion and feedback (supports Google OAuth and anonymous posting)

## Why This Stack?

This project uses a modern, cost-effective approach to deploying interactive data applications:

### Shinylive + GitHub Pages

- **100% Free**: No server costs, no hosting fees, no maintenance overhead
- **Automated CI/CD**: Push to main and GitHub Actions handles the rest—build, export, and deploy automatically
- **Write in R, Not JavaScript**: Build interactive dashboards using R skills you already have—no need to learn web development frameworks
- **No Server Management**: Shinylive runs R directly in the browser using WebAssembly, eliminating the need for Shiny Server
- **Effortless Scaling**: Computation happens on users' devices, so the app scales automatically with traffic
- **Version Control Built-in**: Every deployment tracked in git—instant rollbacks and full change history
- **Global CDN**: GitHub Pages serves content worldwide with fast load times
- **HTTPS by Default**: Automatic SSL certificates with zero configuration

### GraphComment

- **Free Tier**: Unlimited page loads and comments (free forever with branding)
- **Anonymous Posting**: No account required to comment
- **Google/GitHub OAuth**: Optional sign-in with Google, GitHub, Facebook, or Twitter accounts
- **Rich Media**: Support for GIFs, images, and reactions
- **Real-time**: Comments appear instantly for all users
- **Moderation Dashboard**: Easy-to-use web interface for comment moderation
- **Spam Protection**: Built-in spam filtering and moderation tools
- **No GitHub Required**: Perfect for research audiences without developer accounts

## Technology Stack

- **R Shiny**: Interactive web application framework
- **Shinylive**: WebR-based technology for client-side Shiny apps
- **Leaflet**: Interactive mapping library
- **GitHub Actions**: Automated deployment pipeline
- **GitHub Pages**: Static site hosting
- **GraphComment**: Free comment system with Google OAuth and anonymous posting

## Fork This Project

Want to use this template for your own data? Here's how to set up your own version:

### 1. Fork the Repository

Click the "Fork" button on GitHub to create your own copy of this project.

### 2. Configure GitHub Pages

1. Go to your repository **Settings** → **Pages**
2. Under "Build and deployment", set **Source** to **GitHub Actions**

### 3. Set Up GraphComment (Optional)

1. Create a free account at [graphcomment.com](https://graphcomment.com)
2. Add your website at the GraphComment dashboard
3. Copy your GraphComment ID (e.g., "Your-Website-Name")
4. Update [app/export_with_giscus.R](app/export_with_giscus.R):
   - Change `graphcomment_id = "Tick-dashboard-1"` to your GraphComment ID
5. Enable Google OAuth in GraphComment settings for easy sign-in
6. Configure moderation preferences (auto-approve or require approval)

### 4. Update Hard-Coded References

Replace references to the original repository:

- **[app/app.R:62](app/app.R#L62)**: GitHub link in the header
- **[app/export_with_giscus.R](app/export_with_giscus.R)**: GraphComment ID parameter

### 5. Add Your Data

Replace the data files in the [app/](app/) folder with your own:

- Tick surveillance CSV files
- GeoJSON files for map layers
- Update file paths in [app/app.R](app/app.R) as needed

### 6. Deploy

Push to the `main` branch and GitHub Actions will automatically build and deploy your app!

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

- Code: [MIT License](https://opensource.org/licenses/MIT)
- Documentation: [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)

## Contact

For questions or collaboration opportunities, visit [n8layman.github.io](https://n8layman.github.io/).
