# Regional Tick Surveillance Data Explorer

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License for Code: MIT](<https://img.shields.io/badge/License%20(for%20code)-MIT-yellow.svg>)](https://opensource.org/licenses/MIT)
[![License: CC-BY-4.0](<https://img.shields.io/badge/License%20(for%20text)-CC_BY_4.0-blue.svg>)](https://creativecommons.org/licenses/by/4.0/)
<!-- badges: end -->

An interactive Shiny application for exploring tick surveillance data from multiple field sites, deployed as a static site using shinylive.

## Live Application

Visit the live application at: [https://n8layman.github.io/LTA_project/](https://n8layman.github.io/LTA_project/)

## Running Locally

### Prerequisites

- R version 4.4.1 or higher
- Required R packages:
  - shiny
  - leaflet
  - leaflet.providers
  - DT
  - tidyr
  - dplyr
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
   install.packages(c('shiny', 'leaflet', 'leaflet.providers', 'DT',
                      'tidyr', 'dplyr', 'munsell', 'glue', 'jsonlite'))
   ```

3. Run the app:
   ```r
   shiny::runApp('app')
   ```

## Deploying to GitHub Pages

This project includes automated deployment via GitHub Actions. When you push to the `main` branch, the app is automatically exported to a static site and deployed to GitHub Pages.

### Manual Export

To manually export the app with giscus comments:

```r
source("app/export_with_giscus.R")
export_with_giscus(app_dir = "app", output_dir = "docs")
```

This function:
- Exports the Shiny app using `shinylive::export()`
- Automatically injects giscus comments for user feedback
- Outputs a static site ready for GitHub Pages deployment

## Project Structure

```
LTA_project/
├── app/
│   ├── app.R                    # Main Shiny application
│   ├── helpers.R                # Helper functions for data processing
│   ├── styles.css               # Custom CSS styles
│   ├── export_with_giscus.R     # Export script with comments integration
│   ├── *.geojson                # Geographic data files
│   └── *.csv                    # Tick surveillance data
├── docs/                        # Generated static site (GitHub Pages)
├── scripts/                     # Utility scripts and archived files
└── .github/workflows/           # GitHub Actions for automated deployment
```

## Features

- **Interactive Maps**: Explore tick surveillance data on interactive leaflet maps
- **Multiple Sites**: View data from Mohonk Preserve (NY) and Mianus River Gorge Preserve (CT/NY)
- **Data Tables**: Filter and download tick observation data
- **Species Plots**: Visualize tick species distribution at each site
- **User Feedback**: Integrated giscus comments for discussion and feedback

## Technology Stack

- **R Shiny**: Interactive web application framework
- **Shinylive**: WebR-based technology for client-side Shiny apps
- **Leaflet**: Interactive mapping library
- **GitHub Actions**: Automated deployment pipeline
- **GitHub Pages**: Static site hosting
- **Giscus**: GitHub Discussions-powered comment system

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

- Code: [MIT License](https://opensource.org/licenses/MIT)
- Documentation: [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)

## Contact

For questions or collaboration opportunities, visit [n8layman.github.io](https://n8layman.github.io/).
