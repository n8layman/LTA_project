<!-- README.md is generated from README.Rmd. Please edit that file -->

# Containerised R workflow template

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![container-workflow-template](https://github.com/ecohealthalliance/container-template/actions/workflows/container-workflow-template.yml/badge.svg)](https://github.com/ecohealthalliance/container-template/actions/workflows/container-workflow-template.yml)
[![License for Code:
MIT](<https://img.shields.io/badge/License%20(for%20code)-MIT-yellow.svg>)](https://opensource.org/licenses/MIT)
[![License:
CC-BY-4.0](<https://img.shields.io/badge/License%20(for%20text)-CC_BY_4.0-blue.svg>)](https://creativecommons.org/licenses/by/4.0/)

<!-- badges: end -->

Welcome! My name is Nate and this repository is my project pipeline template. I hope it provides some help getting your project started! For more information about me, please see my [website](https://n8layman.github.io/).

## Set-up and installation

This pipeline was created using R version 4.4.2 and the {renv} framework to record R package dependencies and
versions. Packages and versions used are recorded in the `renv.lock`.

- Use this template to create your own project:

  - Option 1: Using GitHub website

    - Visit the template repository at https://github.com/n8layman/project-template
    - Click the green "Use this template" button and select "Create a new repository"
    - Fill in your repository name and description
    - Choose public or private visibility as needed
    - Click "Create repository from template"

  - Option 2: Using GitHub CLI
    - Install GitHub CLI if you haven't already (https://cli.github.com/)
    - Run the following command in your terminal:
      ```
      gh repo create my-new-project --template n8layman/project-template --private --clone
      ```
    - This will create a private repository called "my-new-project" and clone it to your local machine
    - Modify the command as needed, changing the repository name and visibility option

- If you didn't use the `--clone` option, clone your new repository:

  - In the terminal enter
    `git clone https://github.com/yourusername/your-repo-name.git`
    in a suitable directory

- Duplicate the R environment used for the analysis:
  - This project was created using R version 4.4.2. This and other
    versions of R are available on the [R Archive
    Network](https://cloud.r-project.org/)
  - This project uses the {renv} framework to record R package
    dependencies and versions. Packages and versions used are recorded
    in the `renv.lock` file.
  - To install the {renv} package run `install.packages("renv")`
  - Run `renv::hydrate()` to copy whatever packages are already
    available in your user / site libraries into the project library
  - Run `renv::restore()` to install any remaining required packages
    into your project library.

## GitHub Actions Integration

This project comes with pre-configured GitHub Actions workflows to automate routine tasks and ensure project quality. GitHub Actions help maintain consistency and reduce manual effort through continuous integration and automation.

### Automatic Bibliography Generation

As an example of how this works, one of the workflows in this template automatically processes academic papers and generates a BibTeX bibliography. When you add PDF papers to the `resources/papers/` directory and push to GitHub, the workflow:

1. Extracts metadata from the PDFs
2. Searches for DOIs and retrieves complete citation information
3. Generates/updates a consolidated BibTeX file
4. Commits the changes back to the repository

This feature is particularly useful for:

- Maintaining an up-to-date references for your project
- Ensuring consistent citation formatting
- Making citations immediately available to all team members
- Enabling easy citation in R Markdown and Quarto documents

\*The workflow uses the `pdftools`, `rcrossref`, and `bibtex` R packages to process papers. You can view the full implementation in `.github/workflows/process-papers.yml` and `R/process_papers.R`.

Here's the resulting bibtex [file](resources/papers/references.bib) generated from one of my [papers](resources/papers/computer_vision.pdf)!

## Python Environment with Pixi

This project also sets the stage for Python-based workflows that use Pixi for environment management. Pixi is a package management and environment tool that ensures reproducible Python environments across different systems.

### Setting up the Python environment

1. **Install Pixi**

   - Follow the installation instructions at [Pixi's official documentation](https://pixi.sh/latest/)
   - In most systems, you can install with: `curl -fsSL https://pixi.sh/install.sh | bash`

2. **Activate the Python environment**

   - Navigate to the project directory
   - Run `pixi shell` to activate the environment
   - This loads all Python dependencies specified in the `pixi.toml` file

3. **Available Python modules**
   - The environment includes commonly used data science packages:
     - numpy, pandas, scikit-learn, matplotlib
     - jupyter, ipykernel
     - Other specialized packages for geospatial analysis and machine learning

### Working with mixed R and Python workflows

- **Integrating R and Python**:

  - Use the {reticulate} package in R to call Python functions

- **Environment compatibility**:

  - The Pixi environment is configured to be compatible with the project's Docker container
  - All Python dependencies are locked in the `pixi.lock` file, ensuring reproducibility

- **Running Python notebooks**:
  - With the Pixi environment active, use `jupyter notebook` or `jupyter lab` to access interactive notebooks
  - Notebooks are stored in the `notebooks/` directory

## VSCode Integration

This project is configured to work seamlessly with Visual Studio Code for both R and Python development. Notes for getting that going are available [here](resources/notes/vscode_setup.md)

## Machine Learning with tidymodels

This project uses the {tidymodels} framework for modeling and machine learning workflows. {tidymodels} is a collection of packages for modeling and machine learning using tidyverse principles. More information on {tidymodels} can be found [here](https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/).

## Targets workflow

The project pipeline has been automated using the the {targets} pipeline
management tool. {Targets} breaks down the analysis into a series of
discrete, skippable computational steps. Individual steps can be loaded
into memory using tar_load(target_name). An overview of the pipeline can
be found in the `_targets.R` file in the main project folder. Each
component, such as data ingest, is further broken out into its own
collection of targets as indicated in the `_targets.R` file.

Targets are organized into distinct groups, including:

1.  Data ingest targets
2.  Data processing targets
3.  Analysis targets
4.  Output targets

All targets are defined within the `_targets.R` file.

## Re-running computationally expensive targets

Some targets are computationally intensive and long-running. The output
of these targets has been saved in the `\data` folder as compressed RDS
files. These files end in ‘.gz’ and can be manually accessed using
`read_rds()` or automatically through the targets pipeline. By default
these steps will not be re-computed and are disconnected in the pipeline
DAG unless a flag is set in the `.env` file. A description of these
flags can be found at the top of the `_targets.R` and in the `.env`
file.

## Misc

A hook to prevent users from commiting files greater than GitHub’s 100Mb
file size limit is available in the `.githooks` folder. To enable this
copy the `.githooks/pre-commit` file to the `.git/hooks` directory by
running the following command in the terminal within the project base
directory

```
cp .githooks/pre-commit .git/hooks/pre-commit
```

## Dockerized RStudio server

A dockerized container is available that provides a pre-configured
environment with RStudio Server based on
[rocker/rstudio:latest](https://rocker-project.org/images/versioned/rstudio.html),
optimized for machine learning workflows. It comes with {targets},
{tarchetypes}, {tidymodels}, {dbarts}, and {mgcv} pre-installed,
enabling users to quickly build, tune, and deploy predictive models
without the hassle of configuring dependencies or installing packages
manually. The Dockerfile can be built on both `arm64` and `x86`
architectures.

A pre-built image for `x86` systems is available at
[n8layman/docker-rstudio](https://hub.docker.com/repository/docker/n8layman/rstudio-server/general).
This image is automatically built and deployed whenever changes are made
to the Dockerfile. More information on how to start up rocker based
containers including using environment variables to set the container
user and the password is available at
[rocker-project.org](https://rocker-project.org/images/versioned/rstudio.html).
An example docker-compose file is also available in the
docker/rstudio folder.

## Git-Crypt for Secure Credentials

This project uses git-crypt to securely store sensitive information like API keys and access tokens directly in the repository. With git-crypt, confidential files are automatically encrypted before they're committed to the repository and decrypted after checkout by authorized users. To use git-crypt, you'll need to set it up on your system and be added as a trusted collaborator. After installation, sensitive files (like the `.env` file) specified in `.gitattributes` will be encrypted when committed. This approach eliminates the need for distributing credentials through insecure channels while maintaining the convenience of keeping all project files in one repository. For setup instructions, see the [git-crypt documentation](https://github.com/AGWA/git-crypt).

## Going Further

This template provides a solid foundation for reproducible research and analysis projects. Here are some suggestions for extending the template based on your project's specific needs:

### Deployment & Hosting

- **Shiny Applications with shinylive**: Deploy Shiny apps as static sites on GitHub Pages

  1. Export your Shiny app using shinylive:
     ```r
     shinylive::export("app", "docs")
     ```

  2. Enable GitHub Pages in your repository settings:
     - Go to Settings > Pages
     - Under "Source", select "Deploy from a branch"
     - Select the `main` branch and `/docs` folder
     - Click "Save"

  3. Your app will be available at `https://yourusername.github.io/repository-name/`

  Note: shinylive allows you to run Shiny applications entirely in the browser without a server, making it ideal for static hosting on GitHub Pages.

- **Shiny Server/shinyapps.io**: Add deployment configurations for traditional server-based hosting
- **Plumber APIs**: Create RESTful APIs to expose your models
- **Quarto Publishing**: Set up workflows for publishing interactive documents and dashboards
- **Static Site Generation**: Configure GitHub Pages for documentation sites

### Advanced CI/CD

- Add GitHub Actions workflows for:
  - Automated testing of R and Python code
  - Scheduled data updates
  - Container building and registry publishing
  - Automatic documentation updates

### Database Integration

- Set up connections to:
  - SQL databases with {duckdb} and {dolt}
  - [{arrow}](https://arrow.apache.org/docs/r/)

## References

#### This project uses [targets](https://books.ropensci.org/targets/) to ensure that the analysis is reproducible.

#### This project uses [gitflow](https://github.com/nvie/gitflow) to manage project development.

#### This project uses [git-crypt](https://github.com/AGWA/git-crypt) to encrypt sensitive information such as API keys.

#### This project used [renv](https://rstudio.github.io/renv/articles/renv.html) to manage the analysis environment and package versions

#### This project uses [rocker-project.org](https://rocker-project.org/images/versioned/rstudio.html) based container images

#### This project uses [pixi](https://pixi.sh) to manage Python environments and dependencies

#### This project supports [Visual Studio Code](https://code.visualstudio.com/) for integrated development.

#### This project uses [tidymodels](https://www.tidymodels.org/) for machine learning workflows.
