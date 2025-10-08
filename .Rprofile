# Load env vars from any file starting with `.env`. This allows user-specific
# options to be set in `.env_user` (which is .gitignored), and to have both
# encrypted and non-encrypted .env files
load_env <- function(){
  for (env_file in list.files(all.files = TRUE, pattern = "^\\.env.*")) {
    try(readRenviron(env_file), silent = TRUE)
  }
}
load_env()

# Set options for renv convenience
options(
  repos = c(CRAN = "https://cloud.r-project.org",
            ROPENSCI = "https://ropensci.r-universe.dev"),
  renv.config.auto.snapshot = FALSE, ## Attempt to keep renv.lock updated automatically
  renv.config.rspm.enabled = TRUE, ## Use RStudio Package manager for pre-built package binaries for linux
  renv.config.install.shortcuts = FALSE, ## Use the existing local library to fetch copies of packages for renv
  renv.config.cache.enabled = TRUE   ## Use the renv build cache to speed up install times
)

# Set options for internet timeout
options(timeout = max(300, getOption("timeout")))

source("renv/activate.R")
load_env() # reload project .env files, after renv/activate.R runs renv::load() which reads user's .renviron


# If project packages have conflicts define them here so as
# as to manage them across all sessions when building targets
if(requireNamespace("conflicted", quietly = TRUE)) {
}

if (interactive() && Sys.getenv("TERM_PROGRAM") == "vscode") {

  options(vsc.dev.args = list(
    width = 1500,
    height = 1500,
    pointsize = 12,
    res = 300
  ))
}
