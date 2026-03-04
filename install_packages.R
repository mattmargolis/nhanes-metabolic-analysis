pkgs <- c("nhanesA", "dplyr", "tidyr", "ggplot2", "survey", "purrr",
          "here", "knitr", "kableExtra", "rmarkdown")
missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(missing) > 0) {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  cat("All packages already installed\n")
}
cat("Done.\n")
