## =================================================================================================================================
## run_all.R
## Runs the full replication pipeline for "Solar Premiums Driven by Both Energy Income and Development Potential"
## Open this project via solarpremiums.Rproj (or set your working directory to the repo root) before running
## =================================================================================================================================

source(here::here("R", "00_setup.R"))
source(here::here("R", "01_main_did.R"))
source(here::here("R", "02_heterogeneity.R"))
source(here::here("R", "03_parallel_trends.R"))
source(here::here("R", "04_event_study.R"))
source(here::here("R", "05_robustness.R"))
source(here::here("R", "06_summary_stats.R"))
