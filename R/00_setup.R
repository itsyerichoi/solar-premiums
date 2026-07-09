## =================================================================================================================================
## 00_setup.R
## Load packages, register plot font, and read all analysis datasets
## Run this script first — every other script in R/ assumes these objects exist
## =================================================================================================================================
library(here)      # robust relative paths regardless of working directory
library(tidyr)
library(dplyr)
library(fixest)
library(classInt)
library(ggplot2)
library(showtext)
library(scales)
library(forcats)
library(patchwork)

## ---------------------------------------------------------------------------------------------------------------------------------
## Font (used by all ggplot theme() calls below)
## Place DejaVuSans.ttf in fonts/ at the repo root (already included in this repo)
## ---------------------------------------------------------------------------------------------------------------------------------
font_add("DejaVu Sans", here("fonts", "DejaVuSans.ttf"))
showtext_auto()

## ---------------------------------------------------------------------------------------------------------------------------------
## Read data
## CSVs live in data/ at the repo root
## ---------------------------------------------------------------------------------------------------------------------------------
inst_n5  <- read.csv(here("data", "inst_n5.csv"),  colClasses = c(pnu = "character"))
inst_n10 <- read.csv(here("data", "inst_n10.csv"), colClasses = c(pnu = "character"))
inst_n15 <- read.csv(here("data", "inst_n15.csv"), colClasses = c(pnu = "character"))
inst_n30 <- read.csv(here("data", "inst_n30.csv"), colClasses = c(pnu = "character"))

appr_n5  <- read.csv(here("data", "appr_n5.csv"),  colClasses = c(pnu = "character"))
appr_n10 <- read.csv(here("data", "appr_n10.csv"), colClasses = c(pnu = "character"))
appr_n15 <- read.csv(here("data", "appr_n15.csv"), colClasses = c(pnu = "character"))
appr_n30 <- read.csv(here("data", "appr_n30.csv"), colClasses = c(pnu = "character"))
