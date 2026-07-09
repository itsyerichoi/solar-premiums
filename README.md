# Solar Premium Analysis
Replication code and data for **"Solar Premiums Driven by Both Energy Income and Development Potential"**.
Data and code for solar PV premium analysis. If you have questions or suggestions, please contact Yeri at yerichoi@hanyang.ac.kr

## Computational requirement
- R 4.0+

## Structure

```
solar-premiums/
├── data/                     8 analysis datasets (inst_n5/10/15/30, appr_n5/10/15/30)
├── fonts/                    DejaVuSans.ttf, used for all figure text
├── R/
│   ├── 00_setup.R            packages, font registration, data loading
│   ├── 01_main_did.R         Table 1 (Cols 1-3): baseline / energy income / land-use reclassification DID
│   ├── 02_heterogeneity.R    Fig. 5, Table S7: heterogeneous effects by parcel characteristics
│   ├── 03_parallel_trends.R  Fig. 6: property values by event time
│   ├── 04_event_study.R      Tables S2-S3, S5-S6; Fig. 4, Fig. S2, Fig. S3
│   ├── 05_robustness.R       Tables S8-S11, Fig. S4: placebo timing, narrow window, placebo group,
│   │                         control group sizes, parcel fixed effects
│   └── 06_summary_stats.R    Tables S12-S15: descriptive statistics
├── shp/                      solar PV shapefiles
└── solar-premiums.Rproj
```

## Running the code

1. Clone the repo and open `solar-premiums.Rproj` in RStudio (this sets the working directory to the repo root, which the `here` package relies on).
2. Install dependencies if needed:

   ```r
   install.packages(c("here", "tidyr", "dplyr", "fixest", "classInt", "ggplot2",
                       "showtext", "scales", "forcats", "patchwork"))
   ```
3. Run the scripts in `R/` individually in numeric order — later scripts depend on objects created by earlier ones (see the header comment in each file).

## Data

All datasets are parcel-level panel datasets (`pnu` = parcel identifier) for Jeju Island, South Korea, including annual officially assessed land values and parcel-level control variables. n5, n10, n15, and n30 represent alternative datasets in which the control group consists of the 5, 10, 15, and 30 nearest control parcels to each treated parcel, respectively.
