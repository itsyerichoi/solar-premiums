## =================================================================================================================================
## 06_summary_stats.R
## Table S12 Summary statistics of property values (Model 1)
## Table S13 Summary statistics of property values (Model 2)
## Table S14 Summary statistics of property values (Model 3)
## Table S15 Summary statistics of parcel characteristics
## Requires: 00_setup.R, 01_main_did.R, 02_heterogeneity.R (uses appr_n10 with treat_no_jimok/treat_yes_jimok)
## =================================================================================================================================
summary_stats <- function(df, model_type = c("m1", "m2", "m3")) {
  model_type <- match.arg(model_type)

  if (model_type == "m1") {
    df <- df %>%
      mutate(group = case_when(
        treat == 1 ~ "Treatment (PV)",
        treat == 0 ~ "Control"
      ))
  } else if (model_type == "m2") {
    df <- df %>%
      mutate(group = case_when(
        treat_low_gen  == 1 ~ "Treatment: Low generation",
        treat_high_gen == 1 ~ "Treatment: High generation",
        treat == 0          ~ "Control"
      ))
  } else if (model_type == "m3") {
    df <- df %>%
      mutate(group = case_when(
        treat_no_jimok  == 1 ~ "Treatment 1: LU change (X)",
        treat_yes_jimok == 1 ~ "Treatment 2: LU change (O)",
        treat == 0           ~ "Control"
      ))
  }

  df %>%
    filter(yrDif >= -10 & yrDif <= 5) %>%
    group_by(group, yrDif) %>%
    summarise(
      N    = n(),
      Mean = round(mean(propertyVal_2000, na.rm = TRUE)),
      SD   = round(sd(propertyVal_2000,   na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(group, yrDif)
}

## Table S12 Summary Statistics of property values (Model 1)
summary_stats(appr_n10, "m1") %>% print(n=Inf)

## Table S13 Summary statistics of property values (Model 2)
summary_stats(appr_n10, "m2") %>% print(n=Inf)

## Table S14 Summary statistics of property values (Model 3)
summary_stats(appr_n10, "m3") %>% print(n=Inf)

## Table S15 Summary statistics of parcel characteristics
appr_n10 %>%
  filter(yrDif >= -10 & yrDif <= 5) %>%
  distinct(pnu, .keep_all = TRUE) %>%
  summarise(
    across(
      c(area_ha, soilQlt, powerline_km),
      list(
        count = ~sum(!is.na(.)),
        mean  = ~mean(., na.rm = TRUE),
        sd    = ~sd(., na.rm = TRUE)
      )
    )
  )

appr_n10 %>%
  filter(yrDif >= -10 & yrDif <= 5) %>%
  summarise(
    across(
      c(builtup_km, road_km),
      list(
        count = ~sum(!is.na(.)),
        mean  = ~mean(., na.rm = TRUE),
        sd    = ~sd(., na.rm = TRUE)
      )
    )
  )
