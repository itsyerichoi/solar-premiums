## =================================================================================================================================
## 05_robustness.R
## Table S8  Mock timing placebo test
## Table S9 Robustness check with narrow observation window
## Fig. S4   Placebo test results (placebo treatment group)
## Table S10 Robustness check with alternative control group sizes
## Table S11 Robustness check with parcel fixed effects
## Requires: 00_setup.R, 01_main_did.R (uses m1, m2, m3, doZtest_gen, doZtest_lu, appr_n5/10/15/30)
## =================================================================================================================================

## ---------------------------------------------------------------------------------------------------------------------------------
## Fake treatment timing  ->  Table S8
## ---------------------------------------------------------------------------------------------------------------------------------

## Shift treatment timing backward and rebuild event-time windows for the placebo test
make_fake_timing <- function(df, fake_shift){
  df %>%
    mutate(
      yrDif_fake = yrDif - fake_shift,
      time_fake  = if_else(yrDif_fake >= 0, 1L, 0L)
    ) %>%
    filter(yrDif <= -2,                       # exclude actual post-treatment period (key step)
           yrDif_fake >= -6 & yrDif_fake <= 4) # window based on fake timing
}

## Model 1
m1.fake <- function(myDF, fake_shift){
  myDF <- make_fake_timing(myDF, fake_shift)
  feols(log(propertyVal_2000) ~ time_fake + treat + treat:time_fake + area_ha + builtup_km + road_km + powerline_km + soilQlt
        | propertyYr^ADM_NM,
        cluster = ~ ADM_NM + propertyYr,
        data = myDF)
}

## Model 2
m2.fake <- function(myDF, fake_shift){
  myDF <- make_fake_timing(myDF, fake_shift)
  feols(log(propertyVal_2000) ~ time_fake + treat_low_gen + treat_high_gen + treat_low_gen:time_fake + treat_high_gen:time_fake + area_ha + builtup_km + road_km + powerline_km + soilQlt
        | propertyYr^ADM_NM,
        cluster = ~ ADM_NM + propertyYr,
        data = myDF)
}

## Model 3
m3.fake <- function(myDF, fake_shift, var1 = "treat_no_reclass", var2 = "treat_yes_reclass"){
  myDF <- make_fake_timing(myDF, fake_shift)

  myDF$treat_no_jimok  <- myDF[[var1]]
  myDF$treat_yes_jimok <- myDF[[var2]]

  feols(
    log(propertyVal_2000) ~
      time_fake + treat_no_jimok + treat_yes_jimok +
      treat_no_jimok:time_fake + treat_yes_jimok:time_fake +
      area_ha + builtup_km + road_km + powerline_km + soilQlt
    | propertyYr^ADM_NM,
    cluster = ~ ADM_NM + propertyYr,
    data = myDF
  )
}

## Table S9 Mock timing placebo test
m1.fake(appr_n10, fake_shift = -7) %>% summary()
m2.fake(appr_n10, fake_shift = -7) %>% summary()
m3.fake(appr_n10, fake_shift = -7) %>% summary()

## ---------------------------------------------------------------------------------------------------------------------------------
## Narrow observation window  ->  Table S9
## ---------------------------------------------------------------------------------------------------------------------------------

## Model 1
m1.small <- function(myDF){
  results <- feols(log(propertyVal_2000) ~ time+treat+treat:time + area_ha + builtup_km + road_km + powerline_km + soilQlt
                   | propertyYr^ADM_NM,
                   cluster = ~ ADM_NM + propertyYr,
                   data = myDF %>%
                     filter(yrDif >= -5 & yrDif <= 5))
  return(results)
}
## Model 2
m2.small <- function(myDF){
  results <- feols(log(propertyVal_2000) ~ time + treat_low_gen +treat_high_gen + treat_low_gen:time + treat_high_gen:time +
                     area_ha + builtup_km + road_km + powerline_km + soilQlt
                   | propertyYr^ADM_NM,
                   cluster = ~ ADM_NM + propertyYr,
                   data = myDF %>% filter(yrDif >= -5 & yrDif <= 5))
  return(results)
}
## Model 3
m3.small <- function(myDF, var1 = "treat_no_reclass", var2 = "treat_yes_reclass"){
  myDF$treat_no_reclass <- myDF[[var1]]
  myDF$treat_yes_reclass <- myDF[[var2]]

  results <- feols(log(propertyVal_2000) ~ time + treat_no_reclass + treat_yes_reclass + treat_no_reclass:time + treat_yes_reclass:time +
                     area_ha + builtup_km + road_km + powerline_km + soilQlt
                   | propertyYr^ADM_NM,
                   cluster = ~ ADM_NM + propertyYr,
                   data = myDF %>% filter(yrDif >= -5 & yrDif <= 5))
  return(results)
}

## Table S10, Column (1)
m1.small(appr_n10) %>% summary()

## Table S10, Column (2)
m2.small(appr_n10) %>% summary()
m2.small(appr_n10) %>% doZtest_gen()

## Table S10, Column (3)
m3.small(appr_n10) %>% summary()
m3.small(appr_n10) %>% doZtest_lu()

## ---------------------------------------------------------------------------------------------------------------------------------
## Placebo treatment group  ->  Fig. S4
## ---------------------------------------------------------------------------------------------------------------------------------
set.seed(123)

# ── Core placebo simulation function ──────────────────────────
run_placebo <- function(df, n_iter = 10, model_type = c("m1", "m2", "m3")) {
  model_type  <- match.arg(model_type)
  real_n      <- df %>% filter(treat == 1) %>% distinct(pnu) %>% nrow()
  control_pnu <- df %>% filter(treat == 0) %>% distinct(pnu) %>% pull(pnu)

  # Distribution of characteristics among actually treated parcels
  treat_chars <- df %>%
    filter(treat == 1) %>%
    distinct(pnu, treat_low_gen, treat_high_gen, treat_no_reclass, treat_yes_reclass)

  bind_rows(lapply(1:n_iter, function(i) {
    fake_pnu <- sample(control_pnu, real_n)

    # Randomly assign real treated-group characteristics to fake treated parcels
    fake_chars <- treat_chars %>%
      slice_sample(n = real_n, replace = TRUE) %>%
      mutate(pnu = fake_pnu) %>%
      select(pnu, treat_low_gen, treat_high_gen, treat_no_reclass, treat_yes_reclass)

    df_p <- df %>%
      filter(treat == 0) %>%
      left_join(fake_chars, by = "pnu") %>%
      mutate(
        treat             = if_else(pnu %in% fake_pnu, 1L, 0L),
        treat_low_gen     = if_else(pnu %in% fake_pnu, treat_low_gen.y,     0L),
        treat_high_gen    = if_else(pnu %in% fake_pnu, treat_high_gen.y,    0L),
        treat_no_reclass  = if_else(pnu %in% fake_pnu, treat_no_reclass.y,  0L),
        treat_yes_reclass = if_else(pnu %in% fake_pnu, treat_yes_reclass.y, 0L)
      ) %>%
      select(-ends_with(".x"), -ends_with(".y"))

    if (model_type == "m1") {
      mod <- feols(
        log(propertyVal_2000) ~ time + treat + treat:time + area_ha + builtup_km + road_km + soilQlt
        | propertyYr^ADM_NM,
        cluster = ~ ADM_NM + propertyYr,
        data = df_p %>% filter(yrDif >= -10 & yrDif <= 5))
      data.frame(iter = i,
                 est  = as.numeric(coef(mod)["time:treat"]),
                 se   = as.numeric(se(mod)["time:treat"]))

    } else if (model_type == "m2") {
      mod <- feols(
        log(propertyVal_2000) ~ time + treat_low_gen + treat_high_gen +
          treat_low_gen:time + treat_high_gen:time + area_ha + builtup_km + road_km + soilQlt
        | propertyYr^ADM_NM,
        cluster = ~ ADM_NM + propertyYr,
        data = df_p %>% filter(yrDif >= -10 & yrDif <= 5))
      data.frame(iter     = i,
                 est_low  = as.numeric(coef(mod)["time:treat_low_gen"]),
                 se_low   = as.numeric(se(mod)["time:treat_low_gen"]),
                 est_high = as.numeric(coef(mod)["time:treat_high_gen"]),
                 se_high  = as.numeric(se(mod)["time:treat_high_gen"]))

    } else if (model_type == "m3") {
      mod <- feols(
        log(propertyVal_2000) ~ time + treat_no_reclass + treat_yes_reclass +
          treat_no_reclass:time + treat_yes_reclass:time + area_ha + builtup_km + road_km + soilQlt
        | propertyYr^ADM_NM,
        cluster = ~ ADM_NM + propertyYr,
        data = df_p %>% filter(yrDif >= -10 & yrDif <= 5))
      data.frame(iter    = i,
                 est_no  = as.numeric(coef(mod)["time:treat_no_reclass"]),
                 se_no   = as.numeric(se(mod)["time:treat_no_reclass"]),
                 est_yes = as.numeric(coef(mod)["time:treat_yes_reclass"]),
                 se_yes  = as.numeric(se(mod)["time:treat_yes_reclass"]))
    }
  }))
}

res_m1_appr <- run_placebo(appr_n10, 1000, "m1")
res_m2_appr <- run_placebo(appr_n10, 1000, "m2")
res_m3_appr <- run_placebo(appr_n10, 1000, "m3")

## Actual (non-placebo) estimates 
real_m1_appr        <- as.numeric(coef(m1(appr_n10))["time:treat"])
real_m2_appr_low     <- as.numeric(coef(m2(appr_n10))["time:treat_low_gen"])
real_m2_appr_high    <- as.numeric(coef(m2(appr_n10))["time:treat_high_gen"])
real_m3_appr_no      <- as.numeric(coef(m3(appr_n10, 'treat_no_reclass', 'treat_yes_reclass'))["time:treat_no_reclass"])
real_m3_appr_yes     <- as.numeric(coef(m3(appr_n10, 'treat_no_reclass', 'treat_yes_reclass'))["time:treat_yes_reclass"])

cat("M1 appr   p:", mean(abs(res_m1_appr$est) >= abs(real_m1_appr)), "\n")
cat("M2 appr low  p:", mean(abs(res_m2_appr$est_low)  >= abs(real_m2_appr_low)),  "\n")
cat("M2 appr high p:", mean(abs(res_m2_appr$est_high) >= abs(real_m2_appr_high)), "\n")
cat("M3 appr no  p:", mean(abs(res_m3_appr$est_no)  >= abs(real_m3_appr_no)),  "\n")
cat("M3 appr yes p:", mean(abs(res_m3_appr$est_yes) >= abs(real_m3_appr_yes)), "\n")

## Plots
plot_placebo_m1 <- function(res, real_est) {
  ggplot(res, aes(x = est)) +
    geom_histogram(bins = 40, fill = "grey70", color = "white", linewidth = 0.2) +
    geom_vline(xintercept = real_est, color = "red", linewidth = 0.4) +
    theme_classic(base_size = 12) +
    theme(
      text         = element_text(family = "DejaVu Sans"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
      axis.line    = element_blank(),
      axis.ticks   = element_line(linewidth = 0.3)
    )
}

plot_placebo_m2 <- function(res, real_low, real_high) {
  res_long <- res %>%
    pivot_longer(c(est_low, est_high), names_to = "group", values_to = "est") %>%
    mutate(group = if_else(group == "est_low", "Low generation", "High generation"))

  vlines <- data.frame(
    group = c("Low generation", "High generation"),
    xint  = c(real_low, real_high)
  )

  ggplot(res_long, aes(x = est, fill = group)) +
    geom_histogram(bins = 40, alpha = 0.6, position = "identity", color = "white", linewidth = 0.2) +
    geom_vline(data = vlines, aes(xintercept = xint, color = group), linewidth = 0.4) +
    scale_fill_manual(values  = c("Low generation" = "#E41A1C", "High generation" = "#377EB8")) +
    scale_color_manual(values = c("Low generation" = "#E41A1C", "High generation" = "#377EB8"),
                       guide  = "none") +
    labs(fill = "") +
    scale_y_continuous(limits = c(0, 220)) +
    theme_classic(base_size = 12) +
    theme(
      text         = element_text(family = "DejaVu Sans"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
      axis.line    = element_blank(),
      axis.ticks   = element_line(linewidth = 0.3)
    )
}

plot_placebo_m3 <- function(res, real_no, real_yes) {
  res_long <- res %>%
    pivot_longer(c(est_no, est_yes), names_to = "group", values_to = "est") %>%
    mutate(group = if_else(group == "est_no", "No LU change", "LU change"))

  vlines <- data.frame(
    group = c("No LU change", "LU change"),
    xint  = c(real_no, real_yes)
  )

  ggplot(res_long, aes(x = est, fill = group)) +
    geom_histogram(bins = 40, alpha = 0.6, position = "identity", color = "white", linewidth = 0.2) +
    geom_vline(data = vlines, aes(xintercept = xint, color = group), linewidth = 0.4) +
    scale_fill_manual(
      values = c("No LU change" = "#E41A1C", "LU change" = "#377EB8"),
      labels = c("No LU change" = "LU change (X)", "LU change" = "LU change (O)")
    ) +
    scale_color_manual(values = c("No LU change" = "#E41A1C", "LU change" = "#377EB8"),
                       guide  = "none") +
    labs(fill = "") +
    scale_y_continuous(limits = c(0, 260), breaks = c(0, 50, 100, 150, 200, 250)) +
    theme_classic(base_size = 12) +
    theme(
      text         = element_text(family = "DejaVu Sans"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
      axis.line    = element_blank(),
      axis.ticks   = element_line(linewidth = 0.3)
    )
}

# Combine plots
row1 <- plot_placebo_m1(res_m1_appr, real_m1_appr) +
  labs(tag = "a)")

row2 <- plot_placebo_m2(res_m2_appr, real_m2_appr_low, real_m2_appr_high) +
  labs(tag = "b)") +
  plot_layout(guides = "collect") &
  theme(
    legend.position  = "right",
    legend.key.size  = unit(0.4, "cm"),
    legend.text      = element_text(size = 9),
    legend.margin    = margin(2, 4, 2, 4)
  )

row3 <- plot_placebo_m3(res_m3_appr, real_m3_appr_no, real_m3_appr_yes) +
  labs(tag = "c)") +
  plot_layout(guides = "collect") &
  theme(
    legend.position  = "right",
    legend.key.size  = unit(0.4, "cm"),
    legend.text      = element_text(size = 9),
    legend.margin    = margin(2, 4, 2, 4)
  )

## Fig. S4 Placebo test results
(row1 / row2 / row3) +
  plot_annotation(
    theme = theme(
      plot.tag = element_text(family = "DejaVu Sans", size = 13, face = "bold")
    )
  ) &
  labs(x = NULL, y = NULL)

## ---------------------------------------------------------------------------------------------------------------------------------
## Various control group sizes  ->  Table S10
## Uses m1 / m2 / m3 / doZtest_gen / doZtest_lu defined in 01_main_did.R
## ---------------------------------------------------------------------------------------------------------------------------------

## Table S11 Column (1): n5
m1(appr_n5) %>% summary()
m2(appr_n5) %>% summary()
m2(appr_n5) %>% doZtest_gen()
m3(appr_n5) %>% summary()
m3(appr_n5) %>% summary() %>% doZtest_lu()

## Table S11 Column (2): n10
m1(appr_n10) %>% summary()
m2(appr_n10) %>% summary()
m2(appr_n10) %>% doZtest_gen()
m3(appr_n10) %>% summary()
m3(appr_n10) %>% summary() %>% doZtest_lu()

## Table S11 Column (3): n15
m1(appr_n15) %>% summary()
m2(appr_n15) %>% summary()
m2(appr_n15) %>% doZtest_gen()
m3(appr_n15) %>% summary()
m3(appr_n15) %>% summary() %>% doZtest_lu()

## Table S11 Column (4): n30
m1(appr_n30) %>% summary()
m2(appr_n30) %>% summary()
m2(appr_n30) %>% doZtest_gen()
m3(appr_n30) %>% summary()
m3(appr_n30) %>% summary() %>% doZtest_lu()

## ---------------------------------------------------------------------------------------------------------------------------------
## Parcel fixed effects  ->  Table S11
## ---------------------------------------------------------------------------------------------------------------------------------
## Model 1
m1.fe <- function(myDF){
  results <- feols(log(propertyVal_2000) ~ time + treat:time + area_ha + builtup_km + road_km + powerline_km + soilQlt
                   | pnu + propertyYr^ADM_NM,
                   cluster = ~ ADM_NM + propertyYr,
                   data = myDF %>%
                     filter(yrDif >= -10 & yrDif <= 5))
  return(results)
}
## Table S12 Column (1)
m1.fe(appr_n10) %>% summary()

## Model 2
m2.2.fe <- function(myDF){
  results <- feols(log(propertyVal_2000) ~ time + treat_low_gen +treat_high_gen + treat_low_gen:time + treat_high_gen:time + area_ha + builtup_km + road_km + powerline_km + soilQlt
                   | pnu+propertyYr^ADM_NM,
                   cluster = ~ ADM_NM + propertyYr,
                   data = myDF %>% filter(yrDif >= -10 & yrDif <= 5))
  return(results)
}
## Table S11 Column (2)
m2.2.fe(appr_n10) %>% summary()
m2.2.fe(appr_n10) %>% doZtest_gen()

## Model 3 (parcel FE version; m3 without parcel FE is defined in 01_main_did.R)
m3.fe <- function(myDF, var1 = "treat_no_reclass", var2 = "treat_yes_reclass"){
  myDF$treat_no_jimok <- myDF[[var1]]
  myDF$treat_yes_jimok <- myDF[[var2]]

  results <- feols(log(propertyVal_2000) ~ time + treat_no_reclass + treat_yes_reclass + treat_no_reclass:time + treat_yes_reclass:time +
                     area_ha + builtup_km + road_km + powerline_km + soilQlt
                   | pnu+propertyYr^ADM_NM,
                   cluster = ~ ADM_NM + propertyYr,
                   data = myDF %>% filter(yrDif >= -10 & yrDif <= 5))
  return(results)
}
## Table S11 Column (3)
m3.fe(appr_n10) %>% summary()
m3.fe(appr_n10) %>% doZtest_lu()
