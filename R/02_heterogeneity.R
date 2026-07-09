## =================================================================================================================================
## 02_heterogeneity.R
## Fig. 5   Heterogeneous effects of solar development on property values
## Table S7 Heterogeneous effects of solar PV on land values
## Requires: 00_setup.R, 01_main_did.R (uses appr_n10 with treat_no_reclass / treat_yes_reclass and m3)
## =================================================================================================================================
## Heterogeneous effect on log property value
## Both treated and control units are used for subsampling
add_hetero_groups <- function(df, var1, var2) {
  df$treat_no_jimok  <- df[[var1]]
  df$treat_yes_jimok <- df[[var2]]

  # Time-invariant medians, computed on treat + control combined
  base_static <- df %>% distinct(pnu, .keep_all = TRUE)
  area_median <- median(base_static$area_m2, na.rm = TRUE)
  soil_median <- median(base_static$soilQlt, na.rm = TRUE)

  # Time-varying medians, computed at baseline (yrDif == -1)
  base_t0 <- df %>% filter(yrDif == -1)
  builtup_median    <- median(base_t0$builtup,      na.rm = TRUE)
  road_median       <- median(base_t0$road,         na.rm = TRUE)
  powerline_median  <- median(base_t0$powerline_km, na.rm = TRUE)

  df <- df %>%
    mutate(
      areaGroup      = if_else(area_m2      >= area_median,      "Large lot",          "Small lot"),
      builtupGroup   = if_else(builtup      >= builtup_median,   "Far from builtup",   "Near builtup"),
      roadGroup      = if_else(road         >= road_median,      "Far from road",      "Near road"),
      powerlineGroup = if_else(powerline_km >= powerline_median, "Far from powerline", "Near powerline"),
      soilGroup      = if_else(soilQlt      <  soil_median,      "High-quality soil",  "Low-quality soil")
    )

  return(df)
}

appr_n10 <- add_hetero_groups(appr_n10, 'treat_no_reclass', 'treat_yes_reclass')

## MODEL 3 by subgroup
run_did_LU <- function(df, group_var, group_value){
  feols(log(propertyVal_2000) ~ time + treat_no_reclass + treat_yes_reclass + treat_no_reclass:time + treat_yes_reclass:time +
          area_ha + builtup_km + powerline_km + road_km + soilQlt
        | propertyYr^ADM_NM,
        cluster = ~ ADM_NM + propertyYr,
        data = df %>%
          filter(.data[[group_var]] == group_value,
                 yrDif >= -10 & yrDif <= 5))}

extract_te_LU <- function(model, group, LU_type){
  coef_name <- ifelse(
    LU_type == "No LU change",
    "time:treat_no_reclass",
    "time:treat_yes_reclass"
  )

  est <- coef(model)[coef_name]
  se  <- se(model)[coef_name]

  data.frame(
    group = group,
    LU = LU_type,
    estimate = unname(est),
    conf.low  = unname(est - 1.96 * se),
    conf.high = unname(est + 1.96 * se)
  )
}

# Heterogeneity specs: (group_var, value1/value2, type label)
hetero_specs <- list(
  list(var = "areaGroup",       vals = c("Small lot", "Large lot"),
       type = "Area size"),
  list(var = "builtupGroup",    vals = c("Far from builtup", "Near builtup"),
       type = "Distance to builtup"),
  list(var = "roadGroup",       vals = c("Far from road", "Near road"),
       type = "Distance to road"),
  list(var = "powerlineGroup",  vals = c("Far from powerline", "Near powerline"),
       type = "Distance to powerline"),
  list(var = "soilGroup",       vals = c("Low-quality soil", "High-quality soil"),
       type = "Soil quality")
)

# Run models and extract treatment effects for each subgroup
plot_all_LU <- bind_rows(lapply(hetero_specs, function(spec) {
  models <- lapply(spec$vals, function(v) run_did_LU(appr_n10, spec$var, v))

  te <- bind_rows(lapply(seq_along(spec$vals), function(i) {
    bind_rows(
      extract_te_LU(models[[i]], spec$vals[i], "No LU change"),
      extract_te_LU(models[[i]], spec$vals[i], "LU change")
    )
  }))
  te$type <- spec$type
  te
}))

plot_all_LU$group <- factor(
  plot_all_LU$group,
  levels = c(
    "Small lot",        "Large lot",
    "Far from builtup", "Near builtup",
    "Far from road",    "Near road",
    "Far from powerline",    "Near powerline",
    "Low-quality soil", "High-quality soil"
  )
)

plot_all_LU$LU <- factor(
  plot_all_LU$LU,
  levels = c("No LU change", "LU change")
)

group_colors <- c(
  "Small lot"          = "#E41A1C",
  "Large lot"          = "#E41A1C",
  "Far from built-up"   = "#377EB8",
  "Near built-up"       = "#377EB8",
  "Far from road"      = "#FF7F00",
  "Near road"          = "#FF7F00",
  "Far from powerline" = "#4DAF4A",
  "Near powerline"     = "#4DAF4A",
  "Low-quality soil"   = "#984EA3",
  "High-quality soil"  = "#984EA3"
)

plot_all_LU <- plot_all_LU %>%
  mutate(group = fct_recode(group,
                            "Far from built-up" = "Far from builtup",
                            "Near built-up"     = "Near builtup"
  ))

## Fig. 5 Heterogeneous effects of solar development on property values
ggplot(plot_all_LU,
       aes(x = group, y = estimate, color = group, shape = LU)) +
  annotate("segment",
           x = "Small lot", xend = "High-quality soil",
           y = 0, yend = 0,
           linetype = "solid", color = "grey70", linewidth = 0.3) +
  geom_point(size = 3, position = position_dodge(width = 0.7)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0, #  에러가 cap 없애기!!
    linewidth = 0.3,
    position = position_dodge(width = 0.7)
  ) +
  coord_cartesian(ylim = c(-0.05, 0.75)) +
  labs(x = "", y = "Effect on log land value") +
  scale_color_manual(values = group_colors, guide = "none") +
  scale_shape_manual(
    values = c("No LU change" = 17, "LU change" = 16),
    labels = c("No LU change" = "LU change (X)", "LU change" = "LU change (O)")
  ) +
  theme_classic(base_size = 13) +
  theme(
    text              = element_text(family = "DejaVu Sans"),
    panel.border      = element_rect(color = "black", fill = NA, linewidth = 0.3),
    axis.line         = element_blank(),
    axis.ticks        = element_line(linewidth = 0.3),
    legend.position   = c(0.85, 0.90),
    legend.title      = element_blank(),
    legend.key        = element_rect(fill = "white", color = "white"),
    axis.text.x       = element_text(angle = 30, hjust = 1, color = "black"),
    axis.text.y       = element_text(color = "black"),
    axis.title.y      = element_text(margin = margin(r = 10)),
    plot.margin       = margin(t = 5, r = 5, b = -13, l = 5)
  )

## Within-group comparison: LU change (O) vs. No LU change (X)
compare_lu_within <- function(model, group_label) {
  V <- vcov(model)

  est_yes <- as.numeric(coef(model)["time:treat_yes_reclass"])
  est_no  <- as.numeric(coef(model)["time:treat_no_reclass"])
  se_yes  <- as.numeric(se(model)["time:treat_yes_reclass"])
  se_no   <- as.numeric(se(model)["time:treat_no_reclass"])

  p_yes <- 2 * (1 - pnorm(abs(est_yes / se_yes)))
  p_no  <- 2 * (1 - pnorm(abs(est_no  / se_no)))

  var_diff <- V["time:treat_yes_reclass", "time:treat_yes_reclass"] +
    V["time:treat_no_reclass",  "time:treat_no_reclass"]  -
    2 * V["time:treat_yes_reclass", "time:treat_no_reclass"]

  z <- (est_yes - est_no) / sqrt(var_diff)
  p <- 2 * (1 - pnorm(abs(z)))

  data.frame(
    Group     = group_label,
    N         = nobs(model),
    Est_LU_X  = round(est_no,  4),
    SE_LU_X   = round(se_no,   4),
    Pval_LU_X = ifelse(p_no  < 0.001, "<0.001", as.character(round(p_no,  3))),
    Est_LU_O  = round(est_yes, 4),
    SE_LU_O   = round(se_yes,  4),
    Pval_LU_O = ifelse(p_yes < 0.001, "<0.001", as.character(round(p_yes, 3))),
    Diff      = round(est_yes - est_no, 4),
    Z_stat    = round(z, 3),
    P_value   = round(p, 3)
  )
}

## Between-group comparison: same LU status, different subgroup
compare_groups_lu <- function(m1, m2, label1, label2, lu_type) {
  coef_name <- ifelse(lu_type == "LU change",
                      "time:treat_yes_reclass",
                      "time:treat_no_reclass")

  est1 <- as.numeric(coef(m1)[coef_name]); se1 <- as.numeric(se(m1)[coef_name])
  est2 <- as.numeric(coef(m2)[coef_name]); se2 <- as.numeric(se(m2)[coef_name])

  z <- (est1 - est2) / sqrt(se1^2 + se2^2)
  p <- 2 * (1 - pnorm(abs(z)))

  data.frame(
    LU        = lu_type,
    Subgroup1 = label1, Estimate1 = round(est1, 4), SE1 = round(se1, 4),
    Subgroup2 = label2, Estimate2 = round(est2, 4), SE2 = round(se2, 4),
    Z_stat    = round(z, 3),
    P_value   = round(p, 3)
  )
}

# Fit heterogeneity models for each subgroup
appr_small          <- run_did_LU(appr_n10, "areaGroup", "Small lot")
appr_big            <- run_did_LU(appr_n10, "areaGroup", "Large lot")

appr_farBuiltup     <- run_did_LU(appr_n10, "builtupGroup", "Far from builtup")
appr_nearBuiltup    <- run_did_LU(appr_n10, "builtupGroup", "Near builtup")

appr_farRoad        <- run_did_LU(appr_n10, "roadGroup", "Far from road")
appr_nearRoad       <- run_did_LU(appr_n10, "roadGroup", "Near road")

appr_farPowerline   <- run_did_LU(appr_n10, "powerlineGroup", "Far from powerline")
appr_nearPowerline  <- run_did_LU(appr_n10, "powerlineGroup", "Near powerline")

appr_lowQ           <- run_did_LU(appr_n10, "soilGroup", "Low-quality soil")
appr_highQ          <- run_did_LU(appr_n10, "soilGroup", "High-quality soil")

# Models and paired subgroup labels for heterogeneity comparisons
lu_models <- list(
  "Small lot"           = appr_small,
  "Large lot"           = appr_big,
  "Far from builtup"    = appr_farBuiltup,
  "Near builtup"        = appr_nearBuiltup,
  "Far from road"       = appr_farRoad,
  "Near road"           = appr_nearRoad,
  "Far from powerline"  = appr_farPowerline,
  "Near powerline"      = appr_nearPowerline,
  "Low-quality soil"    = appr_lowQ,
  "High-quality soil"   = appr_highQ
)

group_pairs <- list(
  c("Small lot", "Large lot"),
  c("Far from builtup", "Near builtup"),
  c("Far from road", "Near road"),
  c("Far from powerline", "Near powerline"),
  c("Low-quality soil", "High-quality soil")
)

# Table 1: within-group LU change vs. no LU change, for all 10 subgroups
table1 <- bind_rows(lapply(names(lu_models), function(g) {
  compare_lu_within(lu_models[[g]], g)
}))

# Table 2 & 3: between-group comparisons, separately for LU change and no LU change
table2 <- bind_rows(lapply(group_pairs, function(pair) {
  compare_groups_lu(lu_models[[pair[1]]], lu_models[[pair[2]]], pair[1], pair[2], "LU change")
}))

table3 <- bind_rows(lapply(group_pairs, function(pair) {
  compare_groups_lu(lu_models[[pair[1]]], lu_models[[pair[2]]], pair[1], pair[2], "No LU change")
}))

## Table S7 Heterogeneous effects of solar PV on land values
cat("=== 1. Within-group: LU change (O) vs. No LU change (X) ===\n"); print(table1)
cat("=== 2. Between-group comparison, LU change (O) ===\n"); print(table2)
cat("=== 3. Between-group comparison, No LU change (X) ===\n"); print(table3)
