## =================================================================================================================================
## 01_main_did.R
## Table 1  Price effects of solar development (Columns 1-3)
## Requires: 00_setup.R
## =================================================================================================================================
## ---------------------------------------------------------------------------------------------------------------------------------
## Model 1: Baseline DID  ->  Table 1 Column (1)
## ---------------------------------------------------------------------------------------------------------------------------------
m1 <- function(myDF){
  results <- feols(log(propertyVal_2000) ~ time+treat+treat:time + area_ha + builtup_km + road_km + powerline_km + soilQlt
                   | propertyYr^ADM_NM,
                   cluster = ~ ADM_NM + propertyYr,
                   data = myDF %>%
                     filter(yrDif >= -10 & yrDif <= 5))
  return(results)
}

m1(appr_n10) %>% summary()

## ---------------------------------------------------------------------------------------------------------------------------------
## Model 2: Expected energy income  ->  Table 1 Column (2)
## Annual Expected Electricity Generation (kWh/year) = PVOUT (kWh/kWp/year) x Installed Capacity (kWp)
## ---------------------------------------------------------------------------------------------------------------------------------
# Compute expected generation, classify into high/low groups by four cutoff
# methods, and generate treatment interaction terms
prep_gen_groups <- function(df, name) {
  df <- df %>% mutate(expected_gen = pvout * capa)

  vals <- df %>%
    filter(treat == 1) %>%
    distinct(pnu, .keep_all = TRUE) %>%
    pull(expected_gen)

  # cutoff points
  km <- kmeans(na.omit(vals), centers = 2, nstart = 25)
  cutoffs <- c(
    median = median(vals, na.rm = TRUE),
    mean   = mean(vals, na.rm = TRUE),
    jenks  = classIntervals(vals[!is.na(vals)], n = 2, style = "jenks")$brks[2],
    kmeans = mean(sort(km$centers))
  )

  cat(sprintf("%s - %s: %s\n", name, names(cutoffs), cutoffs), sep = "")

  # assign group and treatment interaction terms for each cutoff method
  for (method in names(cutoffs)) {
    suffix <- if (method == "median") "" else paste0("_", method)
    group_col <- paste0("gen_group", suffix)
    df[[group_col]] <- if_else(df$expected_gen >= cutoffs[[method]], "high", "low")
    df[[paste0("treat_low_gen", suffix)]]  <- df$treat * (df[[group_col]] == "low")
    df[[paste0("treat_high_gen", suffix)]] <- df$treat * (df[[group_col]] == "high")
  }

  df
}

# Apply to all datasets and assign back to the global environment
df_names <- c("inst_n5", "inst_n10", "inst_n15", "inst_n30",
              "appr_n5", "appr_n10", "appr_n15", "appr_n30")

df_list <- mget(df_names)
df_list <- Map(prep_gen_groups, df_list, df_names)
list2env(df_list, envir = .GlobalEnv)

m2 <- function(myDF){
  results <- feols(log(propertyVal_2000) ~ time + treat_low_gen +treat_high_gen + treat_low_gen:time + treat_high_gen:time +
                     area_ha + builtup_km + road_km + powerline_km + soilQlt
                   | propertyYr^ADM_NM,
                   cluster = ~ ADM_NM + propertyYr,
                   data = myDF %>% filter(yrDif >= -10 & yrDif <= 5))
  return(results)
}

m2_jenks <- function(myDF){
  feols(log(propertyVal_2000) ~ time + treat_low_gen_jenks + treat_high_gen_jenks + treat_low_gen_jenks:time + treat_high_gen_jenks:time +
          area_ha + builtup_km + road_km + powerline_km + soilQlt
        | propertyYr^ADM_NM,
        cluster = ~ ADM_NM + propertyYr,
        data = myDF %>% filter(yrDif >= -10 & yrDif <= 5))
}

m2_kmeans <- function(myDF){
  feols(log(propertyVal_2000) ~ time + treat_low_gen_kmeans + treat_high_gen_kmeans + treat_low_gen_kmeans:time + treat_high_gen_kmeans:time +
          area_ha + builtup_km + road_km + powerline_km + soilQlt
        | propertyYr^ADM_NM,
        cluster = ~ ADM_NM + propertyYr,
        data = myDF %>% filter(yrDif >= -10 & yrDif <= 5))
}

m2(appr_n10) %>% summary()

## z-test for difference in coefficients between low- and high-generation groups
## suffix: "" for median cutoff, or "_mean"/"_jenks"/"_kmeans" for other methods
doZtest_gen <- function(myModel, suffix = "") {
  low_var  <- paste0("time:treat_low_gen", suffix)
  high_var <- paste0("time:treat_high_gen", suffix)

  # 1. extract coefficients
  b_small <- coef(myModel)[low_var]
  b_big   <- coef(myModel)[high_var]

  # 2. variance-covariance matrix
  V <- vcov(myModel)
  var_diff <- V[low_var, low_var] + V[high_var, high_var] - 2 * V[low_var, high_var]

  # 3. Z-statistic and p-value
  z_stat <- (b_small - b_big) / sqrt(var_diff)
  p_val  <- 2 * (1 - pnorm(abs(z_stat)))

  c(beta_small = b_small, beta_big = b_big, diff = b_small - b_big,
    z_statistic = z_stat, p_value = p_val)
}

## median cutoff: Table 1 Column (2)
m2(appr_n10) %>% doZtest_gen()

## other cutoff methods -> Table S5 Sensitivity of results to energy-generation group classification method
m2_jenks(appr_n10)  %>% doZtest_gen(suffix = "_jenks")
m2_kmeans(appr_n10) %>% doZtest_gen(suffix = "_kmeans")

## ---------------------------------------------------------------------------------------------------------------------------------
## Model 3: Land-use reclassification  ->  Table 1 Column (3)
## ---------------------------------------------------------------------------------------------------------------------------------
reclassGroup <- function(myDF){
  myDF <- myDF %>%
    mutate(
      treat_no_reclass  = treat * (treat_reclass == 0),
      treat_yes_reclass = treat * (treat_reclass == 1)
    )
  return(myDF)
}
inst_n5  <- reclassGroup(inst_n5)
inst_n10 <- reclassGroup(inst_n10)
inst_n15 <- reclassGroup(inst_n15)
inst_n30 <- reclassGroup(inst_n30)

appr_n5  <- reclassGroup(appr_n5)
appr_n10 <- reclassGroup(appr_n10)
appr_n15 <- reclassGroup(appr_n15)
appr_n30 <- reclassGroup(appr_n30)

m3 <- function(myDF, var1 = "treat_no_reclass", var2 = "treat_yes_reclass"){
  myDF$treat_no_reclass <- myDF[[var1]]
  myDF$treat_yes_reclass <- myDF[[var2]]

  results <- feols(log(propertyVal_2000) ~ time + treat_no_reclass + treat_yes_reclass + treat_no_reclass:time + treat_yes_reclass:time +
                     area_ha + builtup_km + road_km + powerline_km + soilQlt
                   | propertyYr^ADM_NM,
                   cluster = ~ ADM_NM + propertyYr,
                   data = myDF %>% filter(yrDif >= -10 & yrDif <= 5))
  return(results)
}

m3(appr_n10, 'treat_no_reclass', 'treat_yes_reclass') %>% summary()

# Z-test for difference in coefficients between two treatment groups
## var_no / var_yes: column name suffixes used in the model (without "time:")
doZtest_lu <- function(myModel, var_no = "treat_no_reclass", var_yes = "treat_yes_reclass") {
  no_var  <- paste0("time:", var_no)
  yes_var <- paste0("time:", var_yes)

  # 1. extract coefficients
  b_no  <- coef(myModel)[no_var]
  b_yes <- coef(myModel)[yes_var]

  # 2. variance-covariance matrix
  V <- vcov(myModel)
  var_diff <- V[no_var, no_var] + V[yes_var, yes_var] - 2 * V[no_var, yes_var]

  # 3. Z-statistic and p-value
  z_stat <- (b_no - b_yes) / sqrt(var_diff)
  p_val  <- 2 * (1 - pnorm(abs(z_stat)))

  c(beta_no = b_no, beta_yes = b_yes, diff = b_no - b_yes,
    z_statistic = z_stat, p_value = p_val)
}
m3(appr_n10, 'treat_no_reclass', 'treat_yes_reclass') %>% doZtest_lu()
