## =================================================================================================================================
## 04_event_study.R
## Table S2  Event study estimates (installation timing)
## Table S3  Event study estimates (approval timing)
## Table S5  Event study estimates according to expected solar generation
## Table S6  Event study estimates according to land-use reclassification status
## Fig. 4    Estimated effects of solar development on property values
## Fig. S2   ... according to solar-generation potential
## Fig. S3   ... according to land-use reclassification status
## Requires: 00_setup.R, 01_main_did.R
## =================================================================================================================================
## Model 1
m1_es <- function(myDF){
  results <- feols(log(propertyVal_2000) ~ i(yrDif, treat, ref = -1) 
                              + area_ha + builtup_km + road_km + powerline_km + soilQlt |
                     propertyYr^ADM_NM,
                   cluster = ~ ADM_NM + propertyYr,        
                   data = myDF %>% filter(yrDif >= -10 & yrDif <= 5
                                          ))
  return(results)}
## Table S2 Event study estimates (installation timing)
m1_es(inst_n10) %>% summary() 
## Table S3 Event study estimates (approval timing)
m1_es(appr_n10) %>% summary() 

## Model 2
m2_es <- function(myDF){
  results <- feols(log(propertyVal_2000) ~ i(yrDif, treat_low_gen,  ref = -1) +
                     i(yrDif, treat_high_gen, ref = -1) +
                     area_ha + builtup_km + road_km + powerline_km +  soilQlt
                   | propertyYr^ADM_NM,
                   cluster = ~ ADM_NM + propertyYr,
                   data = myDF %>% filter(yrDif >= -10 & yrDif <= 5))
  return(results)
}
## Table S5 Event study estimates according to expected solar generation
m2_es(appr_n10) %>% summary() 

## MODEL 3
m3_es <- function(myDF, var1 = "treat_no_reclass", var2 = "treat_yes_reclass"){
  myDF$treat_no_jimok <- myDF[[var1]]
  myDF$treat_yes_jimok <- myDF[[var2]]

  results <- feols(log(propertyVal_2000) ~ i(yrDif, treat_no_jimok,  ref = -1) +
                     i(yrDif, treat_yes_jimok, ref = -1) +
                     area_ha + builtup_km + road_km + powerline_km + soilQlt
                   | propertyYr^ADM_NM,
                   cluster = ~ ADM_NM + propertyYr,
                   data = myDF %>% filter(yrDif >= -10 & yrDif <= 5))
  return(results)
  }
## Table S6 Event study estimates according to land-use reclassification status
m3_es(appr_n10,  'treat_no_reclass', 'treat_yes_reclass') %>% summary() 

## ---------------------------------------------------------------------------------------------------------------------------------
## Event study plots
## ---------------------------------------------------------------------------------------------------------------------------------
# 1. Build event-study data for a single coefficient group
make_es_data_group <- function(model, treat_pattern, group_label) {
  coefs <- coef(model)
  ses   <- se(model)

  es_terms <- grep(treat_pattern, names(coefs), value = TRUE)

  data.frame(
    term = es_terms,
    estimate = coefs[es_terms],
    se = ses[es_terms]
  ) %>%
    mutate(
      conf.low  = estimate - 1.96 * se,
      conf.high = estimate + 1.96 * se,
      year      = as.numeric(gsub("yrDif::(-?\\d+):.*", "\\1", term)),
      group     = group_label
    )
}

# Build event-study data for two coefficient groups from the same model and stack them
make_es_data_pair <- function(model, pattern1, label1, pattern2, label2) {
  bind_rows(
    make_es_data_group(model, pattern1, label1),
    make_es_data_group(model, pattern2, label2)
  )
}

# 2. Extract event-study data for each model
# M1: single treatment group
es_m1      <- make_es_data_group(m1_es(appr_n10), "yrDif::.*:treat$", "All")
es_m1_inst <- make_es_data_group(m1_es(inst_n10), "yrDif::.*:treat$", "All")

# M2.2: low_gen vs. high_gen
es_m2 <- make_es_data_pair(
  m2_es(appr_n10),
  "yrDif::.*:treat_low_gen",  "Low Generation",
  "yrDif::.*:treat_high_gen", "High Generation"
)

# M3: no_reclass vs. yes_reclass
m3_model <- m3_es(appr_n10, "treat_no_reclass", "treat_yes_reclass")
es_m3 <- make_es_data_pair(
  m3_model,
  "yrDif::.*:treat_no_jimok",  "No Reclass",
  "yrDif::.*:treat_yes_jimok", "Yes Reclass"
)

# 3. Plot function for a single group (M1)
plot_es_single <- function(es) {
  ggplot(es, aes(x = year, y = estimate)) +
    annotate("segment",
             x = min(es$year), xend = max(es$year),
             y = 0, yend = 0,
             linetype = "solid", color = "grey70", linewidth = 0.3) +
    annotate("segment",
             x = -1, xend = -1,
             y = -0.15, yend = 0.77,
             linetype = "dashed", color = "grey70", linewidth = 0.3) +
    geom_point(shape = 16, size = 2, color = "#E41A1C") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0, linewidth = 0.3, color = "#E41A1C") +
    labs(x = "Event Time", y = "Effect on log land value") +
    scale_y_continuous(labels = comma, limits = c(-0.15, 0.77)) +
    theme_classic(base_size = 13) +
    theme(
      text         = element_text(family = "DejaVu Sans"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
      axis.line    = element_blank(),
      axis.ticks   = element_line(linewidth = 0.3),
      axis.text    = element_text(family = "DejaVu Sans", color = "black"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.margin  = margin(t = 5, r = 5, b = 5, l = 5)
    )
}

# 4. Plot function for two groups (M2, M3)
plot_es_two <- function(es,
                        colors = c("#377EB8", "#E41A1C"),
                        shapes = c(17, 16),
                        labels = NULL) {
  groups <- unique(es$group)

  # Use group names as labels if none supplied
  if (is.null(labels)) labels <- groups

  ggplot(es, aes(x = year, y = estimate,
                 color = group, shape = group)) +
    annotate("segment",
             x = min(es$year), xend = max(es$year),
             y = 0, yend = 0,
             linetype = "solid", color = "grey70", linewidth = 0.3) +
    annotate("segment",
             x = -1, xend = -1,
             y = -0.15, yend = 0.77,
             linetype = "dashed", color = "grey70", linewidth = 0.3) +
    geom_point(size = 2,
               position = position_dodge(width = 0.4)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0, linewidth = 0.3,
                  position = position_dodge(width = 0.4)) +
    scale_color_manual(values = setNames(colors, groups), labels = labels) +
    scale_shape_manual(values = setNames(shapes, groups), labels = labels) +
    labs(x = "Event Time", y = "Change in log land Value",
         color = NULL, shape = NULL) +
    scale_y_continuous(labels = scales::comma, limits = c(-0.15, 0.77)) +
    theme_classic(base_size = 13) +
    theme(
      text         = element_text(family = "DejaVu Sans"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
      axis.line    = element_blank(),
      axis.ticks   = element_line(linewidth = 0.3),
      axis.text    = element_text(family = "DejaVu Sans", color = "black"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.position  = "right",
      legend.key.size  = unit(0.4, "cm"),
      plot.margin  = margin(t = 5, r = 5, b = 5, l = 5)
    )
}

## Fig. 4 Estimated effects of solar development on property values
plot_es_single(es_m1)

## Fig. S2 Estimated effects of solar development on property values according to solar-generation potential
plot_es_two(es_m2, 
            colors = c("#E41A1C", "#377EB8"),
            shapes = c(16, 17),
            labels = c("Low Generation", "High Generation"))

## Fig. S3 Estimated effects of solar development on property values according to land-use reclassification status
plot_es_two(es_m3, 
            colors = c("#377EB8", "#E41A1C"),
            shapes = c(17, 16),
            labels = c("LU change (X)", "LU change (O)"))
