## =================================================================================================================================
## 03_parallel_trends.R
## Fig. 6  Property values according to event time
## Requires: 00_setup.R, 01_main_did.R (uses appr_n10)
## =================================================================================================================================
appr_n10 %>%
  filter(yrDif >= -10 & yrDif <= 5) %>%
  group_by(yrDif, treat) %>%
  summarise(propertyVal_2000 = mean(propertyVal_2000, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = yrDif, y = propertyVal_2000, color = factor(treat))) +
  annotate("segment",
           x    = 0, xend = 0,
           y    = 0, yend = 120000,
           linetype = "dashed", color = "grey70",
           linewidth=0.5) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2             ,
             shape= 16) +
  coord_cartesian(ylim = c(0, 50000)) +
  scale_y_continuous(labels = scales::comma) +   # ← 추가
  labs(
    # x = "Years Relative to Solar Panel Installation",
    x = 'Event Time (Years)',
    y = "Property Value (2000 KRW)") +
  scale_color_manual(
    values = c("0" = "#377EB8",
               "1" = "#E41A1C"),
    labels = c("0" = "Control", "1" = "Treated")
  ) +
  theme_classic(base_size = 13) +
  theme(
    text             = element_text(family = "DejaVu Sans"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.3),
    axis.line        = element_blank(),
    axis.ticks        = element_line(linewidth = 0.3),
    legend.position  = c(0.10, 0.90),
    legend.title     = element_blank(),
    legend.key       = element_rect(fill = "white", color = "white"),
    axis.text.x      = element_text(angle = 0, hjust = 1,
                                    family = "DejaVu Sans", color = "black"),
    axis.text.y      = element_text(family = "DejaVu Sans", color = "black"),
    axis.title.y     = element_text(margin = margin(r = 10)),
    axis.title.x     = element_text(margin = margin(t = 10)),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )
