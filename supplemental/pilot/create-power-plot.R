################################################################################
# Create Power Analysis Visualization
################################################################################

library(ggplot2)
library(dplyr)
library(gridExtra)

# Read pilot data
d0 <- read.csv('pilot-study.csv', check.names = FALSE)

# Calculate proportions by condition
results <- d0 %>%
  group_by(base_condition, gender_feedback) %>%
  summarize(
    n = n(),
    prop_female = mean(female_pick, na.rm = TRUE),
    se = sd(female_pick, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  mutate(
    condition = factor(gender_feedback,
                      levels = c(0, 1),
                      labels = c("Control", "Treatment")),
    base = factor(base_condition,
                 levels = c("low", "high"),
                 labels = c("Low Base\n(6 authors)", "High Base\n(12 authors)"))
  )

# Create interaction plot
p1 <- ggplot(results, aes(x = base, y = prop_female * 100,
                          color = condition, group = condition)) +
  geom_point(size = 4) +
  geom_line(linewidth = 1.2) +
  geom_errorbar(aes(ymin = (prop_female - 1.96*se) * 100,
                   ymax = (prop_female + 1.96*se) * 100),
               width = 0.1, linewidth = 1) +
  scale_color_manual(values = c("Control" = "#990000", "Treatment" = "#011F5B")) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
  labs(
    title = "Pilot Data: Crossover Interaction (N=301)",
    subtitle = "Gender Feedback has opposite effects depending on Base Condition",
    x = "Base Condition",
    y = "% Selecting Female Author",
    color = "Condition"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40")
  )

# Sample size comparison
sample_sizes <- data.frame(
  Analysis = c("Interaction\n(Recommended)",
               "Simple Effects\n(Per Condition)",
               "Conservative\n(Robust)"),
  N_Total = c(72, 2424, 500),
  N_Per_Cell = c(18, 606, 125),
  Feasibility = c("Very Feasible", "Very Expensive", "Feasible"),
  Order = c(2, 3, 1)
) %>%
  arrange(Order) %>%
  mutate(Analysis = factor(Analysis, levels = Analysis))

p2 <- ggplot(sample_sizes, aes(x = Analysis, y = N_Total, fill = Feasibility)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0("N = ", N_Total, "\n(", N_Per_Cell, " per cell)")),
           vjust = -0.3, size = 4) +
  scale_fill_manual(values = c("Very Feasible" = "#4CAF50",
                               "Feasible" = "#FFC107",
                               "Very Expensive" = "#F44336")) +
  scale_y_continuous(limits = c(0, 2800), breaks = seq(0, 2800, 400)) +
  labs(
    title = "Required Sample Sizes (80% Power)",
    subtitle = "Different analysis strategies require very different N",
    x = "",
    y = "Total N Required"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40")
  )

# Power curves for different sample sizes
sample_range <- seq(100, 1000, by = 50)
library(pwr)

# Cohen's h for interaction (approximate using low base)
h_effect <- 0.165  # Average from pilot

power_data <- data.frame(
  N_Total = sample_range,
  N_Per_Cell = sample_range / 4
) %>%
  rowwise() %>%
  mutate(
    Power_Interaction = pwr.f2.test(
      u = 1,
      f2 = 0.11,  # From pilot interaction
      sig.level = 0.05,
      v = N_Total - 4
    )$power,
    Power_Simple = pwr.2p.test(
      h = h_effect,
      n = N_Per_Cell,
      sig.level = 0.05
    )$power
  ) %>%
  ungroup()

power_long <- power_data %>%
  tidyr::pivot_longer(
    cols = starts_with("Power_"),
    names_to = "Analysis",
    values_to = "Power",
    names_prefix = "Power_"
  ) %>%
  mutate(
    Analysis = factor(Analysis,
                     levels = c("Interaction", "Simple"),
                     labels = c("Interaction Effect", "Simple Effects"))
  )

p3 <- ggplot(power_long, aes(x = N_Total, y = Power * 100, color = Analysis)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = c(300, 500), linetype = "dotted", color = "gray60") +
  annotate("text", x = 300, y = 95, label = "Pilot N", angle = 90,
           vjust = -0.5, color = "gray40") +
  annotate("text", x = 500, y = 95, label = "Recommended N", angle = 90,
           vjust = -0.5, color = "gray40") +
  annotate("text", x = 850, y = 82, label = "80% Power Threshold",
           color = "gray40", size = 4) +
  scale_color_manual(values = c("Interaction Effect" = "#4CAF50",
                                "Simple Effects" = "#F44336")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_x_continuous(breaks = seq(100, 1000, 100)) +
  labs(
    title = "Statistical Power by Sample Size",
    subtitle = "Interaction is well-powered at N=500; Simple effects need N=1000+",
    x = "Total Sample Size",
    y = "Statistical Power (%)",
    color = "Analysis Type"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40")
  )

# Save plots
ggsave("power-analysis-interaction.pdf", plot = p1, width = 10, height = 6)
ggsave("power-analysis-sample-sizes.pdf", plot = p2, width = 10, height = 6)
ggsave("power-analysis-curves.pdf", plot = p3, width = 10, height = 6)

# Combined plot
p_combined <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1)
ggsave("power-analysis-full.pdf", plot = p_combined, width = 10, height = 16)

cat("✓ Power analysis plots saved:\n")
cat("  - power-analysis-interaction.pdf\n")
cat("  - power-analysis-sample-sizes.pdf\n")
cat("  - power-analysis-curves.pdf\n")
cat("  - power-analysis-full.pdf (combined)\n")
