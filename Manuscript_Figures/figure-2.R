# Clear the workspace
rm(list=ls())

# Load required libraries
library(ggplot2)
library(dplyr)
library(extrafont)
library(patchwork)

# Load the Times New Roman font
loadfonts(device = "pdf")

# Define the path to the estimates
estimate_path <- paste0(getwd(), "/Manuscript_Figures/figure2-estimates/")

# Read in the estimates from the .Rds files
study1_estimates <- readRDS(paste0(estimate_path, "study1_estimates.rds"))
study2_estimates <- readRDS(paste0(estimate_path, "study2_estimates.rds"))
study3A_estimates <- readRDS(paste0(estimate_path, "study3A_estimates.rds"))
study3B_estimates <- readRDS(paste0(estimate_path, "study3B_estimates.rds"))
study4A_estimates <- readRDS(paste0(estimate_path, "study4A_estimates.rds"))
study4B_estimates <- readRDS(paste0(estimate_path, "study4B_estimates.rds"))

# Fix study names to match new numbering scheme
# Old "STUDY 5" -> New "STUDY 2"
# Old "STUDY 2A" -> New "STUDY 3A"
# Old "STUDY 2B" -> New "STUDY 3B"
# Old "STUDY 3A" -> New "STUDY 4A"
# Old "STUDY 3B" -> New "STUDY 4B"
study2_estimates$Study <- "STUDY 2"
study3A_estimates$Study <- "STUDY 3A"
study3B_estimates$Study <- "STUDY 3B"
study4A_estimates$Study <- "STUDY 4A"
study4B_estimates$Study <- "STUDY 4B"

# Study 1 uses "women" as the stimuli, normalize to "gender" for consistency
study1_estimates <- study1_estimates %>%
  mutate(Stimuli = ifelse(Stimuli == "women", "gender", Stimuli))

# Combine all estimates into a single data frame
data <- bind_rows(study1_estimates, study2_estimates, study3A_estimates,
                  study3B_estimates, study4A_estimates, study4B_estimates)

# Update the study order (now includes Study 1)
study_order <- c('STUDY 1', 'STUDY 2', 'STUDY 3A', 'STUDY 3B', 'STUDY 4A', 'STUDY 4B')

# Ensure that 'Study' and 'Stimuli' are factors with the correct order
data$Study <- factor(data$Study, levels = study_order)
data$Stimuli <- as.factor(data$Stimuli)

# Calculate average estimates and SEs for non-gender and non-race stimuli
avg_data <- data %>%
  group_by(Study) %>%
  filter(!Stimuli %in% c('gender', 'race')) %>%
  summarise(
    Avg_Estimate = mean(Estimate),
    Avg_SE = sqrt(sum(SE^2)) / n(),
    .groups = 'drop'
  ) %>%
  # Calculate the 95% confidence intervals for averaged data
  mutate(
    Avg_CI_Lower = Avg_Estimate - 1.96 * Avg_SE,
    Avg_CI_Upper = Avg_Estimate + 1.96 * Avg_SE
  )

# Adjust y positions for averaged data
avg_data <- avg_data %>%
  mutate(y_pos_avg = rev(as.numeric(factor(Study, levels = study_order))))

# Filter data for gender and race stimuli and adjust y positions
orange_data <- data %>%
  filter(Stimuli %in% c('gender', 'race')) %>%
  mutate(y_pos_orange = rev(as.numeric(factor(Study, levels = study_order))))

# Define a small y-offset to separate individual points from the averaged points
y_offset <- 0.1

# Adjust y positions for non-gender and non-race stimuli
non_orange_data <- data %>%
  filter(!Stimuli %in% c('gender', 'race')) %>%
  left_join(avg_data %>% dplyr::select(Study, y_pos_avg), by = "Study") %>%
  mutate(y_pos_non_orange = y_pos_avg - y_offset)

# Combine orange, average, and non-orange data for plotting
plot_data <- bind_rows(
  orange_data %>% dplyr::select(Study, Estimate, CI_Lower, CI_Upper, Stimuli, y_pos_orange) %>%
    rename(y_pos = y_pos_orange) %>%
    mutate(Type = 'Race and Gender Feedback'),
  avg_data %>% dplyr::select(Study, Estimate = Avg_Estimate, CI_Lower = Avg_CI_Lower,
                      CI_Upper = Avg_CI_Upper, y_pos = y_pos_avg) %>%
    mutate(Type = 'Averaged Other Feedback'),
  non_orange_data %>% dplyr::select(Study, Estimate, CI_Lower, CI_Upper, y_pos = y_pos_non_orange) %>%
    mutate(Type = 'Other Feedback')
)

# Ensure study order and assign factor levels
plot_data <- plot_data %>%
  mutate(
    Study = factor(Study, levels = rev(study_order)),
    Type = factor(Type, levels = c('Race and Gender Feedback', 'Averaged Other Feedback', 'Other Feedback'))
  )

# Get y positions for studies that have orange data (gender/race feedback)
# This ensures breaks and labels have the same length
orange_studies <- unique(orange_data$Study)
y_positions_orange <- orange_data %>%
  distinct(Study, y_pos_orange) %>%
  arrange(desc(y_pos_orange)) %>%
  pull(y_pos_orange)

# For the orange panel, use only the studies that have gender/race data
orange_study_labels <- orange_data %>%
  distinct(Study, y_pos_orange) %>%
  arrange(desc(y_pos_orange)) %>%
  pull(Study) %>%
  as.character()

# For the grey panel, use all studies
y_positions_avg <- avg_data %>%
  mutate(Study = factor(Study, levels = rev(study_order))) %>%
  arrange(Study) %>%
  pull(y_pos_avg)

# Adjust y-limits for balanced spacing
y_limits <- c(min(y_positions_avg) - 0.25, max(y_positions_avg) + 0.25)

# Define colors
colors <- c('Race and Gender Feedback' = '#990000', 'Averaged Other Feedback' = "#011F5B", 'Other Feedback' = "#011F5B")

# Create Panel A (Orange Estimates)
plot_data_orange <- filter(plot_data, Type == 'Race and Gender Feedback')

p_orange <- ggplot(plot_data_orange, aes(x = Estimate, y = y_pos)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8, color = "lightgray") +
  geom_segment(aes(x = CI_Lower, xend = CI_Upper, y = y_pos, yend = y_pos), color = "#990000", linewidth = 1) +
  geom_point(fill = "#990000", shape = 21, size = 10, stroke = 1, color = "black") +
  scale_x_continuous(
    breaks = seq(-15, 30, by = 5),
    limits = c(-15, 30)
  ) +
  scale_y_continuous(
    breaks = y_positions_orange,
    labels = orange_study_labels,
    limits = y_limits
  ) +
  labs(
    x = "Percentage point increase in selections of candidates from target group (e.g., women) in response to\ndescriptive feedback about target attribute (e.g., % of initial selectees who were women)",
    y = NULL,
    title = "(A) Responses to feedback targeting past selectees' gender (e.g., % Women)\n      or race (e.g., % Racial Minorities)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 24, color = "black", hjust = 0, family = "Times New Roman"),
    axis.text.x = element_text(size = 24, color = "black", family = "Times New Roman"),
    axis.title.x = element_text(size = 24, face = "bold", margin = margin(t = 20), family = "Times New Roman"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    plot.title = element_text(size = 32, hjust = 0.01, face = "bold", family = "Times New Roman"),
    plot.title.position = "plot"
  ) +
  geom_text(
    aes(label = sprintf("%.2f", Estimate)),
    vjust = -1.5, size = 9, color = "black", family = "Times New Roman"
  ) +
  geom_text(
    aes(
      label = ifelse(Stimuli == 'gender', 'G', ifelse(Stimuli == 'race', 'R', ''))
    ),
    color = "white", size = 6, fontface = "bold", vjust = 0.35
  )

# Create Panel B (Grey Estimates) with 95% CIs
plot_data_grey <- filter(plot_data, Type %in% c('Averaged Other Feedback', 'Other Feedback'))

p_grey <- ggplot(plot_data_grey, aes(x = Estimate, y = y_pos)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8, color = "lightgray") +
  # Averaged other feedback
  geom_segment(
    data = subset(plot_data_grey, Type == 'Averaged Other Feedback'),
    aes(x = CI_Lower, xend = CI_Upper, y = y_pos, yend = y_pos, color = Type),
    linewidth = 1
  ) +
  geom_point(
    data = subset(plot_data_grey, Type == 'Averaged Other Feedback'),
    aes(fill = Type),
    shape = 21, size = 10, stroke = 1, color = "black"
  ) +
  # Individual other feedback points
  geom_point(
    data = subset(plot_data_grey, Type == 'Other Feedback'),
    aes(x = Estimate, y = y_pos),
    shape = 18, size = 5, color = "#011F5B", alpha = 0.7
  ) +
  scale_fill_manual(values = colors, guide = 'none') +
  scale_color_manual(values = colors, guide = 'none') +
  scale_x_continuous(
    breaks = seq(-15, 30, by = 5),
    limits = c(-15, 30)
  ) +
  scale_y_continuous(
    breaks = y_positions_avg,
    labels = rev(study_order),
    limits = y_limits
  ) +
  labs(
    x = "Percentage point increase in selections of candidates from target group (e.g., poets) in response to\ndescriptive feedback about target attribute (e.g., % of initial selectees who were poets)",
    y = NULL,
    title = "(B) Responses to feedback targeting past selectees' attributes besides gender/race\n      (e.g., % Political Leaders)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 24, color = "black", hjust = 0, family = "Times New Roman"),
    axis.text.x = element_text(size = 24, color = "black", family = "Times New Roman"),
    axis.title.x = element_text(size = 24, face = "bold", margin = margin(t = 20), family = "Times New Roman"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    plot.title = element_text(size = 32, hjust = 0.01, face = "bold", family = "Times New Roman"),
    plot.title.position = "plot"
  ) +
  geom_text(
    data = subset(plot_data_grey, Type == 'Averaged Other Feedback'),
    aes(label = sprintf("%.2f", Estimate)),
    vjust = -1.5, size = 9, color = "black", family = "Times New Roman"
  ) +
  geom_text(
    data = subset(plot_data_grey, Type == 'Averaged Other Feedback'),
    aes(label = 'A'),
    color = "white", size = 5, fontface = "bold"
  )

# Create a legend plot
legend_data <- data.frame(
  x = c(1.2, 1.2, 1.2),
  y = c(1.4, 1.0, 0.6),
  Type = c("Feedback about gender", "Feedback about race", "Feedback about other attributes,\non average"),
  Label = c("G", "R", "A"),
  Color = c("#011F5B", "#011F5B", "#990000")
)

legend_plot <- ggplot(legend_data, aes(x, y)) +
  # Add a white background with black border
  annotate("rect", xmin = 0.75, xmax = 4.75, ymin = 0.15, ymax = 2,
           fill = "white", color = "black", linewidth = 0.5) +
  geom_point(aes(fill = Color), shape = 21, size = 10, stroke = 1, color = "black") +
  geom_text(aes(label = Label), color = "white", size = 5, fontface = "bold") +
  geom_text(data = legend_data[1:2,], aes(label = Type), hjust = 0, nudge_x = 0.3, size = 7, family = "Times New Roman") +
  geom_text(data = legend_data[3,], aes(label = Type), hjust = 0, nudge_x = 0.3, vjust = 0.8, size = 7, family = "Times New Roman") +
  scale_fill_identity() +
  annotate("text", x = 2.375, y = 1.75, label = "Key", hjust = 0, size = 8, fontface = "bold", family = "Times New Roman") +
  theme_void() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0.5, 5), ylim = c(0.25, 3.5))

# Convert the legend plot to a grob (graphical object)
legend_grob <- ggplotGrob(legend_plot)

# Add legend to Panel B
p_grey <- p_grey +
  annotation_custom(
    grob = legend_grob,
    xmin = -17,
    xmax = 4,
    ymin = max(y_positions_avg) + 7,
    ymax = max(y_positions_avg) + 10
  )

# Combine both panels into one figure
combined_plot <- p_orange / p_grey

# Save the combined figure
ggsave("./Manuscript_Figures/Figure-2.pdf", combined_plot, width = 16, height = 24, units = "in", device = cairo_pdf, family = "Times New Roman")
ggsave("./Manuscript_Figures/Figure-2.png", combined_plot, width = 16, height = 24, units = "in", dpi = 600)
