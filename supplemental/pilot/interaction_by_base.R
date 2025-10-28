# Analysis: Treatment x Base_Gender Interaction for Low and Medium Base Conditions
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)

# Read the data
d0 <- read.csv('pilot-study.csv', check.names = FALSE)

# Robust summary function
robust_summary <- function(model) {
    robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
    model_summary <- summary(model)
    model_summary$coefficients[, "Std. Error"] <- robust_se
    model_summary$coefficients[, "t value"] <- model_summary$coefficients[, "Estimate"] / robust_se
    model_summary$coefficients[, "Pr(>|t|)"] <- 2 * pt(-abs(model_summary$coefficients[, "t value"]), df = model_summary$df[2], lower.tail = TRUE)
    return(model_summary)
}

robust_confint <- function(model, level = 0.95) {
    robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
    est <- coef(model)
    alpha <- 1 - level
    t_crit <- qt(1 - alpha / 2, df = df.residual(model))
    lower <- est - t_crit * robust_se
    upper <- est + t_crit * robust_se
    confint <- cbind(lower, upper)
    rownames(confint) <- names(est)
    colnames(confint) <- c("2.5 %", "97.5 %")
    return(confint)
}

# Split data by base condition
d0_low <- d0 %>% filter(base_condition == "low")
d0_med <- d0 %>% filter(base_condition == "med")
d0_high <- d0 %>% filter(base_condition == "high")

cat("===========================================================\n")
cat("LOW BASE CONDITION ANALYSIS\n")
cat("===========================================================\n\n")

cat("--- Mean 7th selection rate by base_gender count and treatment (LOW) ---\n")
selection_by_count_low <- d0_low %>%
  group_by(base_gender, gender_feedback) %>%
  summarise(
    n = n(),
    pct_select_woman = round(mean(female_pick) * 100, 1),
    se = round(sd(female_pick)/sqrt(n()) * 100, 2)
  ) %>%
  arrange(base_gender, gender_feedback)
print(selection_by_count_low)

cat("\n--- Interaction Model: female_pick ~ gender_feedback * base_gender (LOW) ---\n")
interaction_low <- lm(female_pick ~ gender_feedback * base_gender, data = d0_low)
print(robust_summary(interaction_low))
print(robust_confint(interaction_low))

# Interpretation
cat("\n--- INTERPRETATION (LOW BASE CONDITION) ---\n")
coefs_low <- coef(interaction_low)
cat("When base_gender = 0 (no women initially):\n")
cat("  Control: ", round(coefs_low[1] * 100, 1), "%\n")
cat("  Treatment: ", round((coefs_low[1] + coefs_low[2]) * 100, 1), "%\n")
cat("  Treatment effect: ", round(coefs_low[2] * 100, 1), "pp\n\n")

cat("For each additional woman in base:\n")
cat("  Control changes by: ", round(coefs_low[3] * 100, 1), "pp\n")
cat("  Treatment changes by: ", round((coefs_low[3] + coefs_low[4]) * 100, 1), "pp\n")
cat("  Interaction (differential slope): ", round(coefs_low[4] * 100, 1), "pp\n")

cat("\n\n===========================================================\n")
cat("MEDIUM BASE CONDITION ANALYSIS\n")
cat("===========================================================\n\n")

cat("--- Mean 7th selection rate by base_gender count and treatment (MED) ---\n")
selection_by_count_med <- d0_med %>%
  group_by(base_gender, gender_feedback) %>%
  summarise(
    n = n(),
    pct_select_woman = round(mean(female_pick) * 100, 1),
    se = round(sd(female_pick)/sqrt(n()) * 100, 2)
  ) %>%
  arrange(base_gender, gender_feedback)
print(selection_by_count_med)

cat("\n--- Interaction Model: female_pick ~ gender_feedback * base_gender (MED) ---\n")
interaction_med <- lm(female_pick ~ gender_feedback * base_gender, data = d0_med)
print(robust_summary(interaction_med))
print(robust_confint(interaction_med))

# Interpretation
cat("\n--- INTERPRETATION (MEDIUM BASE CONDITION) ---\n")
coefs_med <- coef(interaction_med)
cat("When base_gender = 0 (no women initially):\n")
cat("  Control: ", round(coefs_med[1] * 100, 1), "%\n")
cat("  Treatment: ", round((coefs_med[1] + coefs_med[2]) * 100, 1), "%\n")
cat("  Treatment effect: ", round(coefs_med[2] * 100, 1), "pp\n\n")

cat("For each additional woman in base:\n")
cat("  Control changes by: ", round(coefs_med[3] * 100, 1), "pp\n")
cat("  Treatment changes by: ", round((coefs_med[3] + coefs_med[4]) * 100, 1), "pp\n")
cat("  Interaction (differential slope): ", round(coefs_med[4] * 100, 1), "pp\n")

cat("\n\n===========================================================\n")
cat("CREATING VISUALIZATIONS\n")
cat("===========================================================\n\n")

# Prepare data for plotting - LOW CONDITION
plot_data_low <- d0_low %>%
  group_by(base_gender, gender_feedback) %>%
  summarise(
    n = n(),
    pct = mean(female_pick) * 100,
    se = sd(female_pick)/sqrt(n()) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    condition_label = ifelse(gender_feedback == 1, "Treatment (Gender Feedback)", "Control (No Gender Feedback)")
  )

# Create labels with N's for LOW
n_labels_low <- d0_low %>%
  group_by(base_gender) %>%
  summarise(total_n = n()) %>%
  mutate(label = paste0(base_gender, "\n(n=", total_n, ")"))

# Plot for LOW ONLY
p_low <- ggplot(plot_data_low, aes(x = base_gender, y = pct, color = condition_label, group = condition_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = pct - 1.96*se, ymax = pct + 1.96*se), width = 0.2, linewidth = 0.8) +
  scale_color_manual(values = c("Control (No Gender Feedback)" = "#011F5B",
                                 "Treatment (Gender Feedback)" = "#990000")) +
  scale_x_continuous(breaks = n_labels_low$base_gender,
                     labels = n_labels_low$label) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(
    x = "Number of Women Selected in First 6 Choices",
    y = "% Selecting Woman for 7th Choice",
    title = "Treatment Effect by Initial Female Representation",
    subtitle = "Low Base Condition Only (3 authors per category)",
    color = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave("Figure-Interaction-Low-Base.pdf", plot = p_low, width = 10, height = 7, device = cairo_pdf)
cat("Saved: Figure-Interaction-Low-Base.pdf\n")

# Prepare data for plotting - MEDIUM CONDITION
plot_data_med <- d0_med %>%
  group_by(base_gender, gender_feedback) %>%
  summarise(
    n = n(),
    pct = mean(female_pick) * 100,
    se = sd(female_pick)/sqrt(n()) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    condition_label = ifelse(gender_feedback == 1, "Treatment (Gender Feedback)", "Control (No Gender Feedback)")
  )

# Create labels with N's for MEDIUM
n_labels_med <- d0_med %>%
  group_by(base_gender) %>%
  summarise(total_n = n()) %>%
  mutate(label = paste0(base_gender, "\n(n=", total_n, ")"))

# Plot for MEDIUM ONLY
p_med <- ggplot(plot_data_med, aes(x = base_gender, y = pct, color = condition_label, group = condition_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = pct - 1.96*se, ymax = pct + 1.96*se), width = 0.2, linewidth = 0.8) +
  scale_color_manual(values = c("Control (No Gender Feedback)" = "#011F5B",
                                 "Treatment (Gender Feedback)" = "#990000")) +
  scale_x_continuous(breaks = n_labels_med$base_gender,
                     labels = n_labels_med$label) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(
    x = "Number of Women Selected in First 6 Choices",
    y = "% Selecting Woman for 7th Choice",
    title = "Treatment Effect by Initial Female Representation",
    subtitle = "Medium Base Condition Only (6 authors per category)",
    color = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave("Figure-Interaction-Med-Base.pdf", plot = p_med, width = 10, height = 7, device = cairo_pdf)
cat("Saved: Figure-Interaction-Med-Base.pdf\n")

cat("\nDone!\n")
