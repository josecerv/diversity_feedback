# Analysis: Balance Check and Treatment x Base_Gender Interaction
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

cat("===========================================================\n")
cat("QUESTION 1: Balance Check - HIGH Base Condition\n")
cat("===========================================================\n\n")

# Filter to high base condition
d0_high <- d0 %>% filter(base_condition == "high")

cat("Mean number of women selected in first 6 choices (HIGH base):\n")
balance_high <- d0_high %>%
  group_by(gender_feedback) %>%
  summarise(
    n = n(),
    mean = round(mean(base_gender), 3),
    sd = round(sd(base_gender), 3),
    median = median(base_gender),
    pct = round(mean(base_gender)/6 * 100, 2)
  )
print(balance_high)

cat("\n--- T-test for difference in base_gender by treatment (HIGH) ---\n")
t_test_high <- t.test(base_gender ~ gender_feedback, data = d0_high)
print(t_test_high)

cat("\n--- Regression: base_gender ~ gender_feedback (HIGH) ---\n")
balance_reg_high <- lm(base_gender ~ gender_feedback, data = d0_high)
print(robust_summary(balance_reg_high))
print(robust_confint(balance_reg_high))

cat("\n--- Distribution of base_gender by treatment (HIGH) ---\n")
dist_high <- d0_high %>%
  group_by(gender_feedback, base_gender) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(gender_feedback) %>%
  mutate(pct = round(n/sum(n) * 100, 1)) %>%
  arrange(gender_feedback, base_gender)
print(dist_high)

cat("\n\n===========================================================\n")
cat("QUESTION 2: Interaction Analysis - Treatment x Base_Gender Count\n")
cat("===========================================================\n\n")

# ALL BASE CONDITIONS
cat("=== ALL BASE CONDITIONS COMBINED ===\n\n")

cat("--- Mean 7th selection rate by base_gender count and treatment ---\n")
selection_by_count <- d0 %>%
  group_by(base_gender, gender_feedback) %>%
  summarise(
    n = n(),
    pct_select_woman = round(mean(female_pick) * 100, 1),
    se = round(sd(female_pick)/sqrt(n()) * 100, 2)
  ) %>%
  arrange(base_gender, gender_feedback)
print(selection_by_count)

cat("\n--- Interaction Model: female_pick ~ gender_feedback * base_gender (ALL) ---\n")
interaction_all <- lm(female_pick ~ gender_feedback * base_gender, data = d0)
print(robust_summary(interaction_all))
print(robust_confint(interaction_all))

# Interpretation
cat("\n--- INTERPRETATION (ALL CONDITIONS) ---\n")
coefs_all <- coef(interaction_all)
cat("When base_gender = 0 (no women initially):\n")
cat("  Control: ", round(coefs_all[1] * 100, 1), "%\n")
cat("  Treatment: ", round((coefs_all[1] + coefs_all[2]) * 100, 1), "%\n")
cat("  Treatment effect: ", round(coefs_all[2] * 100, 1), "pp\n\n")

cat("For each additional woman in base:\n")
cat("  Control changes by: ", round(coefs_all[3] * 100, 1), "pp\n")
cat("  Treatment changes by: ", round((coefs_all[3] + coefs_all[4]) * 100, 1), "pp\n")
cat("  Interaction (differential slope): ", round(coefs_all[4] * 100, 1), "pp\n")

# HIGH BASE CONDITION ONLY
cat("\n\n=== HIGH BASE CONDITION ONLY ===\n\n")

cat("--- Mean 7th selection rate by base_gender count and treatment (HIGH) ---\n")
selection_by_count_high <- d0_high %>%
  group_by(base_gender, gender_feedback) %>%
  summarise(
    n = n(),
    pct_select_woman = round(mean(female_pick) * 100, 1),
    se = round(sd(female_pick)/sqrt(n()) * 100, 2)
  ) %>%
  arrange(base_gender, gender_feedback)
print(selection_by_count_high)

cat("\n--- Interaction Model: female_pick ~ gender_feedback * base_gender (HIGH) ---\n")
interaction_high <- lm(female_pick ~ gender_feedback * base_gender, data = d0_high)
print(robust_summary(interaction_high))
print(robust_confint(interaction_high))

# Interpretation
cat("\n--- INTERPRETATION (HIGH BASE CONDITION) ---\n")
coefs_high <- coef(interaction_high)
cat("When base_gender = 0 (no women initially):\n")
cat("  Control: ", round(coefs_high[1] * 100, 1), "%\n")
cat("  Treatment: ", round((coefs_high[1] + coefs_high[2]) * 100, 1), "%\n")
cat("  Treatment effect: ", round(coefs_high[2] * 100, 1), "pp\n\n")

cat("For each additional woman in base:\n")
cat("  Control changes by: ", round(coefs_high[3] * 100, 1), "pp\n")
cat("  Treatment changes by: ", round((coefs_high[3] + coefs_high[4]) * 100, 1), "pp\n")
cat("  Interaction (differential slope): ", round(coefs_high[4] * 100, 1), "pp\n")

cat("\n\n===========================================================\n")
cat("CREATING VISUALIZATIONS\n")
cat("===========================================================\n\n")

# Prepare data for plotting - ALL CONDITIONS
plot_data_all <- d0 %>%
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

# Create labels with N's for ALL CONDITIONS
n_labels_all <- d0 %>%
  group_by(base_gender) %>%
  summarise(total_n = n()) %>%
  mutate(label = paste0(base_gender, "\n(n=", total_n, ")"))

# Plot for ALL CONDITIONS
p_all <- ggplot(plot_data_all, aes(x = base_gender, y = pct, color = condition_label, group = condition_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = pct - 1.96*se, ymax = pct + 1.96*se), width = 0.2, linewidth = 0.8) +
  scale_color_manual(values = c("Control (No Gender Feedback)" = "#011F5B",
                                 "Treatment (Gender Feedback)" = "#990000")) +
  scale_x_continuous(breaks = n_labels_all$base_gender,
                     labels = n_labels_all$label) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(
    x = "Number of Women Selected in First 6 Choices",
    y = "% Selecting Woman for 7th Choice",
    title = "Treatment Effect by Initial Female Representation",
    subtitle = "All Base Conditions Combined",
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

ggsave("Figure-Interaction-All-Bases.pdf", plot = p_all, width = 10, height = 7, device = cairo_pdf)
cat("Saved: Figure-Interaction-All-Bases.pdf\n")

# Prepare data for plotting - HIGH ONLY
plot_data_high <- d0_high %>%
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

# Create labels with N's for HIGH
n_labels_high <- d0_high %>%
  group_by(base_gender) %>%
  summarise(total_n = n()) %>%
  mutate(label = paste0(base_gender, "\n(n=", total_n, ")"))

# Plot for HIGH ONLY
p_high <- ggplot(plot_data_high, aes(x = base_gender, y = pct, color = condition_label, group = condition_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = pct - 1.96*se, ymax = pct + 1.96*se), width = 0.2, linewidth = 0.8) +
  scale_color_manual(values = c("Control (No Gender Feedback)" = "#011F5B",
                                 "Treatment (Gender Feedback)" = "#990000")) +
  scale_x_continuous(breaks = n_labels_high$base_gender,
                     labels = n_labels_high$label) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(
    x = "Number of Women Selected in First 6 Choices",
    y = "% Selecting Woman for 7th Choice",
    title = "Treatment Effect by Initial Female Representation",
    subtitle = "High Base Condition Only (9 authors per category)",
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

ggsave("Figure-Interaction-High-Base.pdf", plot = p_high, width = 10, height = 7, device = cairo_pdf)
cat("Saved: Figure-Interaction-High-Base.pdf\n")

cat("\nDone!\n")
