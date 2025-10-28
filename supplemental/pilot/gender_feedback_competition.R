# Analysis: How does underrepresentation competition affect gender feedback?

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

# Identify which attribute is most underrepresented
d0 <- d0 %>%
  mutate(
    min_count = pmin(base_gender, base_poets, base_oldies, base_books),
    num_at_min = (base_gender == min_count) +
                 (base_poets == min_count) +
                 (base_oldies == min_count) +
                 (base_books == min_count),
    women_most_underrep_sole = as.numeric(base_gender == min_count & num_at_min == 1),
    women_most_underrep_any = as.numeric(base_gender == min_count),
    women_NOT_most_underrep = as.numeric(base_gender > min_count)
  )

cat("===========================================================\n")
cat("OVERALL: How often are women the most underrepresented?\n")
cat("===========================================================\n\n")

overall_stats <- d0 %>%
  summarise(
    total_n = n(),

    # Women are THE MOST (sole minimum)
    n_women_sole_most = sum(women_most_underrep_sole),
    pct_women_sole_most = round(mean(women_most_underrep_sole) * 100, 1),

    # Women are tied for most (includes ties)
    n_women_any_most = sum(women_most_underrep_any),
    pct_women_any_most = round(mean(women_most_underrep_any) * 100, 1),

    # Women are NOT the most
    n_women_not_most = sum(women_NOT_most_underrep),
    pct_women_not_most = round(mean(women_NOT_most_underrep) * 100, 1)
  )

cat("Total N:", overall_stats$total_n, "\n\n")
cat("Women are THE MOST underrepresented (sole minimum):\n")
cat("  N =", overall_stats$n_women_sole_most, "\n")
cat("  % =", overall_stats$pct_women_sole_most, "%\n\n")

cat("Women are AT LEAST TIED for most underrepresented:\n")
cat("  N =", overall_stats$n_women_any_most, "\n")
cat("  % =", overall_stats$pct_women_any_most, "%\n\n")

cat("Women are NOT the most underrepresented:\n")
cat("  N =", overall_stats$n_women_not_most, "\n")
cat("  % =", overall_stats$pct_women_not_most, "%\n\n")

cat("\n===========================================================\n")
cat("KEY QUESTION: Gender Feedback Effectiveness by Underrepresentation Status\n")
cat("===========================================================\n\n")

cat("Among those who received GENDER FEEDBACK (treatment):\n\n")

# Filter to treatment group only
d0_treatment <- d0 %>% filter(gender_feedback == 1)

cat("--- When women ARE the most underrepresented (sole) ---\n")
treatment_women_most <- d0_treatment %>%
  filter(women_most_underrep_sole == 1) %>%
  summarise(
    n = n(),
    pct_select_woman = round(mean(female_pick) * 100, 1)
  )
cat("N =", treatment_women_most$n, "\n")
cat("% selecting woman =", treatment_women_most$pct_select_woman, "%\n\n")

cat("--- When women are NOT the most underrepresented ---\n")
treatment_women_not_most <- d0_treatment %>%
  filter(women_NOT_most_underrep == 1) %>%
  summarise(
    n = n(),
    pct_select_woman = round(mean(female_pick) * 100, 1)
  )
cat("N =", treatment_women_not_most$n, "\n")
cat("% selecting woman =", treatment_women_not_most$pct_select_woman, "%\n\n")

cat("--- DIFFERENCE (penalty for competition) ---\n")
penalty <- treatment_women_most$pct_select_woman - treatment_women_not_most$pct_select_woman
cat("Difference =", penalty, "percentage points\n\n")

cat("\n--- Statistical Test ---\n")
r_competition <- lm(female_pick ~ women_most_underrep_sole, data = d0_treatment)
print(robust_summary(r_competition))

cat("\n\n===========================================================\n")
cat("COMPARISON: Control vs Treatment by Underrepresentation Status\n")
cat("===========================================================\n\n")

comparison <- d0 %>%
  mutate(
    underrep_status = case_when(
      women_most_underrep_sole == 1 ~ "Women Most Underrep",
      women_NOT_most_underrep == 1 ~ "Women NOT Most Underrep",
      TRUE ~ "Tied"
    )
  ) %>%
  filter(underrep_status != "Tied") %>%
  group_by(underrep_status, gender_feedback) %>%
  summarise(
    n = n(),
    pct_select_woman = round(mean(female_pick) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(underrep_status, gender_feedback)

print(comparison)

cat("\n--- Treatment Effects ---\n")
# When women are most underrepresented
women_most_ctrl <- comparison %>% filter(underrep_status == "Women Most Underrep", gender_feedback == 0) %>% pull(pct_select_woman)
women_most_treat <- comparison %>% filter(underrep_status == "Women Most Underrep", gender_feedback == 1) %>% pull(pct_select_woman)
te_most <- women_most_treat - women_most_ctrl

# When women are NOT most underrepresented
women_not_ctrl <- comparison %>% filter(underrep_status == "Women NOT Most Underrep", gender_feedback == 0) %>% pull(pct_select_woman)
women_not_treat <- comparison %>% filter(underrep_status == "Women NOT Most Underrep", gender_feedback == 1) %>% pull(pct_select_woman)
te_not <- women_not_treat - women_not_ctrl

cat("\nWhen women ARE most underrepresented:\n")
cat("  Control:", women_most_ctrl, "%\n")
cat("  Treatment:", women_most_treat, "%\n")
cat("  Treatment Effect:", te_most, "pp\n\n")

cat("When women are NOT most underrepresented:\n")
cat("  Control:", women_not_ctrl, "%\n")
cat("  Treatment:", women_not_treat, "%\n")
cat("  Treatment Effect:", te_not, "pp\n\n")

cat("DIFFERENCE in treatment effects:", te_most - te_not, "pp\n")

cat("\n--- Interaction Test ---\n")
d0_filtered <- d0 %>% filter(women_most_underrep_sole == 1 | women_NOT_most_underrep == 1)
r_interact <- lm(female_pick ~ gender_feedback * women_most_underrep_sole, data = d0_filtered)
print(robust_summary(r_interact))

cat("\n\n===========================================================\n")
cat("BY BASE CONDITION\n")
cat("===========================================================\n\n")

comparison_by_base <- d0 %>%
  mutate(
    underrep_status = case_when(
      women_most_underrep_sole == 1 ~ "Women Most",
      women_NOT_most_underrep == 1 ~ "Women NOT Most",
      TRUE ~ "Tied"
    )
  ) %>%
  filter(underrep_status != "Tied") %>%
  group_by(base_condition, underrep_status, gender_feedback) %>%
  summarise(
    n = n(),
    pct = round(mean(female_pick) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(base_condition, underrep_status, gender_feedback)

print(comparison_by_base)

cat("\n\n===========================================================\n")
cat("VISUALIZATION\n")
cat("===========================================================\n\n")

# Create summary data for plotting
plot_data <- d0 %>%
  mutate(
    underrep_status = case_when(
      women_most_underrep_sole == 1 ~ "Women ARE\nMost Underrep",
      women_NOT_most_underrep == 1 ~ "Women NOT\nMost Underrep",
      TRUE ~ "Tied"
    ),
    treatment_label = ifelse(gender_feedback == 1, "Gender Feedback", "Control")
  ) %>%
  filter(underrep_status != "Tied") %>%
  group_by(underrep_status, treatment_label) %>%
  summarise(
    n = n(),
    pct = mean(female_pick) * 100,
    se = sd(female_pick)/sqrt(n()) * 100,
    .groups = "drop"
  )

p <- ggplot(plot_data, aes(x = underrep_status, y = pct, fill = treatment_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = pct - 1.96*se, ymax = pct + 1.96*se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = paste0(round(pct, 1), "%\n(n=", n, ")")),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 4.5) +
  scale_fill_manual(values = c("Control" = "#011F5B", "Gender Feedback" = "#990000")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 60)) +
  labs(
    x = "",
    y = "% Selecting Female Author for 7th Choice",
    title = "Gender Feedback Effectiveness:\nDoes Underrepresentation Competition Matter?",
    subtitle = "Among participants who received gender feedback",
    fill = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave("Figure-Gender-Feedback-Competition.pdf", plot = p,
       width = 10, height = 7, device = cairo_pdf)
cat("Saved: Figure-Gender-Feedback-Competition.pdf\n")

cat("\nDone!\n")
