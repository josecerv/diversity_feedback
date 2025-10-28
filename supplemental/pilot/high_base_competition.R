# Analysis: Gender feedback competition in HIGH base condition only

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
    women_NOT_most_underrep = as.numeric(base_gender > min_count)
  )

# Filter to HIGH base condition only
d0_high <- d0 %>% filter(base_condition == "high")

cat("===========================================================\n")
cat("HIGH BASE CONDITION ONLY\n")
cat("===========================================================\n\n")

cat("Total N in HIGH base:", nrow(d0_high), "\n\n")

# How often are women most underrepresented in HIGH?
high_freq <- d0_high %>%
  summarise(
    n_women_most = sum(women_most_underrep_sole),
    pct_women_most = round(mean(women_most_underrep_sole) * 100, 1),
    n_women_not_most = sum(women_NOT_most_underrep),
    pct_women_not_most = round(mean(women_NOT_most_underrep) * 100, 1)
  )

cat("Women are THE MOST underrepresented:", high_freq$pct_women_most, "% (n=", high_freq$n_women_most, ")\n")
cat("Women are NOT the most underrepresented:", high_freq$pct_women_not_most, "% (n=", high_freq$n_women_not_most, ")\n\n")

cat("\n--- Average base_gender count by underrep status (HIGH base) ---\n")
avg_base_gender <- d0_high %>%
  mutate(
    status = case_when(
      women_most_underrep_sole == 1 ~ "Women Most Underrep",
      women_NOT_most_underrep == 1 ~ "Women NOT Most Underrep",
      TRUE ~ "Tied"
    )
  ) %>%
  filter(status != "Tied") %>%
  group_by(status) %>%
  summarise(
    n = n(),
    mean_base_gender = round(mean(base_gender), 2),
    median_base_gender = median(base_gender)
  )
print(avg_base_gender)

cat("\n\n===========================================================\n")
cat("TREATMENT EFFECTS IN HIGH BASE CONDITION\n")
cat("===========================================================\n\n")

comparison_high <- d0_high %>%
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
    se = round(sd(female_pick)/sqrt(n()) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(underrep_status, gender_feedback)

print(comparison_high)

# Calculate treatment effects
women_most_ctrl <- comparison_high %>%
  filter(underrep_status == "Women Most Underrep", gender_feedback == 0) %>%
  pull(pct_select_woman)
women_most_treat <- comparison_high %>%
  filter(underrep_status == "Women Most Underrep", gender_feedback == 1) %>%
  pull(pct_select_woman)
te_most <- women_most_treat - women_most_ctrl

women_not_ctrl <- comparison_high %>%
  filter(underrep_status == "Women NOT Most Underrep", gender_feedback == 0) %>%
  pull(pct_select_woman)
women_not_treat <- comparison_high %>%
  filter(underrep_status == "Women NOT Most Underrep", gender_feedback == 1) %>%
  pull(pct_select_woman)
te_not <- women_not_treat - women_not_ctrl

cat("\n--- TREATMENT EFFECTS ---\n\n")

cat("When women ARE most underrepresented:\n")
cat("  Control:", women_most_ctrl, "%\n")
cat("  Treatment:", women_most_treat, "%\n")
cat("  Treatment Effect: +", round(te_most, 1), " pp\n\n")

cat("When women are NOT most underrepresented:\n")
cat("  Control:", women_not_ctrl, "%\n")
cat("  Treatment:", women_not_treat, "%\n")
cat("  Treatment Effect:", round(te_not, 1), "pp\n\n")

cat("DIFFERENCE in treatment effects:", round(te_most - te_not, 1), "pp\n\n")

if(te_not < 0) {
  cat("*** NOTE: Treatment effect is NEGATIVE when women are NOT most underrepresented! ***\n")
  cat("This suggests that in HIGH base condition, when women are already relatively\n")
  cat("well-represented, gender feedback may actually BACKFIRE.\n\n")
}

cat("\n--- Statistical Tests ---\n\n")

# Among those with women most underrepresented
d0_high_most <- d0_high %>% filter(women_most_underrep_sole == 1)
cat("Regression: female_pick ~ gender_feedback (when women most underrep, HIGH base)\n")
r_most <- lm(female_pick ~ gender_feedback, data = d0_high_most)
print(robust_summary(r_most))

# Among those with women NOT most underrepresented
d0_high_not <- d0_high %>% filter(women_NOT_most_underrep == 1)
cat("\nRegression: female_pick ~ gender_feedback (when women NOT most underrep, HIGH base)\n")
r_not <- lm(female_pick ~ gender_feedback, data = d0_high_not)
print(robust_summary(r_not))

# Interaction test
d0_high_filtered <- d0_high %>% filter(women_most_underrep_sole == 1 | women_NOT_most_underrep == 1)
cat("\nInteraction: female_pick ~ gender_feedback * women_most_underrep (HIGH base)\n")
r_interact <- lm(female_pick ~ gender_feedback * women_most_underrep_sole, data = d0_high_filtered)
print(robust_summary(r_interact))

cat("\n\n===========================================================\n")
cat("VISUALIZATION\n")
cat("===========================================================\n\n")

# Create plot
plot_data_high <- comparison_high %>%
  mutate(
    treatment_label = ifelse(gender_feedback == 1, "Gender Feedback", "Control")
  )

p_high <- ggplot(plot_data_high, aes(x = underrep_status, y = pct_select_woman,
                                      fill = treatment_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = pct_select_woman - 1.96*se, ymax = pct_select_woman + 1.96*se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = paste0(pct_select_woman, "%\n(n=", n, ")")),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 4.5) +
  scale_fill_manual(values = c("Control" = "#011F5B", "Gender Feedback" = "#990000")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 60)) +
  scale_x_discrete(labels = c("Women Most Underrep" = "Women ARE\nMost Underrep",
                              "Women NOT Most Underrep" = "Women NOT\nMost Underrep")) +
  labs(
    x = "",
    y = "% Selecting Female Author for 7th Choice",
    title = "Gender Feedback in HIGH Base Condition",
    subtitle = "Treatment effect REVERSES when women are not most underrepresented",
    fill = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "red"),
    panel.grid.minor = element_blank()
  )

ggsave("Figure-High-Base-Competition.pdf", plot = p_high,
       width = 10, height = 7, device = cairo_pdf)
cat("Saved: Figure-High-Base-Competition.pdf\n")

cat("\nDone!\n")
