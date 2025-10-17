# Visualization by Set Assignment
# This creates a combined figure showing treatment effects separately for Set 1 and Set 2

library(dplyr)
library(ggplot2)
library(tidyverse)

# Read the data
d0 <- read.csv('Study5.csv', check.names = F)

# Function to convert p-value to significance stars
get_sig_stars <- function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("n.s.")
}

# Calculate descriptive statistics by set and treatment for women
dwomen_by_set <- d0 |>
  dplyr::select(women_feedback, women_proportion, set_num) |>
  dplyr::group_by(set_num, women_feedback) |>
  dplyr::summarise(
    n = n(),
    freq = mean(women_proportion),
    sd = sd(women_proportion) * 100,
    se = (sd(women_proportion) / sqrt(n())) * 100,
    .groups = 'drop'
  ) |>
  dplyr::mutate(
    Condition = case_when(
      women_feedback == 1 ~ "Treatment",
      TRUE ~ "Control"
    ),
    Set = paste0("Set ", set_num)
  )

# Calculate p-values for each set
library(lmtest)
library(sandwich)

robust_summary <- function(model) {
    robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
    model_summary <- summary(model)
    model_summary$coefficients[, "Std. Error"] <- robust_se
    model_summary$coefficients[, "t value"] <- model_summary$coefficients[, "Estimate"] / robust_se
    model_summary$coefficients[, "Pr(>|t|)"] <- 2 * pt(-abs(model_summary$coefficients[, "t value"]),
                                                        df = model_summary$df[2], lower.tail = TRUE)
    return(model_summary)
}

# Test treatment effect within each set
d0_set1 <- d0 |> filter(set_num == 1)
d0_set2 <- d0 |> filter(set_num == 2)

r_set1 <- lm(women_proportion ~ women_feedback, data = d0_set1)
r_set2 <- lm(women_proportion ~ women_feedback, data = d0_set2)

p_set1 <- robust_summary(r_set1)$coefficients["women_feedback", "Pr(>|t|)"]
p_set2 <- robust_summary(r_set2)$coefficients["women_feedback", "Pr(>|t|)"]

sig_set1 <- get_sig_stars(p_set1)
sig_set2 <- get_sig_stars(p_set2)

# Add significance labels to the data
dwomen_by_set <- dwomen_by_set |>
  mutate(sig_label = case_when(
    set_num == 1 ~ sig_set1,
    set_num == 2 ~ sig_set2
  ))

# Create the plot
p_by_set <- ggplot(dwomen_by_set, aes(x = Condition, y = freq*100, fill = Condition)) +
  geom_bar(stat="identity", width = 0.7, position = position_dodge(width = 0.7)) +
  geom_text(aes(label=paste0(sprintf("%.1f", freq*100),"%")),
            position=position_dodge(width=0.7), vjust=5, size = 5, color = "white") +
  geom_errorbar(aes(ymin=freq*100-se, ymax=freq*100+se), width = .15,
                position = position_dodge(width = 0.7)) +
  facet_wrap(~Set, nrow = 1) +
  geom_segment(data = dwomen_by_set %>% filter(Condition == "Treatment"),
               aes(x = 1, xend = 2, y = freq*100 + se + 5, yend = freq*100 + se + 5),
               inherit.aes = FALSE) +
  geom_text(data = dwomen_by_set %>% filter(Condition == "Treatment"),
            aes(x = 1.5, y = freq*100 + se + 7, label = sig_label),
            inherit.aes = FALSE, vjust = 0, size = 6) +
  theme_bw() +
  scale_fill_manual(values = c("Control" = "#990000", "Treatment" = "#011F5B"),
                    labels = c("No women feedback", "Women feedback provided")) +
  scale_y_continuous(labels = function(x) paste0(x,"%"), limits = c(0,100)) +
  labs(x = "",
       y = "% of Women Selected (out of 3 choices)",
       title = "Treatment Effect on Women Selection by Set Assignment",
       subtitle = "Set 1: Women feedback shows 10% | Set 2: Women feedback shows 20%",
       fill = "") +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.key.size = unit(7, 'mm'),
        legend.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_rect(fill = "white", color = "black"))

print(p_by_set)

# Save the plot
ggsave("Figure-Study5-BySet.pdf", plot = p_by_set, width = 10, height = 7,
       units = "in", device = cairo_pdf, family = "Times New Roman")

# Print the statistics
cat("\n=== Treatment Effect Statistics by Set ===\n\n")
cat("Set 1 (Women feedback shows 10%):\n")
print(robust_summary(r_set1))
cat("\nSet 2 (Women feedback shows 20%):\n")
print(robust_summary(r_set2))

# Calculate effect sizes
effect_set1 <- coef(r_set1)["women_feedback"] * 100
effect_set2 <- coef(r_set2)["women_feedback"] * 100

cat("\n=== Effect Sizes ===\n")
cat(sprintf("Set 1 effect: %.2f percentage points (p = %.4f) %s\n",
            effect_set1, p_set1, sig_set1))
cat(sprintf("Set 2 effect: %.2f percentage points (p = %.4f) %s\n",
            effect_set2, p_set2, sig_set2))
cat(sprintf("Difference: %.2f percentage points\n", effect_set2 - effect_set1))

# Test if the effects are significantly different (interaction)
r_interaction <- lm(women_proportion ~ women_feedback * set_num, data = d0)
p_interaction <- robust_summary(r_interaction)$coefficients["women_feedback:set_num", "Pr(>|t|)"]
cat(sprintf("\nInteraction test (are effects different?): p = %.4f\n", p_interaction))

# Descriptive statistics table
cat("\n=== Descriptive Statistics ===\n")
print(dwomen_by_set %>% select(Set, Condition, n, freq, se))
