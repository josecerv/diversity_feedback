# Figure 4: Study 5 - Effect of Gender Feedback by Pool Condition
# Run this script from the Manuscript_Figures/scripts/ directory

library(ggplot2)
library(dplyr)
library(extrafont)

# Load fonts
loadfonts(device = "pdf", quiet = TRUE)

# Read the data
d0 <- read.csv('../data/Study5.csv', check.names = FALSE)

# Run statistical tests to determine significance levels
test_men <- t.test(female_pick ~ treatment, data = d0 %>% filter(men_pool == 1))
test_women <- t.test(female_pick ~ treatment, data = d0 %>% filter(men_pool == 0))

# Function to get significance stars
get_sig_stars <- function(p_value) {
  if (p_value < 0.001) return("***")
  else if (p_value < 0.01) return("**")
  else if (p_value < 0.05) return("*")
  else return("n.s.")
}

sig_men <- get_sig_stars(test_men$p.value)
sig_women <- get_sig_stars(test_women$p.value)

# Prepare plot data for men pool (women underrepresented, 25% women)
dmen_plot <- d0 %>%
  filter(men_pool == 1) %>%
  dplyr::select(treatment, female_pick) %>%
  group_by(treatment) %>%
  dplyr::summarise(
    n = n(),
    freq = mean(female_pick),
    sd = sd(female_pick) * 100,
    se = (sd(female_pick) / sqrt(n())) * 100
  ) %>%
  mutate(treatment = case_when(treatment == 1 ~ "Treatment",
                               TRUE ~ "Control")) %>%
  rename(Condition = treatment)

# Prepare plot data for women pool (women overrepresented, 75% women)
dwomen_plot <- d0 %>%
  filter(men_pool == 0) %>%
  dplyr::select(treatment, female_pick) %>%
  group_by(treatment) %>%
  dplyr::summarise(
    n = n(),
    freq = mean(female_pick),
    sd = sd(female_pick) * 100,
    se = (sd(female_pick) / sqrt(n())) * 100
  ) %>%
  mutate(treatment = case_when(treatment == 1 ~ "Treatment",
                               TRUE ~ "Control")) %>%
  rename(Condition = treatment)

# Combine into single dataframe
df_combined <- bind_rows(
  dmen_plot %>% mutate(Category = "Women Underrepresented\nin Candidate Set"),
  dwomen_plot %>% mutate(Category = "Women Overrepresented\nin Candidate Set"),
  .id = "id"
) %>%
  mutate(Category = factor(Category, levels = c('Women Underrepresented\nin Candidate Set',
                                                 'Women Overrepresented\nin Candidate Set')))

# Create combined plot
p_combined <- ggplot(df_combined, aes(x = Condition, y = freq * 100, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.85, position = position_dodge(width = 0.7)) +
  geom_text(aes(label = paste0(sprintf("%.1f", freq * 100), "%")),
            position = position_dodge(width = 0.7), vjust = 5, size = 7, color = "white") +
  geom_errorbar(aes(ymin = freq * 100 - se, ymax = freq * 100 + se),
                width = .1, position = position_dodge(width = 0.7)) +
  facet_wrap(~Category, nrow = 1, strip.position = "bottom") +
  geom_segment(data = df_combined %>% filter(Category == "Women Underrepresented\nin Candidate Set" & Condition == "Treatment"),
               aes(x = 1, xend = 2, y = freq * 100 + se + 10, yend = freq * 100 + se + 10),
               inherit.aes = FALSE) +
  geom_segment(data = df_combined %>% filter(Category == "Women Overrepresented\nin Candidate Set" & Condition == "Control"),
               aes(x = 1, xend = 2, y = freq * 100 + se + 10, yend = freq * 100 + se + 10),
               inherit.aes = FALSE) +
  geom_text(data = df_combined %>% filter(Category == "Women Underrepresented\nin Candidate Set" & Condition == "Treatment"),
            aes(x = 1.5, y = freq * 100 + se + 10, label = sig_men),
            inherit.aes = FALSE, vjust = 0, size = 7) +
  geom_text(data = df_combined %>% filter(Category == "Women Overrepresented\nin Candidate Set" & Condition == "Control"),
            aes(x = 1.5, y = freq * 100 + se + 10, label = sig_women),
            inherit.aes = FALSE, vjust = 0, size = 7) +
  theme_bw() +
  scale_fill_manual(values = c("#011F5B", "#990000"),
                    labels = c("No gender feedback provided", "Gender feedback provided"), "") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  scale_x_discrete(labels = c("Control" = "Not\nShown", "Treatment" = "Shown")) +
  labs(x = "", y = "% of Final Selections who Were Women") +
  theme(plot.caption = element_text(face = "italic"),
        legend.position = c(0.5, 0.95),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18),
        legend.key.size = unit(5, 'mm'),
        legend.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "white"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(size = 18, color = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 18, color = "black"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        strip.text = element_text(size = 18, color = "black"),
        strip.background = element_rect(colour = "white", fill = "white"))

# Save the plot (PNG only)
ggsave("../Figure-4.png", plot = p_combined, width = 10, height = 8, units = "in", dpi = 600)

cat("Figure-4.png saved successfully!\n")
