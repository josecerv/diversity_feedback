#!/usr/bin/env Rscript
# Regenerate Figure-Study5 at a wide aspect (14 x 6.2) so it reads nicely
# on a 16:9 slide without horizontal stretching — mirrors what we did for
# Study 1's Figure-Study3A.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

here <- "C:/Users/jcerv/Jose/diversity_feedback/Study-5"
setwd(here)

d0 <- read.csv("Study5.csv", check.names = FALSE)
d0$women_overrep <- 1 - d0$men_pool

# significance tests
test_men   <- t.test(female_pick ~ treatment, data = d0 %>% filter(men_pool == 1))
test_women <- t.test(female_pick ~ treatment, data = d0 %>% filter(men_pool == 0))

sig_stars <- function(p) {
  if (is.na(p)) return("n.s.")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  "n.s."
}
sig_men   <- sig_stars(test_men$p.value)
sig_women <- sig_stars(test_women$p.value)

dmen_plot <- d0 %>%
  filter(men_pool == 1) %>%
  group_by(treatment) %>%
  summarise(n = n(), freq = mean(female_pick),
            se  = sd(female_pick) / sqrt(n()) * 100, .groups = "drop") %>%
  mutate(Condition = ifelse(treatment == 1, "Treatment", "Control"))

dwomen_plot <- d0 %>%
  filter(men_pool == 0) %>%
  group_by(treatment) %>%
  summarise(n = n(), freq = mean(female_pick),
            se  = sd(female_pick) / sqrt(n()) * 100, .groups = "drop") %>%
  mutate(Condition = ifelse(treatment == 1, "Treatment", "Control"))

df <- bind_rows(
  dmen_plot   %>% mutate(Category = "Women Underrepresented\nin Candidate Set"),
  dwomen_plot %>% mutate(Category = "Women Overrepresented\nin Candidate Set")
) %>%
  mutate(Category = factor(Category,
                            levels = c("Women Underrepresented\nin Candidate Set",
                                       "Women Overrepresented\nin Candidate Set")))

p <- ggplot(df, aes(x = Condition, y = freq * 100, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.85,
           position = position_dodge(width = 0.7)) +
  geom_text(aes(label = paste0(sprintf("%.1f", freq * 100), "%"),
                y = pmax(freq * 100 / 2, 4)),
            position = position_dodge(width = 0.7),
            size = 10, color = "white", fontface = "bold") +
  geom_errorbar(aes(ymin = freq * 100 - se, ymax = freq * 100 + se),
                width = 0.1, position = position_dodge(width = 0.7)) +
  facet_wrap(~ Category, nrow = 1, strip.position = "bottom") +
  geom_segment(data = df %>% filter(
                 Category == "Women Underrepresented\nin Candidate Set" &
                 Condition == "Treatment"),
               aes(x = 1, xend = 2,
                   y = freq * 100 + se + 10,
                   yend = freq * 100 + se + 10),
               inherit.aes = FALSE) +
  geom_segment(data = df %>% filter(
                 Category == "Women Overrepresented\nin Candidate Set" &
                 Condition == "Control"),
               aes(x = 1, xend = 2,
                   y = freq * 100 + se + 10,
                   yend = freq * 100 + se + 10),
               inherit.aes = FALSE) +
  geom_text(data = df %>% filter(
              Category == "Women Underrepresented\nin Candidate Set" &
              Condition == "Treatment"),
            aes(x = 1.5, y = freq * 100 + se + 11, label = sig_men),
            inherit.aes = FALSE, vjust = 0, size = 10) +
  geom_text(data = df %>% filter(
              Category == "Women Overrepresented\nin Candidate Set" &
              Condition == "Control"),
            aes(x = 1.5, y = freq * 100 + se + 11, label = sig_women),
            inherit.aes = FALSE, vjust = 0, size = 10) +
  theme_bw() +
  scale_fill_manual(values = c("#011F5B", "#990000"),
                    labels = c("No gender feedback provided",
                               "Gender feedback provided"),
                    name = "") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100)) +
  scale_x_discrete(labels = c("Control" = "Not\nShown",
                              "Treatment" = "Shown")) +
  labs(x = "", y = "% of Final Selections who Were Women") +
  theme(legend.position = c(0.5, 0.97),
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.key.size = unit(6, "mm"),
        legend.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "white"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(size = 20, color = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 20, color = "black"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        strip.text = element_text(size = 22, color = "black"),
        strip.background = element_rect(colour = "white", fill = "white"))

ggsave("Figure-Study5-wide.png", plot = p,
       width = 14, height = 6.2, units = "in", dpi = 400)

cat("saved Figure-Study5-wide.png\n")
