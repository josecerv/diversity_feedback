library(tidyverse)
library(extrafont)
library(sandwich)

# Load fonts
loadfonts(device = "win", quiet = TRUE)

# Read data
d0 <- read.csv('StudyS1.csv', check.names = FALSE)

# Create plot dataframes - using mean() like other studies
dgender_plot <- d0 |>
  select(gender_feedback, female) |>
  group_by(gender_feedback) |>
  summarise(
    n = n(),
    freq = mean(female),
    sd = sd(female) * 100,
    se = (sd(female) / sqrt(n())) * 100
  ) |>
  mutate(gender_feedback = case_when(gender_feedback==1 ~ '"Treatment"',
                          TRUE ~ '"Control"')) |>
  rename(Condition = gender_feedback)

dceo_plot <- d0 |>
  select(ceo, ceo_pick) |>
  group_by(ceo) |>
  summarise(
    n = n(),
    freq = mean(ceo_pick),
    sd = sd(ceo_pick) * 100,
    se = (sd(ceo_pick) / sqrt(n())) * 100
  ) |>
  mutate(ceo = case_when(ceo==1 ~ '"Treatment"',
                          TRUE ~ '"Control"')) |>
  rename(Condition = ceo)

dfounder_plot <- d0 |>
  select(founder, founder_pick) |>
  group_by(founder) |>
  summarise(
    n = n(),
    freq = mean(founder_pick),
    sd = sd(founder_pick) * 100,
    se = (sd(founder_pick) / sqrt(n())) * 100
  ) |>
  mutate(founder = case_when(founder==1 ~ '"Treatment"',
                          TRUE ~ '"Control"')) |>
  rename(Condition = founder)

dtech_plot <- d0 |>
  select(tech, tech_pick) |>
  group_by(tech) |>
  summarise(
    n = n(),
    freq = mean(tech_pick),
    sd = sd(tech_pick) * 100,
    se = (sd(tech_pick) / sqrt(n())) * 100
  ) |>
  mutate(tech = case_when(tech==1 ~ '"Treatment"',
                          TRUE ~ '"Control"')) |>
  rename(Condition = tech)

df_combined <- bind_rows(
  dceo_plot %>% mutate(Category = "\nCEOs"),
  dtech_plot %>% mutate(Category = "\nTechnologists"),
  dfounder_plot %>% mutate(Category = "\nFounders"),
  dgender_plot %>% mutate(Category = "\nFemale")
, .id = "id") %>%
  mutate(Category = factor(Category, levels = c('\nCEOs', '\nTechnologists', '\nFounders', '\nFemale')))

p_combined <- ggplot(df_combined, aes(x = Condition, y = freq*100, fill = Condition)) +
  geom_bar(stat="identity", width = 0.85, position = position_dodge(width = 0.7)) +
  geom_text(aes(label=paste0(sprintf("%.1f", freq*100),"%")),
            position=position_dodge(width=0.7), vjust=5, size = 5, color = "white", family = "Times New Roman") +
  geom_errorbar(aes(ymin=freq*100-se, ymax=freq*100+se), width = .1, position = position_dodge(width = 0.7)) +
  facet_wrap(~factor(Category, c('\nCEOs', '\nTechnologists', '\nFounders', '\nFemale')), nrow = 1, strip.position = "bottom") +
  geom_segment(data = df_combined %>% filter(Condition == '"Treatment"'),
               aes(x = 1, xend = 2, y = freq*100 + se + 5, yend = freq*100 + se + 5),
               inherit.aes = FALSE) +
  geom_text(data = df_combined %>% filter(Category %in% c('\nCEOs', '\nTechnologists') & Condition == '"Treatment"'),
            aes(x = 1.5, xend = 1.5, y = freq*100 + se + 7, yend = freq*100 + se + 7, label = "n.s."),
            inherit.aes = FALSE, vjust = 0, size = 5, family = "Times New Roman") +
  geom_text(data = df_combined %>% filter(Category %in% c('\nFounders') & Condition == '"Treatment"'),
            aes(x = 1.5, xend = 1.5, y = freq*100 + se + 5, yend = freq*100 + se + 5, label = "*"),
            inherit.aes = FALSE, vjust = 0, size = 5, family = "Times New Roman") +
  geom_text(data = df_combined %>% filter(Category == '\nFemale' & Condition == '"Treatment"'),
            aes(x = 1.5, xend = 1.5, y = freq*100 + se + 5, yend = freq*100 + se + 5, label = "***"),
            inherit.aes = FALSE, vjust = 0, size = 5, family = "Times New Roman") +
  theme_bw() +
  scale_fill_manual(values = c("#011F5B", "#990000"), labels = c("No feedback provided", "Feedback provided"), "Feedback") +
  scale_y_continuous(labels = function(x) paste0(x,"%"), limits = c(0,80)) +
  scale_x_discrete(labels = c('"Control"' = "Not\nShown", '"Treatment"' = "Shown")) +
  labs(x = "Feedback on % of panelists who were...", y = "% of New Panelists with the Target Identity",
       title = "The Effect of Getting Feedback on Your Panel's Composition") +
  theme(plot.caption = element_text(face = "italic", family = "Times New Roman"),
        legend.position = c(0.5, 0.85),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = 20, family = "Times New Roman"),
        legend.key.size = unit(7, 'mm'),
        legend.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill= NA, color = "white"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(face="bold", size = 22, vjust = 13, family = "Times New Roman"),
        plot.title = element_blank(),
        axis.title.y = element_text(size = 20, color = "black", family = "Times New Roman"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 20, color = "black", family = "Times New Roman"),
        strip.text = element_text(size = 20, color = "black", family = "Times New Roman"),
        strip.background = element_rect(colour = "white", fill = "white"))

ggsave("../Supplemental_Figures/Figure-S3.png", plot = p_combined, width = 10, height = 8, dpi = 600)
cat("Figure saved to Supplemental_Figures/Figure-S3.png\n")
