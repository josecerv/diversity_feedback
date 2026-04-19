# Regenerate Figure-Study3B at a widescreen aspect ratio by replaying the
# exact ggplot2 code from Study-3B.Rmd against Study3B.csv — same look/feel,
# just with width = 14" (instead of 10") so it fills a 16:9 slide.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

here <- "C:/Users/jcerv/Jose/diversity_feedback/Study-3B"
setwd(here)

d0 <- read.csv("Study3B.csv")

# --- Panel-level summaries (identical to Study-3B.Rmd) --------------------
mk_panel <- function(df, feedback_col, pick_col) {
  df %>% dplyr::select(all_of(c(feedback_col, pick_col))) %>%
    group_by(.data[[feedback_col]]) %>%
    summarise(
      n = n(),
      freq = mean(.data[[pick_col]]),
      sd = sd(.data[[pick_col]]) * 100,
      se = (sd(.data[[pick_col]]) / sqrt(n())) * 100,
      .groups = "drop"
    ) %>%
    rename(Condition = !!feedback_col) %>%
    mutate(Condition = ifelse(Condition == 1, "\"Treatment\"", "\"Control\""))
}

dbudget_plot <- mk_panel(d0, "budget_shown", "budget_pick")
dyear_plot   <- mk_panel(d0, "year_shown",   "year_pick")
dpoli_plot   <- mk_panel(d0, "poli_shown",   "poli_pick")
dfemale_plot <- mk_panel(d0, "gender_feedback", "female_pick")

df_combined <- bind_rows(
  dbudget_plot %>% mutate(Category = "\nMade for a\nBudget of >$40M"),
  dyear_plot   %>% mutate(Category = "\nReleased After\n2010"),
  dpoli_plot   %>% mutate(Category = "\nAbout a\nPolitical leader"),
  dfemale_plot %>% mutate(Category = "\nAbout a \nWoman Protagonist"),
  .id = "id"
) %>%
  mutate(Category = factor(Category,
                            levels = c("\nMade for a\nBudget of >$40M",
                                       "\nReleased After\n2010",
                                       "\nAbout a\nPolitical leader",
                                       "\nAbout a \nWoman Protagonist")))

# --- Build plot identical to Rmd, with Calibri fallback to sans -----------
plot_font <- "sans"  # widely available; figure is about composition not font

p <- ggplot(df_combined, aes(x = Condition, y = freq * 100, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.85,
           position = position_dodge(width = 0.7)) +
  geom_text(aes(label = paste0(sprintf("%.1f", freq * 100), "%"),
                y = pmax(freq * 100 / 2, 4)),
            position = position_dodge(width = 0.7),
            size = 9, color = "white", fontface = "bold",
            family = plot_font) +
  geom_errorbar(aes(ymin = freq * 100 - se, ymax = freq * 100 + se),
                width = .1, position = position_dodge(width = 0.7)) +
  facet_wrap(~Category, nrow = 1, strip.position = "bottom") +
  geom_segment(data = df_combined %>% filter(Condition == "\"Treatment\""),
               aes(x = 1, xend = 2,
                   y = freq * 100 + se + 7, yend = freq * 100 + se + 7),
               inherit.aes = FALSE) +
  geom_text(data = df_combined %>% filter(Category %in%
                c("\nMade for a\nBudget of >$40M",
                  "\nReleased After\n2010",
                  "\nAbout a\nPolitical leader") &
                Condition == "\"Treatment\""),
            aes(x = 1.5, y = freq * 100 + se + 9, label = "n.s."),
            inherit.aes = FALSE, vjust = 0, size = 7, family = plot_font) +
  geom_text(data = df_combined %>% filter(Category ==
                "\nAbout a \nWoman Protagonist" &
                Condition == "\"Treatment\""),
            aes(x = 1.5, y = freq * 100 + se + 9, label = "***"),
            inherit.aes = FALSE, vjust = 0, size = 7, family = plot_font) +
  theme_bw() +
  scale_fill_manual(values = c("#011F5B", "#990000"),
                     labels = c("No feedback provided",
                                "Feedback provided"),
                     name = "Feedback") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                      limits = c(0, 75),
                      breaks = seq(0, 75, 25)) +
  scale_x_discrete(labels = c("Control" = "Not\nShown",
                               "Treatment" = "Shown")) +
  labs(x = "Feedback on % of films that were...",
       y = "% of Final Films Selected with the Target Attribute") +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "white"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(12, 12, 8, 12),
    axis.title.x = element_text(face = "bold", size = 18, margin = margin(t = 4),
                                 family = plot_font),
    plot.title = element_blank(),
    axis.title.y = element_text(size = 16, color = "black",
                                 margin = margin(r = 8), family = plot_font),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = 16, color = "black",
                                family = plot_font),
    strip.text = element_text(size = 18, color = "black",
                               family = plot_font),
    strip.background = element_rect(colour = "white", fill = "white")
  )

# --- Save at widescreen aspect so it fills a 13.33 x 7.5 inch slide ------
ggsave("Figure-Study3B-wide.png", plot = p,
       width = 14, height = 6.2, units = "in", dpi = 400)

cat("Saved Figure-Study3B-wide.png (14 x 6.2 in @ 400dpi)\n")
