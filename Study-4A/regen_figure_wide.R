#!/usr/bin/env Rscript
# Regenerate Figure-Study4A at a wide aspect (14 x 6.2) so it reads nicely
# on a 16:9 slide. Mirrors the Study-3B/Study-5 regen pattern.
#
# Cell-mean percentage labels are centered vertically inside the bars and
# enlarged per Jose's feedback (2026-04-19): the numbers are the hero of
# the slide, not incidental.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

here <- "C:/Users/jcerv/Jose/diversity_feedback/Study-4A"
setwd(here)

d0 <- read.csv("Study4A.csv", check.names = FALSE)

mk_panel <- function(df, feedback_col, pick_col) {
  df %>% dplyr::select(all_of(c(feedback_col, pick_col))) %>%
    group_by(.data[[feedback_col]]) %>%
    summarise(
      n    = n(),
      freq = mean(.data[[pick_col]]),
      sd   = sd(.data[[pick_col]]) * 100,
      se   = (sd(.data[[pick_col]]) / sqrt(n())) * 100,
      .groups = "drop"
    ) %>%
    rename(Condition = !!feedback_col) %>%
    mutate(Condition = ifelse(Condition == 1, "\"Treatment\"", "\"Control\""))
}

dpoets_plot  <- mk_panel(d0, "poets",         "poets_pick")
dbooks_plot  <- mk_panel(d0, "books",         "book_pick")
doldies_plot <- mk_panel(d0, "oldies",        "oldies_pick")
drace_plot   <- mk_panel(d0, "race_feedback", "race_pick")

df_combined <- bind_rows(
  dpoets_plot  %>% mutate(Category = "\nWrote Poetry"),
  dbooks_plot  %>% mutate(Category = "\nWrote > 10 books"),
  doldies_plot %>% mutate(Category = "\nWere Born\nin the 1800s"),
  drace_plot   %>% mutate(Category = "\nWere Racial\nMinorities"),
  .id = "id"
) %>%
  mutate(Category = factor(Category,
                            levels = c("\nWrote Poetry",
                                       "\nWrote > 10 books",
                                       "\nWere Born\nin the 1800s",
                                       "\nWere Racial\nMinorities")))

plot_font <- "Times New Roman"  # matches the Study-4A.Rmd canonical theme

make_plot <- function(label_size) {
ggplot(df_combined, aes(x = Condition, y = freq * 100, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.85,
           position = position_dodge(width = 0.7)) +
  geom_text(aes(label = paste0(sprintf("%.1f", freq * 100), "%"),
                y = pmax(freq * 100 / 2, 4)),
            position = position_dodge(width = 0.7),
            size = label_size, color = "white", fontface = "bold",
            family = plot_font) +
  geom_errorbar(aes(ymin = freq * 100 - se, ymax = freq * 100 + se),
                width = .1, position = position_dodge(width = 0.7)) +
  facet_wrap(~Category, nrow = 1, strip.position = "bottom") +
  geom_segment(data = df_combined %>% filter(Condition == "\"Treatment\""),
               aes(x = 1, xend = 2,
                   y = freq * 100 + se + 5, yend = freq * 100 + se + 5),
               inherit.aes = FALSE) +
  geom_text(data = df_combined %>% filter(
              Category == "\nWrote > 10 books" & Condition == "\"Treatment\""),
            aes(x = 1.5, y = freq * 100 + se + 7, label = "n.s."),
            inherit.aes = FALSE, vjust = 0, size = 7, family = plot_font) +
  geom_text(data = df_combined %>% filter(
              Category %in% c("\nWrote Poetry", "\nWere Born\nin the 1800s") &
              Condition == "\"Treatment\""),
            aes(x = 1.5, y = freq * 100 + se + 7, label = "+"),
            inherit.aes = FALSE, vjust = 0, size = 7, family = plot_font) +
  geom_text(data = df_combined %>% filter(
              Category == "\nWere Racial\nMinorities" & Condition == "\"Treatment\""),
            aes(x = 1.5, y = freq * 100 + se + 7, label = "***"),
            inherit.aes = FALSE, vjust = 0, size = 7, family = plot_font) +
  theme_bw() +
  scale_fill_manual(values = c("#011F5B", "#990000"),
                    labels = c("No feedback provided", "Feedback provided"),
                    name = "Feedback") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 95)) +
  scale_x_discrete(labels = c("\"Control\"" = "Not\nShown",
                              "\"Treatment\"" = "Shown")) +
  labs(x = "Feedback on % of authors who...",
       y = "% of New Authors with the Target Attribute") +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "white"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(12, 12, 8, 12),
    axis.title.x = element_text(face = "bold", size = 18,
                                 margin = margin(t = 4), family = plot_font),
    plot.title = element_blank(),
    axis.title.y = element_text(size = 16, color = "black",
                                 margin = margin(r = 8), family = plot_font),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = 16, color = "black", family = plot_font),
    strip.text = element_text(size = 18, color = "black", family = plot_font),
    strip.background = element_rect(colour = "white", fill = "white")
  )
}

# Label size tuned per output aspect: wide has more horizontal room per bar
# so it can carry a larger label; narrow is tighter so size is dialed back.
p_wide   <- make_plot(label_size = 9)
p_narrow <- make_plot(label_size = 6)

ggsave("Figure-Study4A-wide.png", plot = p_wide,
       width = 14, height = 6.2, units = "in", dpi = 400)

# Also save at the Rmd's canonical 10 x 8 narrow aspect (matches the current
# embedded image in slide 10; keeps the slide's layout unchanged and just
# refreshes the cell-mean labels).
ggsave("Figure-Study4A.png", plot = p_narrow,
       width = 10, height = 8, units = "in", dpi = 600)

cat("Saved Figure-Study4A-wide.png (14 x 6.2 in @ 400dpi)\n")
cat("Saved Figure-Study4A.png (10 x 8 in @ 600dpi)\n")
