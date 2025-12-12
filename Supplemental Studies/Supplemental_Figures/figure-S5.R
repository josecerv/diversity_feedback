library(ggplot2)
library(cowplot)
library(patchwork)
library(extrafont)

# Load fonts
loadfonts(device = "win", quiet = TRUE)
loadfonts(device = "pdf", quiet = TRUE)

# Read in each ggplot object
# Panel A (no encouragement)
p_combined_A <- readRDS("../S2A/p_combined_S2A_A.rds")
# Panel B (with encouragement)
p_combined_B <- readRDS("../S2A/p_combined_S2A_B.rds")

# Add theme modification to adjust tag position, size, and make bold
# Panel A keeps its legend at the top
p_combined_A <- p_combined_A +
  labs(tag = "A") +
  theme(
    plot.tag.position = c(.15, .91),
    plot.tag = element_text(
      size = 30,
      face = "bold",
      family = "Times New Roman"
    )
  )

# Panel B - add legend back (same style as Panel A)
p_combined_B <- p_combined_B +
  labs(tag = "B") +
  theme(
    plot.tag.position = c(.15, .91),
    plot.tag = element_text(
      size = 30,
      face = "bold",
      family = "Times New Roman"
    ),
    legend.position = c(0.5, 0.95),
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.text = element_text(size = 14, family = "Times New Roman"),
    legend.key.size = unit(7, 'mm'),
    legend.background = element_rect(fill = "white")
  )

# A on top, B on bottom (vertical stacking like Figure 6)
final_plot <- p_combined_A / p_combined_B

# Save to PDF (Figure S5)
ggsave("Figure-S5.pdf", family = "Times New Roman", plot = final_plot,
       device = cairo_pdf, width = 14, height = 19)
ggsave("Figure-S5.png", plot = final_plot, width = 14, height = 19, dpi = 600)
