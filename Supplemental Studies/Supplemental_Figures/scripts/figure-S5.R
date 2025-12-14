# Figure S5: Study S2A - Two cells combined (two-panel figure)
# Run this script from the Supplemental_Figures/scripts/ directory

library(ggplot2)
library(cowplot)
library(patchwork)
library(extrafont)

# Load fonts
loadfonts(device = "pdf", quiet = TRUE)

# Read in each ggplot object from data folder
# Panel A (no encouragement)
p_combined_A <- readRDS("../data/p_combined_S2A_A.rds")
# Panel B (with encouragement)
p_combined_B <- readRDS("../data/p_combined_S2A_B.rds")

# Add theme modification to adjust tag position, size, and make bold
# Consistent with manuscript figures: size 30, bold, Times New Roman
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

# A on top, B on bottom (vertical stacking)
final_plot <- p_combined_A / p_combined_B

# Save to PNG (Figure S5) - consistent with manuscript: 14x19, 600 dpi
ggsave("../Figure-S5.png", plot = final_plot, width = 14, height = 19, dpi = 600)

cat("Figure-S5.png saved successfully!\n")
