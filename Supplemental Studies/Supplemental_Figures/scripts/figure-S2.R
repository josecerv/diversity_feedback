# Figure S2: Study 3A and Study 3B combined (two-panel figure)
# Run this script from the Supplemental_Figures/scripts/ directory

library(ggplot2)
library(cowplot)
library(patchwork)
library(extrafont)

# Load fonts
loadfonts(device = "pdf", quiet = TRUE)

# Read in each ggplot object from data folder
# Study 3A on top (labeled A)
p_combined_3A <- readRDS("../data/p_combined_study3A.rds")
# Study 3B on bottom (labeled B)
p_combined_3B <- readRDS("../data/p_combined_study3B.rds")

# Add theme modification to adjust tag position, size, and make bold
# Consistent with manuscript figures: size 30, bold, Times New Roman
p_combined_3A <- p_combined_3A +
  labs(tag = "A") +
  theme(
    plot.tag.position = c(.15, .91),
    plot.tag = element_text(
      size = 30,
      face = "bold",
      family = "Times New Roman"
    )
  )

p_combined_3B <- p_combined_3B +
  labs(tag = "B") +
  theme(
    plot.tag.position = c(.15, .91),
    plot.tag = element_text(
      size = 30,
      face = "bold",
      family = "Times New Roman"
    )
  )

# 3A on top, 3B on bottom
final_plot <- p_combined_3A / p_combined_3B

# Save to PNG (Figure S2) - consistent with manuscript: 14x19, 600 dpi
ggsave("../Figure-S2.png", plot = final_plot, width = 14, height = 19, dpi = 600)

cat("Figure-S2.png saved successfully!\n")
