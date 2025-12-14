# Figure S3: Study 4A and Study 4B combined (two-panel figure)
# Run this script from the Supplemental_Figures/scripts/ directory

library(ggplot2)
library(cowplot)
library(patchwork)
library(extrafont)

# Load fonts
loadfonts(device = "pdf", quiet = TRUE)

# Read in each ggplot object from data folder
# Study 4A on top (labeled A)
p_combined_4A <- readRDS("../data/p_combined_study4A.rds")
# Study 4B on bottom (labeled B)
p_combined_4B <- readRDS("../data/p_combined_study4B.rds")

# Add theme modification to adjust tag position, size, and make bold
# Consistent with manuscript figures: size 30, bold, Times New Roman
p_combined_4A <- p_combined_4A +
  labs(tag = "A") +
  theme(
    plot.tag.position = c(.15, .91),
    plot.tag = element_text(
      size = 30,
      face = "bold",
      family = "Times New Roman"
    )
  )

p_combined_4B <- p_combined_4B +
  labs(tag = "B") +
  theme(
    plot.tag.position = c(.15, .91),
    plot.tag = element_text(
      size = 30,
      face = "bold",
      family = "Times New Roman"
    )
  )

# 4A on top, 4B on bottom
final_plot <- p_combined_4A / p_combined_4B

# Save to PNG (Figure S3) - consistent with manuscript: 14x19, 600 dpi
ggsave("../Figure-S3.png", plot = final_plot, width = 14, height = 19, dpi = 600)

cat("Figure-S3.png saved successfully!\n")
