# Figure 3: Study 1 and Study 2 combined (two-panel figure)
# Run this script from the Manuscript_Figures/scripts/ directory

library(ggplot2)
library(cowplot)
library(patchwork)
library(extrafont)

# Load fonts
loadfonts(device = "pdf", quiet = TRUE)

# Read in each ggplot object from data folder
# Study 1 on top (labeled A)
p_combined_1 <- readRDS("../data/p_combined_study1.rds")
# Study 2 on bottom (labeled B)
p_combined_2 <- readRDS("../data/p_combined_study2.rds")

# Add theme modification to adjust tag position, size, and make bold
p_combined_1 <- p_combined_1 +
  labs(tag = "A") +
  theme(
    plot.tag.position = c(.15, .91),
    plot.tag = element_text(
      size = 30,
      face = "bold",
      family = "Times New Roman"
    )
  )

p_combined_2 <- p_combined_2 +
  labs(tag = "B") +
  theme(
    plot.tag.position = c(.15, .91),
    plot.tag = element_text(
      size = 30,
      face = "bold",
      family = "Times New Roman"
    )
  )

# Study 1 on top, Study 2 on bottom
final_plot <- p_combined_1 / p_combined_2

# Save to PNG (Figure 3)
ggsave("../Figure-3.png", plot = final_plot, width = 14, height = 19, dpi = 600)

cat("Figure-3.png saved successfully!\n")
