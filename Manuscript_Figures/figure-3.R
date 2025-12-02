library(ggplot2)
library(cowplot)
library(patchwork)

# Read in each ggplot object
# Study 3A on top (labeled A)
p_combined_3A <- readRDS("./Study-3A/p_combined_B.rds")
# Study 3B on bottom (labeled B)
p_combined_3B <- readRDS("./Study-3B/p_combined_A.rds")

# Add theme modification to adjust tag position, size, and make bold
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

# Save to PDF (now Figure 3)
ggsave("./Manuscript_Figures/Figure-3.pdf", family = "Times New Roman", plot = final_plot, device=cairo_pdf, width = 14, height = 19)
ggsave("./Manuscript_Figures/Figure-3.png", plot = final_plot, width = 14, height = 19, dpi = 600)
