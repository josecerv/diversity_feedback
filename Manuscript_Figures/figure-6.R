library(ggplot2)
library(cowplot)
library(patchwork)

# Read in each ggplot object
# Study 4A on top (labeled A)
p_combined_4A <- readRDS("./Study-4A/p_combined_study4A.rds")
# Study 4B on bottom (labeled B)
p_combined_4B <- readRDS("./Study-4B/p_combined_study4B.rds")

# Add theme modification to adjust tag position, size, and make bold
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

# Save to PDF (Figure 6)
ggsave("./Manuscript_Figures/Figure-6.pdf", family = "Times New Roman", plot = final_plot, device=cairo_pdf, width = 14, height = 19)
ggsave("./Manuscript_Figures/Figure-6.png", plot = final_plot, width = 14, height = 19, dpi = 600)
