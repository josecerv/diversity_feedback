library(ggplot2)
library(cowplot)
library(patchwork)

# Read in each ggplot object
# Study 1 on top (labeled A)
p_combined_1 <- readRDS("./Study-1/p_combined_study1.rds")
# Study 2 on bottom (labeled B)
p_combined_2 <- readRDS("./Study-2/p_combined_study2.rds")

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

# Save to PDF (Figure 4)
ggsave("./Manuscript_Figures/Figure-4.pdf", family = "Times New Roman", plot = final_plot, device=cairo_pdf, width = 14, height = 19)
ggsave("./Manuscript_Figures/Figure-4.png", plot = final_plot, width = 14, height = 19, dpi = 600)
