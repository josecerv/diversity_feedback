library(ggplot2)
library(cowplot)

# Read in each ggplot object
p_combined_A <- readRDS("./Study-1A/p_combined_A.rds")
p_combined_B <- readRDS("./Study-1B/p_combined_B.rds")

# Add theme modification to adjust tag position, size, and make bold
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

p_combined_B <- p_combined_B + 
  labs(tag = "B") +
  theme(
    plot.tag.position = c(.15, .91),
    plot.tag = element_text(
      size = 30, 
      face = "bold",
      family = "Times New Roman"
    )
  )

final_plot <- p_combined_A / p_combined_B

# Save to PDF
ggsave("./Manuscript_Figures/Figure-2.pdf", family = "Times New Romain", plot = final_plot, device=cairo_pdf, width = 14, height = 19)
