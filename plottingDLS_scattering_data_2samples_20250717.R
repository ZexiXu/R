# ZX
# 2025-07-17
# Martinsried
# Change filename and sample names

library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)  # for unit()

# Load data
file_path <- "filename.xlsx"
dls_data <- read_excel(file_path)
colnames(dls_data)[1] <- "Size"

# Common theme settings with extra vertical spacing in legend
common_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.2, 'cm'),
    legend.spacing.y = unit(0.05, 'cm'),  # extra space between legend items
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  )

### Figure 1: Columns 2-4 ###
data_1 <- dls_data %>%
  select(Size, 2:4) %>%
  pivot_longer(-Size, names_to = "Sample", values_to = "Intensity")

fig1 <- ggplot(data_1, aes(x = Size, y = Intensity, color = Sample)) +
  geom_line(size = 1) +
  scale_x_log10() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(-0.1, NA)) +
  labs(
    x = "Size (d.nm)",
    y = "Intensity (a.u.)",
    title = "sample1_name"
  ) +
  common_theme +
  guides(color = guide_legend(ncol = 1))  # One sample per row (1 column)

### Figure 2: Columns 5-7 ###
data_2 <- dls_data %>%
  select(Size, 5:7) %>%
  pivot_longer(-Size, names_to = "Sample", values_to = "Intensity")

fig2 <- ggplot(data_2, aes(x = Size, y = Intensity, color = Sample)) +
  geom_line(size = 1) +
  scale_x_log10() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(-0.1, NA)) +
  labs(
    x = "Size (d.nm)",
    y = "Intensity (a.u.)",
    title = expression("sampel2_name")
  ) +
  common_theme +
  guides(color = guide_legend(ncol = 1))  # One sample per row (1 column)

### Figure 3: All samples; columns 5-7 dashed ###
data_3 <- dls_data %>%
  pivot_longer(-Size, names_to = "Sample", values_to = "Intensity") %>%
  mutate(LineType = ifelse(Sample %in% colnames(dls_data)[5:7], "dashed", "solid"))

fig3 <- ggplot(data_3, aes(x = Size, y = Intensity, color = Sample, linetype = LineType)) +
  geom_line(size = 1) +
  scale_x_log10() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(-0.1, NA)) +
  scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed")) +
  labs(
    x = "Size (d.nm)",
    y = "Intensity (a.u.)",
    title = "DLS"
  ) +
  common_theme +
  guides(
    color = guide_legend(ncol = 1),       # One sample per row
    linetype = guide_legend(ncol = 1)     # One line type per row
  )

### Display plots ###
print(fig1)
print(fig2)
print(fig3)

# Define output folder (same as Excel file location)
output_folder <- dirname(file_path)

# Save as PDF
ggsave(file.path(output_folder, "sample1.pdf"), fig1, width = 8, height = 7)
ggsave(file.path(output_folder, "sample2.pdf"), fig2, width = 8, height = 7)
ggsave(file.path(output_folder, "merged.pdf"), fig3, width = 8, height = 8)

# Save as PNG (300 dpi)
ggsave(file.path(output_folder, "sample1.png"), fig1, width = 8, height = 7, dpi = 300)
ggsave(file.path(output_folder, "sample2.png"), fig2, width = 8, height = 7, dpi = 300)
ggsave(file.path(output_folder, "merged.png"), fig3, width = 8, height = 8, dpi = 300)