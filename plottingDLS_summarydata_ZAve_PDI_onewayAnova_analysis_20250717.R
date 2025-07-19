# ZX
# 2025-07-17
# Martinsried
# Change filename and sample names

library(readxl)
library(ggplot2)
library(ggsignif)
library(dplyr)

# Load data
file_path <- "filename.xlsx"
raw_data <- read_excel(file_path, skip = 1)

# Extract relevant data
data <- data.frame(
  Sample = raw_data[[3]],  
  Z_Ave = as.numeric(raw_data[[6]]),  
  PDI = as.numeric(raw_data[[7]])    
)

data <- na.omit(data)
data$Group <- factor(c(rep("sample1", 3), rep("sample2", 3)))

# Function for summary stats
summary_stats <- function(df, measurevar, groupvar){
  df %>%
    group_by({{groupvar}}) %>%
    summarise(
      mean = mean({{measurevar}}),
      sd = sd({{measurevar}}),
      n = n(),
      se = sd / sqrt(n)
    )
}

z_stats <- summary_stats(data, Z_Ave, Group)
pdi_stats <- summary_stats(data, PDI, Group)

# ANOVA tests
anova_z <- aov(Z_Ave ~ Group, data = data)
p_z <- summary(anova_z)[[1]]["Group", "Pr(>F)"]
p_z_label <- ifelse(p_z < 0.001, "p < 0.001", paste0("p = ", signif(p_z, 3)))
star_z <- ifelse(p_z < 0.05, "*", "ns")

anova_pdi <- aov(PDI ~ Group, data = data)
p_pdi <- summary(anova_pdi)[[1]]["Group", "Pr(>F)"]
p_pdi_label <- ifelse(p_pdi < 0.001, "p < 0.001", paste0("p = ", signif(p_pdi, 3)))
star_pdi <- ifelse(p_pdi < 0.05, "*", "ns")

### Plot for Z-Ave ###
y_line_z <- max(z_stats$mean + z_stats$se) * 1.1
y_max_z <- y_line_z * 1.2

plot_z <- ggplot(z_stats, aes(x = Group, y = mean, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", size = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  geom_jitter(data = data, aes(x = Group, y = Z_Ave), width = 0.1, size = 3, color = "black") +
  geom_text(aes(label = round(mean, 1), y = mean - se - 10), size = 5, color = "black") +
  geom_signif(
    comparisons = list(c("sample1", "sample2")),
    annotations = star_z,
    y_position = y_line_z,
    tip_length = 0.02,
    textsize = 6
  ) +
  geom_text(
    aes(x = 1.5, y = y_line_z - (0.05 * y_line_z), label = p_z_label),
    color = "black",
    size = 5,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(title = "Z-Ave (Size)", y = "Z-Ave (nm)", x = "") +
  ylim(0, y_max_z)


### Plot for PDI ###
y_line_pdi <- max(pdi_stats$mean + pdi_stats$se) * 1.1
y_max_pdi <- y_line_pdi * 1.2

plot_pdi <- ggplot(pdi_stats, aes(x = Group, y = mean, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", size = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  geom_jitter(data = data, aes(x = Group, y = PDI), width = 0.1, size = 3, color = "black") +
  geom_text(aes(label = round(mean, 3), y = mean - se - 0.04), size = 5, color = "black") +
  geom_signif(
    comparisons = list(c("sample1", "sample2")),
    annotations = star_pdi,
    y_position = y_line_pdi,
    tip_length = 0.02,
    textsize = 6
  ) +
  geom_text(
    aes(x = 1.5, y = y_line_pdi - (0.05 * y_line_pdi), label = p_pdi_label),
    color = "black",
    size = 5,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(title = "PDI", y = "PDI", x = "") +
  ylim(0, y_max_pdi)


### Save plots ###
output_dir <- dirname(file_path)

ggsave(
  filename = file.path(output_dir, "Z-Ave (Size).png"),
  plot = plot_z,
  width = 6,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "PDI.png"),
  plot = plot_pdi,
  width = 6,
  height = 5,
  dpi = 300
)

# Display plots
print(plot_z)
print(plot_pdi)
