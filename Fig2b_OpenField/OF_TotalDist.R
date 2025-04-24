rm(list = ls(envir = globalenv()), envir = globalenv())

# Set working directory to script location (RStudio only)
if (requireNamespace("rstudioapi", quietly = TRUE))
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load/install required packages
for (pkg in c("ggplot2", "openxlsx", "tidyverse", "reshape2", "rstatix"))
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)

# Read and reshape data for plot
data <- read.csv("OF_TotalDist.csv")
x1 <- data[, 22:24]; x2 <- data[, 28:30]
df <- data.frame(
  Days = rep(1:24, 3),
  StimType = rep(c("NoStim", "Stimx3+2wks", "Stimx10+2wks"), each = 24),
  Mean = reshape2::melt(x1)[, 2],
  SEM  = reshape2::melt(x2)[, 2]
)

# Plot and save
g <- ggplot(df, aes(x = Days, y = Mean, color = StimType)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = Mean - SEM, ymax = Mean + SEM, fill = StimType, colour = StimType),
              alpha = 0.3, linetype = "blank") +
  theme_bw(base_size = 30) +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 30, colour = "black")) +
  scale_colour_manual(values = c("black", "red", "blue")) +
  scale_fill_manual(values = c("black", "red", "blue")) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, 24.2)) +
  scale_y_continuous(expand = c(0, 0), limits = c(1800, 8200))

ggsave("OF_TotalDist.png", g, width = 9, height = 6.5, dpi = 300)

# Prepare data for ANOVA
data <- data[, 4:21]
data[] <- lapply(data, as.numeric)
rownames(data) <- paste0("Day", 1:24)
data$rowname <- rownames(data)
data <- cbind(reshape2::melt(data),
              rep(c("NoStim", "Stimx3", "Stimx10"), each = 24 * 6))
colnames(data) <- c("Day", "mouseID", "Value", "StimType")

# Repeated 2-way ANOVA
res.aov <- anova_test(data = data, dv = Value, wid = mouseID,
                      between = StimType, within = Day)
anova_tbl <- get_anova_table(res.aov)

# Post-hoc tests
pwc <- data %>%
  group_by(Day) %>%
  pairwise_t_test(Value ~ StimType, paired = FALSE, p.adjust.method = "bonferroni")

# Save both results to Excel
openxlsx::write.xlsx(list(ANOVA = anova_tbl, PostHoc = pwc),
                     file = "OF_stat.xlsx", rowNames = FALSE)

# End of program
