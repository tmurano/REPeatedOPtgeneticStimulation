rm(list = ls())

# Load required packages
lapply(c("readxl", "openxlsx", "ggplot2", "tidyr", "dplyr", "reshape2", "rstatix", "tibble"),
       library, character.only = TRUE)

# Set working directory to source file location
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

#################### Line Plot for Time Bins ####################

Data <- read_excel("TS_Results.xlsx", sheet = "TS")
Data <- Data[, -c(1, 19)]  # Remove dead mouse

sem <- function(x) sd(x) / sqrt(length(x))
Data_mean <- cbind(apply(Data[, 1:10], 1, mean),
                   apply(Data[, 11:19], 1, mean),
                   apply(Data[, 20:29], 1, mean))
Data_sem <- cbind(apply(Data[, 1:10], 1, sem),
                  apply(Data[, 11:19], 1, sem),
                  apply(Data[, 20:29], 1, sem))

df <- data.frame(
  Time = 1:10,
  StimType = rep(c("NoStim", "Stimx3+2wks", "Stimx10+2wks"), each = 10),
  Mean = melt(Data_mean)[, 3],
  SEM = melt(Data_sem)[, 3]
)

g <- ggplot(df, aes(x = Time, y = Mean, color = StimType, group = StimType)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.5, linewidth = 1) +
  theme_classic(base_size = 30) +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 25, colour = "black"),
        axis.ticks = element_line(linewidth = 1.5),
        legend.position = "none") +
  scale_colour_manual(values = c("black", "red", "blue")) +
  scale_fill_manual(values = c("black", "red", "blue")) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 20), limits = c(0, 100)) +
  xlab("Blocks of 1 min") + ylab("Immobility (%)")

ggsave("TS.png", g, width = 6, height = 6, dpi = 300)

#################### Repeated Measures ANOVA ####################

Data$timebin <- 1:10
data_long <- pivot_longer(Data, cols = -timebin, names_to = "MouseID", values_to = "Immobility")
data_long$StimType <- rep(c(rep("NoStim", 10), rep("Stimx3+2wks", 9), rep("Stimx10+2wks", 10)), times = 10)

res.aov <- anova_test(data = data_long, dv = Immobility, wid = MouseID, between = StimType, within = timebin)
anova <- get_anova_table(res.aov)

pwc1 <- data_long %>%
  group_by(timebin) %>%
  pairwise_t_test(Immobility ~ StimType, paired = FALSE, p.adjust.method = "bonferroni")

pwc2 <- data_long %>%
  pairwise_t_test(Immobility ~ StimType, paired = FALSE, p.adjust.method = "bonferroni")

write.xlsx(list(ANOVA_Result = anova, Bonf_Result1 = pwc1, Bonf_Result2 = pwc2), "TS_Stats.xlsx")

#################### Group Average Boxplot ####################

Data <- read_excel("TS_Results.xlsx", sheet = "TS")
Data <- Data[, -c(1, 19)]  # Remove dead mouse
Data <- data.frame(Immobility = colMeans(Data),
                   Group = c(rep("NoStim", 10), rep("Stimx3+2wks", 9), rep("Stimx10+2wks", 10)))
Data$Group <- factor(Data$Group, levels = c("NoStim", "Stimx3+2wks", "Stimx10+2wks"))
Data1 <- melt(Data, id.vars = "Group", value.name = "Immobility")

g <- ggplot(Data1, aes(y = Immobility, x = Group, colour = Group, fill = Group)) +
  geom_boxplot(size = 1, width = 0.8, alpha = 0.5) +
  geom_point(shape = 21, size = 4, color = "black", alpha = 1, stroke = 0.75, show.legend = FALSE) +
  theme_classic(base_size = 24) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = "black")) +
  coord_cartesian(xlim = c(0.4, 3.6), expand = FALSE) +
  guides(fill = guide_legend(override.aes = list(color = "transparent")),
         color = "none", shape = "none") +
  scale_fill_manual(values = c("gray30", "blue", "red")) +
  scale_color_manual(values = c("gray30", "blue", "red")) +
  scale_y_continuous(limits = c(30, 80), breaks = seq(30, 80, 10)) +
  ylab("Immobility (average, %)")

ggsave("TS_ave.png", g, width = 3.5, height = 5, dpi = 300)

#################### One-Way ANOVA and Tukey Post-hoc ####################

anova_result <- aov(Immobility ~ Group, data = Data1)
tukey_result <- TukeyHSD(anova_result)

a <- summary(anova_result)[[1]] %>% rownames_to_column("name")
t <- data.frame(tukey_result[[1]]) %>% rownames_to_column("name")

write.xlsx(list(ANOVA_Result = a, Tukey_Result = t), "TS_ave_Stats.xlsx")

#################### End of Program ####################

