rm(list = ls(envir = globalenv()), envir = globalenv())

# Load required packages
lapply(c("openxlsx", "readxl", "ggplot2", "tidyr", "dplyr", "reshape2", "rstatix"), library, character.only = TRUE)

# Set working directory to script file location
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

#################### Info ####################

rowname <- c("NoStim_1", "NoStim_2", "Stim10_10", "Stim10_1", "NoStim_3", "NoStim_4", "Stim3_2", "Stim3_3",
             "Stim3_4", "Stim3_5", "Stim10_2", "NoStim_5", "NoStim_6", "Stim3_6", "Stim3_7", "Stim3_8",
             "Stim10_3", "Stim10_4", "Stim10_5", "Stim10_6", "NoStim_7", "Stim10_7", "Stim10_8", "NoStim_8",
             "NoStim_9", "Stim3_9", "Stim10_9", "NoStim_10", "Stim3_10", "Stim3_1")

NoStim <- paste0("NoStim_", 1:10)
Stim3  <- paste0("Stim3_", 1:10)
Stim10 <- paste0("Stim10_", 1:10)
new_order <- c(NoStim, Stim3, Stim10)

sem <- function(x) sd(x) / sqrt(length(x))

#################### FC Cued Distance ####################

Data <- read_excel("FC_Results.xlsx", sheet = "5.Cued_Dist_2")
rownames(Data) <- rowname
Data <- Data[new_order, ]
Data <- Data[-c(18, 20), -1]  # remove dead mouse

Data_mean <- cbind(
  apply(Data[1:10, ], 2, mean),
  apply(Data[11:18, ], 2, mean),
  apply(Data[19:28, ], 2, mean)
)

Data_sem <- cbind(
  apply(Data[1:10, ], 2, sem),
  apply(Data[11:18, ], 2, sem),
  apply(Data[19:28, ], 2, sem)
)

df <- data.frame(
  Time = rep(1:6, 3),
  StimType = rep(c("NoStim", "Stimx3+2wks", "Stimx10+2wks"), each = 6),
  Mean = melt(Data_mean)[, 3],
  SEM = melt(Data_sem)[, 3]
)

g <- ggplot(df, aes(x = Time, y = Mean, color = StimType, group = StimType)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.5, linewidth = 1) +
  theme_classic(base_size = 30) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30, colour = "black"),
    axis.ticks = element_line(linewidth = 1.5),
    legend.position = "none"
  ) +
  scale_colour_manual(values = c("black", "red", "blue")) +
  scale_fill_manual(values = c("black", "red", "blue")) +
  scale_x_continuous(breaks = 1:6, labels = as.character(1:6)) +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 100)) +
  xlab("Time (min)") + ylab("Distance traveled (cm)")

ggsave("FC_Cued_Distance.png", g, width = 6, height = 6, dpi = 300)

# repeated 2-way ANOVA
Data_transposed <- as.data.frame(t(Data))
colnames(Data_transposed) <- c(NoStim, Stim3[1:8], Stim10)
Data_transposed$timebin <- 1:6

data_long <- pivot_longer(Data_transposed, cols = -timebin, names_to = "MouseID", values_to = "Distance")
data_long$StimType <- rep(c(rep("NoStim", 10), rep("Stimx3+2wks", 8), rep("Stimx10+2wks", 10)), times = 6)

res.aov <- anova_test(data = data_long, dv = Distance, wid = MouseID, between = StimType, within = timebin)
anova <- get_anova_table(res.aov)

pwc1 <- data_long %>% group_by(timebin) %>% pairwise_t_test(Distance ~ StimType, paired = FALSE, p.adjust.method = "bonferroni")
pwc2 <- data_long %>% pairwise_t_test(Distance ~ StimType, paired = FALSE, p.adjust.method = "bonferroni")

write.xlsx(list(ANOVA_Result = anova, Bonf_Result1 = pwc1, Bonf_Result2 = pwc2), file = "FC_Cued_Distance_Stats.xlsx")

#################### FC Cued Freezing ####################

Data <- read_excel("FC_Results.xlsx", sheet = "5.Cued_Freezing_2")
rownames(Data) <- rowname
Data <- Data[new_order, ]
Data <- Data[-c(18, 20), -1] #remove dead mouse

Data_mean <- cbind(
  apply(Data[1:10, ], 2, mean),
  apply(Data[11:18, ], 2, mean),
  apply(Data[19:28, ], 2, mean)
)

Data_sem <- cbind(
  apply(Data[1:10, ], 2, sem),
  apply(Data[11:18, ], 2, sem),
  apply(Data[19:28, ], 2, sem)
)

df <- data.frame(
  Time = rep(1:6, 3),
  StimType = rep(c("NoStim", "Stimx3+2wks", "Stimx10+2wks"), each = 6),
  Mean = melt(Data_mean)[, 3],
  SEM = melt(Data_sem)[, 3]
)

g <- ggplot(df, aes(x = Time, y = Mean, color = StimType, group = StimType)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.5, linewidth = 1) +
  theme_classic(base_size = 30) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30, colour = "black"),
    axis.ticks = element_line(linewidth = 1.5),
    legend.position = "none"
  ) +
  scale_colour_manual(values = c("black", "red", "blue")) +
  scale_fill_manual(values = c("black", "red", "blue")) +
  scale_x_continuous(breaks = 1:6, labels = as.character(1:6)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  xlab("Time (min)") + ylab("Freezing (%)")

ggsave("FC_Context_Freezing.png", g, width = 6, height = 6, dpi = 300)

# repeated 2-way ANOVA
Data_transposed <- as.data.frame(t(Data))
colnames(Data_transposed) <- c(NoStim, Stim3[1:8], Stim10)
Data_transposed$timebin <- 1:6

data_long <- pivot_longer(Data_transposed, cols = -timebin, names_to = "MouseID", values_to = "Freezing")
data_long$StimType <- rep(c(rep("NoStim", 10), rep("Stimx3+2wks", 8), rep("Stimx10+2wks", 10)), times = 6)

res.aov <- anova_test(data = data_long, dv = Freezing, wid = MouseID, between = StimType, within = timebin)
anova <- get_anova_table(res.aov)

pwc1 <- data_long %>% group_by(timebin) %>% pairwise_t_test(Freezing ~ StimType, paired = FALSE, p.adjust.method = "bonferroni")
pwc2 <- data_long %>% pairwise_t_test(Freezing ~ StimType, paired = FALSE, p.adjust.method = "bonferroni")

write.xlsx(list(ANOVA_Result = anova, Bonf_Result1 = pwc1, Bonf_Result2 = pwc2), file = "FC_Cued_Freezing_Stats.xlsx")

#################### End of Program ####################


