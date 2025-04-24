rm(list = ls())

library(openxlsx)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(rstatix)

if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

#################### Linegraph (days) ####################
data <- read_excel("Integrated_Data.xlsx")
data <- data[5290:nrow(data), 2:29] # 2024/03/18 7:00-

c <- ncol(data)
r <- floor(nrow(data) / 60)
Data <- matrix(NA, nrow = r, ncol = c)
for (i in 1:r) Data[i, ] <- colSums(data[(i * 60 - 59):(i * 60), ])
Data <- as.data.frame(Data)

c <- ncol(Data)
r <- floor(nrow(Data) / 24)
Data1 <- matrix(NA, nrow = r, ncol = c)
for (i in 1:r) Data1[i, ] <- colSums(Data[(i * 24 - 23):(i * 24), ])
Data1 <- as.data.frame(Data1)

colnames(Data1) <- colnames(data)
labels <- c(sprintf("1.Habit%02d", 1:3), sprintf("2.Stim%02d", 1:10), sprintf("3.Day%02d", 1:(r - 13)))
rownames(Data1) <- labels

Data1 <- Data1[-c(1, 2, 3), ]
Data1[c(28, 29, 37, 47, 52), ] <- NA
r <- r - 3

sem <- function(x) sd(x) / sqrt(length(x))
Data_sem <- cbind(apply(Data1[, 1:10], 1, sem), apply(Data1[, 11:18], 1, sem), apply(Data1[, 19:28], 1, sem))
Data_mean <- cbind(apply(Data1[, 1:10], 1, mean), apply(Data1[, 11:18], 1, mean), apply(Data1[, 19:28], 1, mean))

df <- data.frame(
  Days = 1:r,
  StimType = rep(c("NoStim", "Stimx3+2wks", "Stimx10+2wks"), each = r),
  Mean = melt(Data_mean)[, 3],
  SEM = melt(Data_sem)[, 3]
)

g <- ggplot(df, aes(x = Days, y = Mean, color = StimType, group = StimType)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = Mean - SEM, ymax = Mean + SEM, fill = StimType, colour = StimType),
              linetype = "blank", alpha = 0.3) +
  theme_bw(base_size = 30) +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.text = element_text(colour = "black")) +
  scale_colour_manual(values = c("black", "red", "blue")) +
  scale_fill_manual(values = c("black", "red", "blue")) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, 51)) +
  scale_y_continuous(expand = c(0, 0), limits = c(10000, NA))

plot(g)
ggsave("HomeCageAct_Days.png", g, width = 8, height = 5, dpi = 300)

#################### repeated 2-way ANOVA ####################
data_df <- Data1[1:51, ]
data_df$days <- 1:51
data_long <- pivot_longer(data_df, cols = -days, names_to = "subject", values_to = "Distance")
data_long$StimType <- rep(c(rep("NoStim", 10), rep("Stimx3+2wks", 8), rep("Stimx10+2wks", 10)), 51)

res.aov <- anova_test(data = data_long, dv = Distance, wid = subject, between = StimType, within = days)
anova <- get_anova_table(res.aov)

pwc1 <- data_long %>%
  group_by(days) %>%
  pairwise_t_test(Distance ~ StimType, paired = FALSE, p.adjust.method = "bonferroni")

pwc2 <- data_long %>%
  pairwise_t_test(Distance ~ StimType, paired = FALSE, p.adjust.method = "bonferroni")

write.xlsx(x = list(ANOVA_Result = anova, Bonf_Result1 = pwc1, Bonf_Result2 = pwc2), file = "HC_Stats.xlsx")

#################### End of Program ####################

 