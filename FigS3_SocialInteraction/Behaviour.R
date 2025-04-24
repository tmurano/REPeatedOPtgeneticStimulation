rm(list = ls(envir = globalenv()), envir = globalenv())

# Load required packages
lapply(c("openxlsx", "ggplot2", "reshape2"), library, character.only = TRUE)

# Set working directory to script file location
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

########################### Opt_CSI1 ###########################

data <- read.xlsx("Opt_CSI1.xlsx")
data$Group <- factor(data$Group, levels = c("No Stim", "Stimx3", "Stimx10"))
data1 <- melt(data, id.vars = "Group", variable.name = "Chamber", value.name = "StayTime")

g <- ggplot(data1, aes(y = StayTime, x = Group, colour = Chamber, fill = Chamber)) +
  stat_summary(fun = "mean", geom = "bar", width = 0.8, alpha = 0.5, colour = "transparent", linewidth = 0.5, position = position_dodge(0.9)) +
  stat_summary(aes(group = Chamber), fun.data = mean_se, geom = "errorbar", linewidth = 1, width = 0.3, position = position_dodge(0.9)) +
  geom_point(aes(x = as.numeric(Group) + 0.05, fill = Chamber), shape = 21, size = 4, color = "black", show.legend = FALSE, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), alpha = 1, stroke = 0.75) +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, 100)) +
  ylab("Time spent in chamber (sec)") +
  scale_fill_manual(values = c("gray30", "gray80", "tomato"), name = "") +
  scale_color_manual(values = c("gray30", "gray80", "tomato"), name = "") +
  coord_cartesian(xlim = c(0.4, 3.6), expand = FALSE) +
  guides(fill = guide_legend(override.aes = list(color = "transparent")), color = "none", shape = "none") +
  theme_set(theme_classic(base_size = 24)) +
  theme(
    axis.text = element_text(colour = "black"),
    text = element_text(size = 24),
    legend.text = element_text(size = 24),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

plot(g)
ggsave('Opt_CSI1.png', g, width = 9, height = 5, dpi = 300)

########################### Opt_CSI2 ###########################

rm(list = ls(envir = globalenv()), envir = globalenv())
library(openxlsx); library(ggplot2); library(reshape2)

data <- read.xlsx("Opt_CSI2.xlsx")
data$Group <- factor(data$Group, levels = c("No Stim", "Stimx3", "Stimx10"))
data1 <- melt(data, id.vars = "Group", variable.name = "Chamber", value.name = "StayTime")

g <- ggplot(data1, aes(y = StayTime, x = Group, colour = Chamber, fill = Chamber)) +
  stat_summary(fun = "mean", geom = "bar", width = 0.8, alpha = 0.5, colour = "transparent", linewidth = 0.5, position = position_dodge(0.9)) +
  stat_summary(aes(group = Chamber), fun.data = mean_se, geom = "errorbar", linewidth = 1, width = 0.3, position = position_dodge(0.9)) +
  geom_point(aes(x = as.numeric(Group) + 0.05, fill = Chamber), shape = 21, size = 4, color = "black", show.legend = FALSE, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), alpha = 1, stroke = 0.75) +
  scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, 100)) +
  ylab("Time spent around the cage (sec)") +
  scale_fill_manual(values = c("tomato", "gray30"), name = "") +
  scale_color_manual(values = c("tomato", "gray30"), name = "") +
  coord_cartesian(xlim = c(0.4, 3.6), expand = FALSE) +
  guides(fill = guide_legend(override.aes = list(color = "transparent")), color = "none", shape = "none") +
  theme_set(theme_classic(base_size = 24, base_family = "Helvetica")) +
  theme(
    axis.text = element_text(colour = "black"),
    text = element_text(size = 24),
    legend.text = element_text(size = 24),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

plot(g)
ggsave('Opt_CSI2.png', g, width = 8, height = 5, dpi = 300)

########################### CSI1 (ANOVA + Tukey) ###########################

data1 <- read.xlsx("Opt_CSI1.xlsx")
NoStim <- melt(data1[1:7, 2:4], id.vars = NULL)
Stim3  <- melt(data1[8:14, 2:4], id.vars = NULL)
Stim10 <- melt(data1[15:21, 2:4], id.vars = NULL)

posthoc_N   <- TukeyHSD(aov(value ~ variable, data = NoStim))$variable
posthoc_S3  <- TukeyHSD(aov(value ~ variable, data = Stim3))$variable
posthoc_S10 <- TukeyHSD(aov(value ~ variable, data = Stim10))$variable

########################### CSI2 (t-test) ###########################

data2 <- read.xlsx("Opt_CSI2.xlsx")
NoStim <- melt(data2[1:7, 2:3], id.vars = NULL)
Stim3  <- melt(data2[8:14, 2:3], id.vars = NULL)
Stim10 <- melt(data2[15:21, 2:3], id.vars = NULL)

ttest_N   <- t.test(NoStim[1:7, 2], NoStim[8:14, 2], var.equal = TRUE)
ttest_S3  <- t.test(Stim3[1:7, 2], Stim3[8:14, 2], var.equal = TRUE)
ttest_S10 <- t.test(Stim10[1:7, 2], Stim10[8:14, 2], var.equal = TRUE)

########################### Save results ###########################

write.xlsx(
  list(
    CSI1_Tukey_NoStim  = data.frame(name = rownames(posthoc_N), posthoc_N),
    CSI1_Tukey_Stim3   = data.frame(name = rownames(posthoc_S3), posthoc_S3),
    CSI1_Tukey_Stim10  = data.frame(name = rownames(posthoc_S10), posthoc_S10),
    CSI2_ttest = data.frame(
      Group   = c("NoStim", "Stim3", "Stim10"),
      t       = c(ttest_N$statistic, ttest_S3$statistic, ttest_S10$statistic),
      df      = c(ttest_N$parameter, ttest_S3$parameter, ttest_S10$parameter),
      p.value = c(ttest_N$p.value, ttest_S3$p.value, ttest_S10$p.value)
    )
  ),
  file = "Opt_CSI_Stats.xlsx", rowNames = FALSE
)
