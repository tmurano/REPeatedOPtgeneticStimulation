rm(list = ls(envir = globalenv()), envir = globalenv())

# Load required packages
packages <- c("ggplot2", "openxlsx", "reshape2", "rstatix")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Set working directory to this script location
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# ======================= Load and preprocess data =======================

data <- read.xlsx("OF_Dist.xlsx", colNames = FALSE)
n_day <- nrow(data)

NoStim_Ccnb  <- data[, 1:7]
NoStim_Scram <- data[, 8:14]
Stim10_Ccnb  <- data[, 15:21]
Stim10_Scram <- data[, 22:28]

# Function to calculate mean and SEM
mean_sem <- function(mat) {
  list(
    mean = rowMeans(mat, na.rm = TRUE),
    sem  = apply(mat, 1, function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))
  )
}

m_NCcnb  <- mean_sem(NoStim_Ccnb)
m_TCcnb  <- mean_sem(Stim10_Ccnb)
m_NScram <- mean_sem(NoStim_Scram)
m_TScram <- mean_sem(Stim10_Scram)

# ======================= Plot: Scram =======================

df_scram <- data.frame(
  Day = rep(1:n_day, 2),
  StimType = rep(c("NoStim_Scram", "Stim10_Scram"), each = n_day),
  Mean = c(m_NScram$mean, m_TScram$mean),
  SEM  = c(m_NScram$sem,  m_TScram$sem)
)

g_scram <- ggplot(df_scram, aes(x = Day, y = Mean, color = StimType)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = Mean - SEM, ymax = Mean + SEM, fill = StimType),
              alpha = 0.4, linetype = 0) +
  geom_point(size = 3, shape = 16) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.text = element_text(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5)) +
  scale_x_continuous(limits = c(1, n_day), expand = c(0, 0)) +
  scale_y_continuous(limits = c(2500, 7500), expand = c(0, 0), breaks = seq(3000, 8000, 1000)) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("black", "red"))

ggsave("OFdist_Scram.png", g_scram, width = 6, height = 4, dpi = 300)

# ======================= Plot: Ccnb =======================

df_ccnb <- data.frame(
  Day = rep(1:n_day, 2),
  StimType = rep(c("NoStim_Ccnb", "Stim10_Ccnb"), each = n_day),
  Mean = c(m_NCcnb$mean, m_TCcnb$mean),
  SEM  = c(m_NCcnb$sem,  m_TCcnb$sem)
)

g_ccnb <- ggplot(df_ccnb, aes(x = Day, y = Mean, color = StimType)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = Mean - SEM, ymax = Mean + SEM, fill = StimType),
              alpha = 0.4, linetype = 0) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.text = element_text(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5)) +
  scale_x_continuous(limits = c(1, n_day), expand = c(0, 0)) +
  scale_y_continuous(limits = c(2500, 7500), expand = c(0, 0), breaks = seq(3000, 8000, 1000)) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("black", "red"))

ggsave("OFdist_Ccnb.png", g_ccnb, width = 6, height = 4, dpi = 300)

# ======================= 2-way RM-ANOVA =======================

colnames(data) <- paste0("m", 1:28)
group_labels <- rep(c("NoStim_Ccnb", "NoStim_Scram", "Stim10_Ccnb", "Stim10_Scram"), each = 7)

data_long <- melt(as.matrix(data), varnames = c("Day", "mouseID"), value.name = "Value")
data_long$Day <- factor(paste0("Day", data_long$Day), levels = paste0("Day", 1:n_day))
data_long$StimType <- rep(group_labels, each = n_day)
data_long$mouseID <- factor(paste(data_long$StimType, data_long$mouseID, sep = "_"))

res.aov <- anova_test(
  data = data_long,
  dv = Value, wid = mouseID,
  between = StimType, within = Day
)
anova_table <- get_anova_table(res.aov)

# ======================= Post-hoc Test =======================

pwc <- data_long %>%
  group_by(Day) %>%
  pairwise_t_test(Value ~ StimType, paired = FALSE, p.adjust.method = "bonferroni")

# ======================= Save statistics =======================

write.xlsx(list(
  ANOVA = anova_table,
  PostHoc = pwc
), file = "OFdist_stats.xlsx")

# ======================= End of Program =======================
