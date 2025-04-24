rm(list = ls(envir = globalenv()), envir = globalenv())

# Load required packages
lapply(c("readxl", "ggplot2", "reshape2", "openxlsx"), library, character.only = TRUE)

# Set working directory to script file location
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

########################### Load and plot ###########################

data <- read_excel("Opt_CSIpref.xlsx", sheet = "PrefIndex") 
data$Group <- factor(data$Group, levels = c("No Stim", "Stimx3", "Stimx10"))
data1 <- melt(data, id.vars = "Group", variable.name = "variable", value.name = "PrefIndex")

g <- ggplot(data1, aes(y = PrefIndex, x = Group, colour = Group, fill = Group)) + 
  geom_boxplot(size = 1, width = 0.8, alpha = 0.5) + 
  geom_point(aes(x = as.numeric(Group), fill = Group), shape = 21, size = 4, 
             color = "black", show.legend = FALSE, alpha = 1, stroke = 0.75) +
  theme_set(theme_classic(base_size = 24)) + 
  theme(legend.text = element_text(size = 20), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(colour = "black")) +
  coord_cartesian(xlim = c(0.4, 3.6), expand = FALSE) +
  guides(fill = guide_legend(override.aes = list(color = "transparent")),
         color = "none", shape = "none") +
  scale_fill_manual(values = c("gray30", "blue", "red"), name = "") + 
  scale_color_manual(values = c("gray30", "blue", "red"), name = "") +
  scale_y_continuous(limits = c(0.1, 1.1), breaks = seq(0.1, 1.1, 0.2)) + 
  ylab("Preference Index")

ggsave("Opt_CSI_PrefIndex.png", g, width = 6, height = 5, dpi = 300)
plot(g)

########################### ANOVA + Tukey + Save ###########################

anova_result <- aov(PrefIndex ~ Group, data = data)
anova_table <- summary(anova_result)[[1]]

tukey_result <- TukeyHSD(anova_result)
tukey_df <- data.frame(tukey_result$Group)
tukey_df <- cbind(Comparison = rownames(tukey_df), tukey_df)

write.xlsx(list(
  ANOVA = anova_table,
  TukeyHSD = tukey_df
), file = "Opt_CSIpref_Stats.xlsx", rowNames = FALSE)
