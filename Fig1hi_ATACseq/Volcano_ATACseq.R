rm(list = ls(envir = globalenv()), envir = globalenv())

# Set working directory to this script's location (RStudio only)
if (requireNamespace("rstudioapi", quietly = TRUE))
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Install required packages
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
for (pkg in c("EnhancedVolcano", "DESeq2")) if (!requireNamespace(pkg, TRUE)) BiocManager::install(pkg)
for (pkg in c("ggplot2", "tidyverse", "dplyr")) if (!require(pkg, character.only = TRUE)) install.packages(pkg)

# Load libraries
library(ggplot2); library(tidyverse); library(dplyr); library(EnhancedVolcano); library(DESeq2)

# Read input data
info <- read.delim("Mouse_Info.txt", header = TRUE, stringsAsFactors = FALSE, colClasses = rep("character", 3))
colnames(info) <- c("MouseName", "StimType", "OF.15min")
count <- read.csv("Peak.csv", header = TRUE)
gene <- count[, 1]
count <- count[, 6:17]
colnames(count) <- info$MouseName
row.names(count) <- gene

# Select condition ### remove # to change data 
typ <- "Stimx3.2wks"
typ1 <- info[5:8,]
typ <- "Stimx10.2wks"  
typ1 <- info[9:12, ]

data <- rbind(info[1:4, ], typ1)
count_filtered <- count[, colnames(count) %in% data$MouseName]

# DE analysis
dds <- DESeqDataSetFromMatrix(countData = count_filtered, colData = data, design = ~StimType)
dds2 <- DESeq(dds)
res <- results(dds2)
resLFC <- lfcShrink(dds = dds2, res = res, type = "normal", coef = 2)

# Volcano plot
png(paste0("Volcano_", typ, ".png"), width = 1800, height = 2200, res = 250)
print(EnhancedVolcano(toptable = res,
                      x = "log2FoldChange", y = "padj",
                      lab = rownames(res), xlim = c(-4, 4), ylim = c(0, 30),
                      pCutoff = 0.05, FCcutoff = log2(100),
                      pointSize = 0.5, selectLab = c(""),
                      title = "EnhancedVolcano\n(FC cutoff=0, padj cutoff=0.05)"))
dev.off()

# Export all differential genes
res %>%
  as.data.frame() %>%
  rownames_to_column("peak_No") %>%
  filter(padj < 1) %>%
  arrange(desc(log2FoldChange), desc(padj)) %>%
  write.csv(paste0(typ, "_AllPeakList.csv"), row.names = FALSE)

# End of program
