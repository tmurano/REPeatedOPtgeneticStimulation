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

# Load sample info and count matrix
info <- read.delim("Mouse_Info.txt", header = TRUE, stringsAsFactors = FALSE, colClasses = rep("character", 4))
colnames(info) <- c("MouseName", "StimType", "OF.15min", "Opt.Stim")

count <- read.delim("FPKM_count.txt", header = TRUE, stringsAsFactors = FALSE)
gene <- count[, 1]
count <- count[, -1]
colnames(count) <- info$MouseName
rownames(count) <- gene

# Select condition
 typ <- "Stimx3.2wks"; typ1 <- info[7:12, ]
# typ <- "Stimx10.2wks"; typ1 <- info[13:18, ]
# typ <- "Stimx3.24hrs"; typ1 <- info[19:24, ]
# typ <- "Stimx10.24hrs"; typ1 <- info[25:30, ]

data <- rbind(info[1:6, ], typ1)
count_filtered <- count[, colnames(count) %in% data$MouseName]

# Differential expression analysis
dds <- DESeqDataSetFromMatrix(countData = count_filtered, colData = data, design = ~StimType)
dds2 <- DESeq(dds)
res <- results(dds2)
resLFC <- lfcShrink(dds = dds2, res = res, type = "normal", coef = 2)

# Volcano plot
png(paste0("Volcano_", typ, ".png"), width = 1800, height = 1600, res = 250)
print(EnhancedVolcano(toptable = resLFC,
                      x = "log2FoldChange", y = "padj",
                      lab = rownames(resLFC),
                      xlim = c(-1.1, 1.1), ylim = c(0, 25),
                      pCutoff = 0.05, FCcutoff = log2(1.2),
                      pointSize = 1.0, labSize = 10.0,
                      selectLab = c(""),
                      title = "EnhancedVolcano\n(FC cutoff=0, padj cutoff=0.05)"))
dev.off()

# Export all differential genes
res %>%
  as.data.frame() %>%
  rownames_to_column("Gene") %>%
  filter(padj < 1) %>%
  arrange(desc(log2FoldChange), desc(padj)) %>%
  write.csv(paste0(typ, "_AllGeneList.csv"), row.names = FALSE)

# End of program

